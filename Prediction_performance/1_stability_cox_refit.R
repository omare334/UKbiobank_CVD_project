# this is the whole script that performs stability selection: all on cvd, performs the refit for all the stability selected models and performs the prediction


# Another script performs the stability selected model for the environment variables on the stabily selected biological variables
# please refer to the script stability_bio_PFER in /rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/calibrated_new_lasso
# the lasso object can be found in the folder cox_output


# Step 1: loading the libraries
#Loading the libraries

library(glmnet)
library(fastDummies)
library(caret)
library(dplyr)
library(ggrepel)
library(ggplot2)
suppressPackageStartupMessages(library(glmnet))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(pheatmap))
suppressPackageStartupMessages(library(fake))
suppressPackageStartupMessages(library(sharp))
library(survival)



# Step 2: load the data set and the train test split workspace - prepare the data
cleaned_df <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/cleaned_df.RDS")

# Splitting the data into training and testing
load("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/workspaces/Train_test_workspace.RData")

# cleaning the names 
data = cleaned_df

col_names <- colnames(data)
col_names <- gsub("\\(|\\)|+|-|/|,", "", col_names)
colnames(data) <- col_names

# we also change the names of clened_df, the original dataset - this is needed for when we are going to put back the dates variables
colnames(cleaned_df) = col_names

# removing the NMR variables and the dates to one-hot encode and scale the variables - removing the case variables too because we don't need them

data = data %>% select(-which(sapply(names(data), grepl, pattern = "NMR"))) %>%select (-c("date_recr", "date_diagnosis", "date_death",
                                                                                          "time_to_diagnosis", "prevalent_case", "incident_case", "Date_lost_to_followup"))
## Resetting the confoundrs

data <- data %>% rename_at(vars(contains("PRS_bio")), list(~str_replace(., "PRS_bio", "PRS_conf")))
data <- data %>% rename_at(vars(contains("Ethnic_background_bio")), list(~str_replace(., "Ethnic_background_bio", "Ethnic_background_conf")))

## reorder the columns to put the confounders first
eid = data$eid
confounders =  data %>% select(which(sapply(names(data), grepl, pattern = "conf"))) 
confounders$eid = eid
data =  data %>% select(- which(sapply(names(data), grepl, pattern = "conf"))) 

data2 = merge(confounders, data, by = "eid")
data = data2
# we specify the outcome - here it is CVD
y <- data$case # outcome

x_eid = data$eid # save it to add it later
x_case = data$case


# Now we remove ethnic background - because of the conflict with PRS - lipid lowering is removed because it has only 1 value - can't one-hot encode it and wont be useful in the model
# scaling and creating dummy variables for the data
data <- data%>%
  select(-c(com_lipid_lowering))

# Select only numeric and integer columns to scale

data <- data %>%
  mutate_if(is.numeric, scale)


# one-hot encode the data
data <- model.matrix( ~ ., dplyr::select(data, -c(case, eid)))[, -1]
data = as.data.frame(data)

# putting case and eid again
data$eid = x_eid
data$case = x_case

# data is ready 
# preparing the data for cox

######################


#putting the dates back
output_f <- merge(data, data.frame(eid = cleaned_df$eid, date_recr = cleaned_df$date_recr,date_diagnosis = cleaned_df$date_diagnosis, date_death = cleaned_df$date_death,
                                   time_to_diagnosis = cleaned_df$time_to_diagnosis, Date_lost_to_follow_up = cleaned_df$Date_lost_to_followup), by = "eid", all.x = TRUE)


# create new columns for time in years from date_recr to date_diagnosis, date_death, and date_lost_to_follow-up
output_f$years_to_diagnosis <- as.numeric(difftime(output_f$date_diagnosis, output_f$date_recr, units = "days")) / 365.25
output_f$years_to_death <- as.numeric(difftime(output_f$date_death, output_f$date_recr, units = "days")) / 365.25
output_f$years_to_lost_to_follow_up <- as.numeric(difftime(output_f$Date_lost_to_follow_up, output_f$date_recr, units = "days")) / 365.25

# Create a new column called left_the_study with NA values
output_f$left_the_study <- NA

# Calculate the number of years from date_recr to 2017-05-01
output_f$left_the_study <- as.numeric(difftime(as.Date("2022-05-01"), output_f$date_recr, units = "days")) / 365.25

output_f$time_to_event <- NA


# add al the columns to create a time to event
output_f$time_to_event <- ifelse(!is.na(output_f$years_to_diagnosis), output_f$years_to_diagnosis, 
                                 ifelse(!is.na(output_f$years_to_death), output_f$years_to_death, 
                                        ifelse(!is.na(output_f$years_to_lost_to_follow_up), output_f$years_to_lost_to_follow_up, 
                                               output_f$left_the_study)))
# Remove negarive numbers
output_f <- output_f[output_f$time_to_event >= 0,]



time_ev<- output_f[, c("eid","time_to_event", "case")]



#remove columns in excess

df_final<- output_f[, !colnames(output_f) %in% 
                      c("date_recr", "date_diagnosis","date_death" )]


######################dates are ready - prepare for the model ############################################################
set.seed(1)
# preparing the train and test data
x_train <- df_final[train,] # training stability on 50% of the data
x_test <- df_final[test,] # regression and prediction on the other 50% of the data - here test stands for the refit and prediction

#Prepare surv object for cox

CVD.surv_train <- Surv(x_train$time_to_event, x_train$case) # this survival object is for stability selection not for training
# the object for the refit is created later, we still need to create 2, one for the refit and the other for the prediction


#prepare train and test set where we select only the predictors variables - no dates and case
selection_set<-x_train[, !names(x_train) %in% c("time_to_event", "case", "eid", "time_to_diagnosis", "Date_lost_to_follow_up", "years_to_diagnosis",
                                               "years_to_death","years_to_lost_to_follow_up")]



# this is for the refit and the prediction later
#refit_prediction_set<- x_test[, !names(x_test) %in% c("time_to_event", "case", "eid", "time_to_diagnosis", "Date_lost_to_follow_up", "years_to_diagnosis",
                                             #"years_to_death","years_to_lost_to_follow_up")]


# Working on stability lasso on bio + env
#
# no calibration - PRS, ETHNIC BACKGROUN, AGE AND SEX CONFOUNDERS

## need to reorder the columns for the confounders
t0 <- Sys.time()
stability_lasso_cox_new <- VariableSelection(xdata = selection_set, ydata = CVD.surv_train, verbose = FALSE, penalty.factor = c(rep(0, 8),rep(1, 128)),
                                             family = "cox", seed = 1)

t1 <- Sys.time()
print(t1 - t0)

saveRDS(stability_lasso_cox_new, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/cox_output/stability_lasso_cox_new.rds")


# these are the variables that are selected



# uncalibrated model
select_new = SelectedVariables(stability_lasso_cox_new)
keep_new = names(SelectedVariables(stability_lasso_cox_new))[which(select_new ==1)]


#######################Refitting the model on the second 50 split : 30 - 20####################################################


# the first part can be not run
# load the following workspace
load("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/1_cox_selection_workspace.RData")


stability_lasso_cox_new = readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/cox_output/stability_lasso_cox_new.rds")

# prepare the data for the refit

y_train = y[train] # outcome for the stability selection
y_test = y[test] # outcome for the regression and the prediction

refit <- sample(1:nrow(x_test), 0.6*nrow(x_test)) # participants for the regression - for the refit
prediction <- seq(1, nrow(x_test))[-refit] # participants for the prediction

refitting <- x_test[refit, ] # for cox regression this is the training set - for the refit
predicting <- x_test[prediction,] # for cox regression this is the testing set - prediction

y_refitting <- y_test[refit] # this is the outcome for the training - refit
y_predicting <- y_test[prediction] # this is the outcome for the testing - prediction


CVD.surv_refitting <- Surv(refitting$time_to_event, refitting$case) # training to refit the model


CVD.surv_predicting <- Surv(predicting$time_to_event, predicting$case) # prediction of the refit model

#prepare refit and prediction set to use without the time variables
cox_refit<-refitting[, !names(refitting) %in% c("time_to_event", "case", "eid", "time_to_diagnosis", "Date_lost_to_follow_up", "years_to_diagnosis",
                                                 "years_to_death","years_to_lost_to_follow_up")]
cox_prediction<- predicting[, !names(predicting) %in% c("time_to_event","case","eid", "time_to_diagnosis", "Date_lost_to_follow_up", "years_to_diagnosis",
                                               "years_to_death","years_to_lost_to_follow_up")]


# Refitting the model
# new one - uncalibrated
refitted_cox_new <- Refit(xdata = cox_refit, ydata = CVD.surv_refitting ,stability = stability_lasso_cox_new, family = "cox")


###################doing cox vs everything
cox_refit2 = cox_refit
cox_refit2$case = as.numeric(refitting$case)
cox_refit2$time_to_event = refitting$time_to_event
cox_refit2$eid = refitting$eid

class(cox_refit2$case)
all_cox <- coxph( Surv(time_to_event, case) ~ ., id = eid,data = cox_refit2)
all_cox <- coxph(Surv(time_to_event, case) ~ ., data = cox_refit2, id = eid)

#refitted_cox
summary(refitted_cox_new)


saveRDS(refitted_cox_new, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/cox_output/refitted_cox_new.rds")


####################prediction of the models on the last 20% of the data#########################################################
#### OMAR part##############################################
?predict
predicted_cox <- predict(refitted_cox_new, newdata = cox_test, type = "expected")
summary(predicted_cox)

performance = ExplanatoryPerformance(xdata = cox_refit, ydata = CVD.surv_refitting, new_xdata = cox_prediction,
                                     new_ydata = CVD.surv_predicting, stability = stability_lasso_cox_new, family = "cox")

 
?ExplanatoryPerformance

