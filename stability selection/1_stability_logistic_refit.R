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
library(stringr)

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

data = data %>% select(-which(sapply(names(data), grepl, pattern = "NMR"))) %>%select (-c("date_recr", "date_diagnosis", "date_death",
                                                                                          
                                                                                          "time_to_diagnosis", "prevalent_case", "incident_case", "Date_lost_to_followup"))
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

# # putting case and eid again
#data$eid = x_eid
#data$case = x_case

# data is ready 


##############split into train and test########

x_selection <- data[train,] # training stability on 50% of the data
x_test <- data[test,] # regression and prediction on the other 50% of the data

#y <- data$case # outcome

y_selection = y[train] # outcome for the stability selection
y_test = y[test] # outocome for the regression and the prediction



############################ this is the split for the refit and the test
refit <- sample(1:nrow(x_test), 0.6*nrow(x_test)) # participants for the regression - the refit
prediction <- seq(1, nrow(x_test))[-refit] # participants for the prediction

logistic_refit <- x_test[refit, ] # for logistic regression this is the training set - the refit
logistic_predict<- x_test[prediction,] # for logistic regression this is the testing set - prediction

y_refitting <- y_test[refit] # this is the outcome for the training - the refit
y_predicting <- y_test[prediction] # this is the outcome for the testing - prediction


################### performing stability selection
# Working on stability lasso on bio + env

t0 <- Sys.time()
stability_lasso_logistic <- VariableSelection(xdata = x_selection, ydata = y_selection, verbose = FALSE,penalty.factor = c(rep(0, 8),rep(1, 127)) , family = "binomial", seed = 1)
t1 <- Sys.time()
print(t1 - t0)


saveRDS(stability_lasso_logistic, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/logistic_output/stability_lasso_logistic.rds")


#these are the variables selected
# uncalibrated model
select_new = SelectedVariables(stability_lasso_logistic)
keep_new = names(SelectedVariables(stability_lasso_logistic))[which(select_new ==1)]


#############to compare for sensitivity analysis
stability_lasso_cox_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/cox_output/stability_lasso_cox_new.rds")
select_cox = SelectedVariables(stability_lasso_cox_new)
keep_cox = names(SelectedVariables(stability_lasso_cox_new))[which(select_cox ==1)]
summary(stability_lasso_logistic)
hat_params[2]

selprop <- SelectionProportions(stability_lasso_logistic)
hat_params<- Argmax(stability_lasso_logistic)
selprop_cox <- SelectionProportions(stability_lasso_cox_new )
hat_params_cox<- Argmax(stability_lasso_cox_new )
# Visualisation of selection proportions
# Visualisation of selection proportions
par(mar = c(10, 5, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", col = ifelse(selprop>=
                                                               hat_params[2], yes = "blue", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = names(selprop)[i],
       las = 2, col = ifelse(names(selprop)[i] %in% keep_cox,
                             yes = "red", no = "grey"), col.axis = ifelse(names(selprop)[i] %in% keep_cox, yes = "red", no = "grey"))
}
length(keep_new)

par(mar = c(10, 5, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", col = ifelse(selprop >=
                                                               hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = names(selprop)[i],
       las = 2, col = ifelse(selprop[i] >= hat_params[2],
                             yes = "red", no = "grey"), col.axis = ifelse(selprop[i] >=
                                                                            hat_params[2], yes = "red", no = "grey"))
}

names(selprop)



#######################Refitting the model on the second 50 split : 30 - 20####################################################


# the first part can be not run
# load the following workspace
load("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/1_logistic_selection_workspace.RData")


stability_lasso_logistic = readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/logistic_output/stability_lasso_logistic.rds")

# data is prepared for the refit

# Refitting the model
# new one - uncalibrated
refitted_logistic <- Refit(xdata = logistic_refit, ydata = y_refitting ,stability = stability_lasso_logistic, family = "binomial")



saveRDS(refitted_logistic, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/logistic_output/refitted_logistic.rds")


####################prediction of the models on the last 20% of the data#########################################################
#### OMAR part##############################################


performance = ExplanatoryPerformance(xdata = logistic_refit, ydata = y_refitting, new_xdata = logistic_predict,
                                     new_ydata = y_predicting, stability = stability_lasso_logistic, family = "binomial")


# Predict probabilities for the test set
probabilities <- predict(refitted_logistic, newdata = logistic_predict, type = "response")

# Plot the ROC curve
roc_curve <- roc(y_predicting, probabilities)
plot(roc_curve)

class(performance)





stability_lasso_logistic = readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/lasso_output/stability_lasso3.rds")

# stability_lasso is from /rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/lasso_output
# selprop <- SelectionProportions(stability_lasso2)
# #selprop 
# # Calibrated parameters
# hat_params <- Argmax(stability_lasso2)
# #hat_params
# var_selected = which(unlist(selprop >= hat_params [2]))
# var_selected

selprop <- SelectionProportions(stability_lasso3)
#selprop 
# Calibrated parameters
hat_params <- Argmax(stability_lasso3)
#hat_params
var_selected3 = which(unlist(selprop >= hat_params [2]))
var_selected3



# Refitting the model
refitted <- Refit(xdata = logistic_training, ydata = y_training ,stability = stability_lasso3)
refitted
summary(refitted)


predicted_logistic <- predict(refitted, newdata = logistic_testing, type = "response")

predicted_logistic

