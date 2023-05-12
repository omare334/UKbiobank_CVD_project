
load("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/2_stability_cox_for2ndlayer.RData")
##### stability for Confounders

#QRISK3 =  data %>% select(which(sapply(names(data), grepl, pattern = "com"))) 
y <- data$case # outcome

x_eid = data$eid # save it to add it later
x_case = data$case



eid = data$eid
case = data$case
confounders =  data %>% select(which(sapply(names(data), grepl, pattern = "conf"))) 
confounders$eid = eid
confounders$case = case



# Now we remove ethnic background - because of the conflict with PRS - lipid lowering is removed because it has only 1 value - can't one-hot encode it and wont be useful in the model
# scaling and creating dummy variables for the data

# Select only numeric and integer columns to scale

# Select only numeric and integer columns to scale

confounders <- confounders %>%
  mutate_if(is.numeric, scale)


# one-hot encode the data
confounders <- model.matrix( ~ ., dplyr::select(confounders, -c(case, eid)))[, -1]
confounders = as.data.frame(confounders)


confounders$eid = eid
confounders$case = case





#putting the dates back
output_f <- merge(confounders, data.frame(eid = cleaned_df$eid, date_recr = cleaned_df$date_recr,date_diagnosis = cleaned_df$date_diagnosis, date_death = cleaned_df$date_death,
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
confounders_train <- df_final[train,] # training stability on 50% of the data
confounders_test <- df_final[test,] # regression and prediction on the other 50% of the data - here test stands for the refit and prediction

#Prepare surv object for cox

CVD.surv_train_conf <- Surv(confounders_train$time_to_event, confounders_train$case) # this survival object is for stability selection not for training
# the object for the refit is created later, we still need to create 2, one for the refit and the other for the prediction


#prepare train and test set where we select only the predictors variables - no dates and case
selection_set_conf<-confounders_train[, !names(confounders_train) %in% c("time_to_event", "case", "eid", "time_to_diagnosis", "Date_lost_to_follow_up", "years_to_diagnosis",
                                                              "years_to_death","years_to_lost_to_follow_up")]



# this is for the refit and the prediction later
#refit_prediction_set<- x_test[, !names(x_test) %in% c("time_to_event", "case", "eid", "time_to_diagnosis", "Date_lost_to_follow_up", "years_to_diagnosis",
#"years_to_death","years_to_lost_to_follow_up")]


# Working on stability lasso on bio + env
#
# no calibration - PRS, ETHNIC BACKGROUN, AGE AND SEX CONFOUNDERS

## need to reorder the columns for the confounders
t0 <- Sys.time()
stability_lasso_cox_conf <- VariableSelection(xdata = selection_set_conf, ydata = CVD.surv_train_conf, verbose = FALSE,
                                               family = "cox", seed = 1)

t1 <- Sys.time()
print(t1 - t0)

# uncalibrated model
select_new = SelectedVariables(stability_lasso_cox_QRISK)
keep_new = names(SelectedVariables(stability_lasso_cox_QRISK))[which(select_new ==1)]


#######################Refitting the model on the second 50 split : 30 - 20####################################################




# prepare the data for the refit

y_train = y[train] # outcome for the stability selection
y_test = y[test] # outcome for the regression and the prediction

refit <- sample(1:nrow(confounders_test), 0.6*nrow(confounders_test)) # participants for the regression - for the refit
prediction <- seq(1, nrow(confounders_test))[-refit] # participants for the prediction

refitting <- confounders_test[refit, ] # for cox regression this is the training set - for the refit
predicting <- confounders_test[prediction,] # for cox regression this is the testing set - prediction

y_refitting <- y_test[refit] # this is the outcome for the training - refit
y_predicting <- y_test[prediction] # this is the outcome for the testing - prediction


CVD.surv_refitting_conf <- Surv(refitting$time_to_event, refitting$case) # training to refit the model
CVD.surv_predicting_conf <- Surv(predicting$time_to_event, predicting$case) # prediction of the refit model

#prepare refit and prediction set to use without the time variables
cox_refit_conf<-refitting[, !names(refitting) %in% c("time_to_event", "case", "eid", "time_to_diagnosis", "Date_lost_to_follow_up", "years_to_diagnosis",
                                                      "years_to_death","years_to_lost_to_follow_up")]
cox_prediction_conf<- predicting[, !names(predicting) %in% c("time_to_event","case","eid", "time_to_diagnosis", "Date_lost_to_follow_up", "years_to_diagnosis",
                                                              "years_to_death","years_to_lost_to_follow_up")]


cox_roc_conf <- ExplanatoryPerformance(xdata = cox_refit_conf, ydata = CVD.surv_refitting_conf,
                                        new_xdata = cox_prediction_conf, new_ydata = CVD.surv_predicting_conf, 
                                        stability = stability_lasso_cox_conf, family = "cox")


