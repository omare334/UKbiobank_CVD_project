# this is the script that performs stability selection on the enviorrnmental variables on the biological variables that were stabily selected for CVD

# please refer to the objects in the folder /rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/calibrated_new_lasso/cox_output
# Also load the workspace used for the previous script 
load("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/1_logistic_selection_workspace.RData")


# the refitted and prediction part is not needed for this script



# refitting these models is in the second part of the script you can load the workspace and run them 

# Loading the libraries
library(glmnet)
library(fastDummies)
library(caret)
library(dplyr)
library(ggrepel)
library(ggplot2)
suppressPackageStartupMessages(library(glmnet))
#install.packages("sharp")
#install.packages("impute")
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(pheatmap))
suppressPackageStartupMessages(library(fake))
suppressPackageStartupMessages(library(sharp))

##############stability selected objects#########################################

# from the workspace loaded, data is the entire data, without the dates, without the NMR variables, the variables are scled and one-hot encoded
# we already have x_selection and x_test


refitted_logistic <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/logistic_output/refitted_logistic.rds")
stability_lasso_logistic <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/logistic_output/stability_lasso_logistic.rds")

# uncalibrated model
select= SelectedVariables(stability_lasso_logistic)
keep= names(SelectedVariables(stability_lasso_logistic))[which(select ==1)]

# we are going to select the biological variables selected to perform stability selection on them


names <- grep("(bio|com)", keep, value = TRUE)
bio_selected<- select(x_selection, all_of(names))


# now we select the env variables that are the predictors - for the training set
env_only = x_selection %>% select(- which(sapply(names(x_selection), grepl, pattern = "bio|com")))


# Working on stability lasso of env on bio

# variables selected by both pfer and uncalibrated

################## cystatin
stability_lasso_Cystatin_C_new <- VariableSelection(xdata = env_only, ydata = bio_selected_new$bio_blood_Cystatin_C, verbose = FALSE,  family = "gaussian", seed = 1)

saveRDS(stability_lasso_Cystatin_C_new, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_Cystatin_C_new.rds")

#######################ldl
stability_lasso_LDL_direct_new <- VariableSelection(xdata = env_only, ydata = bio_selected_new$bio_blood_LDL_direct, verbose = FALSE,  family = "gaussian", seed = 1)
saveRDS(stability_lasso_LDL_direct_new, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_LDL_direct_new.rds")

############testosterone

stability_lasso_Testosterone_new <- VariableSelection(xdata = env_only, ydata = bio_selected_new$bio_blood_Testosterone, verbose = FALSE,  family = "gaussian", seed = 1)
saveRDS(stability_lasso_Testosterone_new, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_Testosterone_new.rds")


################hdl

stability_lasso_HDL_cholesterol_new <- VariableSelection(xdata = env_only, ydata = bio_selected_new$bio_blood_HDL_cholesterol, verbose = FALSE,  family = "gaussian", seed = 1)
saveRDS(stability_lasso_HDL_cholesterol_new, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_HDL_cholesterol_new.rds")


##################AF

bio_selected_new$com_has_AF1 = as.factor(bio_selected_new$com_has_AF1)

stability_lasso_com_AF_new <- VariableSelection(xdata = env_only, ydata = bio_selected_new$com_has_AF1, verbose = FALSE,  family = "binomial", seed = 1)
saveRDS(stability_lasso_com_AF_new, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_com_AF_new.rds")


###############t2d

bio_selected_new$com_has_t2d1 = as.factor(bio_selected_new$com_has_t2d1)
stability_lasso_com_t2d_new<- VariableSelection(xdata = env_only, ydata = bio_selected_new$com_has_t2d1, verbose = FALSE,  family = "binomial", seed = 1)
saveRDS(stability_lasso_com_t2d_new, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_com_t2d_new.rds")



##################ckd

bio_selected_new$com_has_ckd1 = as.factor(bio_selected_new$com_has_ckd1)

stability_lasso_com_ckd_new<- VariableSelection(xdata = env_only, ydata = bio_selected_new$com_has_ckd1, verbose = FALSE,  family = "binomial", seed = 1)
saveRDS(stability_lasso_com_ckd_new, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_com_ckd_new.rds")

####################RA
bio_selected_new$com_has_RA1 = as.factor(bio_selected_new$com_has_RA1)
stability_lasso_com_RA_new <- VariableSelection(xdata = env_only, ydata = bio_selected_new$com_has_RA1, verbose = FALSE,  family = "binomial", seed = 1)
saveRDS(stability_lasso_com_RA_new, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_com_RA_new.rds")


################psychiatric
bio_selected_new$com_has_psychiatric1 = as.factor(bio_selected_new$com_has_psychiatric1)
stability_lasso_com_psychiatric_new<- VariableSelection(xdata = env_only, ydata = bio_selected_new$com_has_psychiatric1, verbose = FALSE,  family = "binomial", seed = 1)
saveRDS(stability_lasso_com_psychiatric_new, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_com_psychiatric_new.rds")


##################### bp
bio_selected_new$com_bp1 = as.factor(bio_selected_new$com_bp1)
stability_lasso_com_bp_new<- VariableSelection(xdata = env_only, ydata = bio_selected_new$com_bp1, verbose = FALSE,  family = "binomial", seed = 1)
saveRDS(stability_lasso_com_bp_new, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_com_bp_new.rds")

##############MAP
stability_lasso_MAP_new<- VariableSelection(xdata = env_only, ydata = bio_selected_new$MAP_bio, verbose = FALSE,  family = "gaussian", seed = 1)
saveRDS(stability_lasso_MAP_new, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_MAP_new.rds")


###################Now the additional variables selected for the uncalibrated model#######################


####################### Lipo A
stability_lasso_Lipoprotein_A_new <- VariableSelection(xdata = env_only, ydata = bio_selected_new$bio_blood_Lipoprotein_A, verbose = FALSE,  family = "gaussian", seed = 1)
saveRDS(stability_lasso_Lipoprotein_A_new , file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_Lipoprotein_A_new.rds")


################# migraine
bio_selected_new$com_has_migraine1 = as.factor(bio_selected_new$com_has_migraine1)
stability_lasso_com_migraine_new <- VariableSelection(xdata = env_only, ydata = bio_selected_new$com_has_migraine1, verbose = FALSE,  family = "binomial", seed = 1)
saveRDS(stability_lasso_com_migraine_new, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_com_migraine_new.rds")


################ sle
bio_selected_new$com_has_sle1 = as.factor(bio_selected_new$com_has_sle1)
stability_lasso_com_sle_new<- VariableSelection(xdata = env_only, ydata = bio_selected_new$com_has_sle1, verbose = FALSE,  family = "binomial", seed = 1)
saveRDS(stability_lasso_com_sle_new, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_com_sle_new.rds")


##### alkaline phosphatase
#stability_lasso_alkaline_phosphatase_new <- VariableSelection(xdata = env_only, ydata = bio_selected_new$bio_blood_Alkaline_phosphatase, verbose = FALSE,  family = "gaussian", seed = 1)
#saveRDS(stability_lasso_alkaline_phosphatase_new, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_output/stability_lasso_alkaline_phosphatase_new.rds")



###########C-reactiveprotein
#stability_lasso_C_reactive_new<- VariableSelection(xdata = env_only, ydata = bio_selected_new$bio_blood_Creactive_protein, verbose = FALSE,  family = "gaussian", seed = 1)
#saveRDS(stability_lasso_C_reactive_new, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_output/stability_lasso_C_reactive_new.rds")


#################### triglycerides
#stability_lasso_Triglycerides_new<- VariableSelection(xdata = env_only, ydata = bio_selected_new$bio_blood_Triglycerides, verbose = FALSE,  family = "gaussian", seed = 1)
#saveRDS(stability_lasso_Triglycerides_new , file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_output/stability_lasso_Triglycerides_new.rds")

###loading in all the correct models###
#stability_lasso_alkaline_phosphatase_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_output/stability_lasso_alkaline_phosphatase_new.rds")
stability_lasso_com_AF_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_com_AF_new.rds")
stability_lasso_com_bp_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_com_bp_new.rds")
stability_lasso_com_ckd_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_com_ckd_new.rds")
stability_lasso_com_migraine_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_com_migraine_new.rds")
stability_lasso_com_psychiatric_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_com_psychiatric_new.rds")
stability_lasso_com_RA_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_com_RA_new.rds")
stability_lasso_com_sle_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_com_sle_new.rds")
stability_lasso_com_t2d_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_com_t2d_new.rds")
stability_lasso_Cystatin_C_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_Cystatin_C_new.rds")
stability_lasso_HDL_cholesterol_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_HDL_cholesterol_new.rds")
stability_lasso_LDL_direct_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_LDL_direct_new.rds")
stability_lasso_Lipoprotein_A_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_Lipoprotein_A_new.rds")
stability_lasso_MAP_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_MAP_new.rds")
stability_lasso_PRS_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_PRS_new.rds")
stability_lasso_Testosterone_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_logistic/stability_lasso_Testosterone_new.rds")
#stability_lasso_Triglycerides_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_output/stability_lasso_Triglycerides_new.rds")
#stability_lasso_C_reactive_new <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/env_bio_output/stability_lasso_C_reactive_new.rds")

###############################################################################################################################################################################
############################################################################################################################################################################
#######refitting the data ###############################



# loading the following workspace is an option in order not torun the first part
#load("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/calibrated_new_lasso/env_bio_workspace_beforerefit.RData")
#str(x_train)
# x_t2<-x_selection[, !names(x_selection) %in% c("time_to_event", "case", "eid", "time_to_diagnosis", "Date_lost_to_follow_up", "years_to_diagnosis",
#                                        "years_to_death","years_to_lost_to_follow_up", "left_the_study")]
# x_s2 <-x_test[, !names(x_test) %in% c("time_to_event", "case", "eid", "time_to_diagnosis", "Date_lost_to_follow_up", "years_to_diagnosis",
#                                       "years_to_death","years_to_lost_to_follow_up", "left_the_study")]

refit <- sample(1:nrow(x_test), 0.6*nrow(x_test)) # participants for the regression
predict <- seq(1, nrow(x_test))[-refit] # participants for the prediction

linear_refit <- x_test[refit, ] # for linear regression this is the training set - refit
linear_predict<- x_test[predict,] # for linear regression this is the testing set - prediction




# we only want the environmental variables
env_refit = linear_refit %>% select(- which(sapply(names(linear_refit), grepl, pattern = "bio|com"))) # the environment variables for training the regression - refit
env_predict = linear_predict %>% select(- which(sapply(names(linear_predict), grepl, pattern = "bio|com"))) # the environment variables for testing the regression and predict



# reading the stability selected models of the variables
### they can be read from the folder: /rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/calibrated_new_lasso/env_bio_output/
# or just load the workspace:
## note that for each variables there is 2 stability objects: calibrated and uncalibrated - there is 6 additional uncalibrated stabiloity lasso objects



##### this is done to get all the variables that were selected for each of the objects####################
############################### for pfer ################################



###################for uncalibrated model #######################
# create a list of objects that start with 'stability_lasso' and ends with new
object_list_new <- ls(pattern = "^stability_lasso.*new$")
object_list_new


# create an empty list to store the selected variables
selected_list_new <- list()

# create a for loop to apply the function to each object and save the result
for (object in object_list_new) {
  #print(class(get(object))) 
  selected_list_new[[object]] <- names(SelectedVariables(get(object)))[which(SelectedVariables(get(object)) ==1)]
  selected_list_new[[object]] <- c(selected_list_new[[object]], "PRS_bio",  "Ethnic_background_bio2","Ethnic_background_bio3", "Ethnic_background_bio4",
                                   "Ethnic_background_bio5" , "Ethnic_background_bioUnknown" )
}



#############################################################################################

#we first need to adjust env_training and testing to change it to have the right data types 
#changing every collumn with 0 and 1s into factors 
convert_binary_cols_to_factor <- function(df) {
  # Identify columns with only 0's and 1's
  binary_cols <- sapply(df, function(x) all(x == 0 | x == 1))
  
  # Convert binary columns to factors
  df[, binary_cols] <- lapply(df[, binary_cols], as.factor)
  
  # Return the modified data frame
  return(df)
}

#using that function
env_refit %>% convert_binary_cols_to_factor() %>% str()
env_refit <- convert_binary_cols_to_factor(env_refit)
env_predict <- convert_binary_cols_to_factor(env_predict)
str(env_refit)
str(env_predict)
#seeing if the data types are different at any points 
for (col in names(env_refit)) {
  if (!identical(class(env_refit[[col]]), class(env_refit[[col]]))) {
    print(paste("Columns", col, "have different data types"))
  }
}



################## variables selected with calibrated and uncalibrated #########

hdl_model_new = lm(as.formula(paste("bio_blood_HDL_cholesterol ~", paste(unlist(selected_list_new["stability_lasso_HDL_cholesterol_new"]), collapse = "+"))), data = linear_refit)
cystatin_model_new = lm(as.formula(paste("bio_blood_Cystatin_C ~", paste(unlist(selected_list_new["stability_lasso_Cystatin_C_new"]), collapse = "+"))), data = linear_refit)
LDL_model_new = lm(as.formula(paste("bio_blood_LDL_direct ~", paste(unlist(selected_list_new["stability_lasso_LDL_direct_new"]), collapse = "+"))), data = linear_refit)
Testosterone_new_model = lm(as.formula(paste("bio_blood_Testosterone ~", paste(unlist(selected_list_new["stability_lasso_Testosterone_new"]), collapse = "+"))), data = linear_refit)
AF_new_model = lm(as.formula(paste("com_has_AF1 ~", paste(unlist(selected_list_new["stability_lasso_com_AF_new"]), collapse = "+"))), data = linear_refit)
com_t2d_new_model = glm(as.formula(paste("com_has_t2d1 ~", paste(unlist(selected_list_new["stability_lasso_com_t2d_new"]), collapse = "+"))), data = linear_refit)
com_ckd_new_model = glm(as.formula(paste("com_has_ckd1 ~", paste(unlist(selected_list_new["stability_lasso_com_ckd_new"]), collapse = "+"))), data = linear_refit)
com_RA_new = glm(as.formula(paste("com_has_RA1 ~", paste(unlist(selected_list_new["stability_lasso_com_RA_new"]), collapse = "+"))), data = linear_refit)
com_psychiatric_new_model = glm(as.formula(paste("com_has_psychiatric1 ~", paste(unlist(selected_list_new["stability_lasso_com_psychiatric_new"]), collapse = "+"))), data = linear_refit)
com_BP_new_model = lm(as.formula(paste("com_bp1 ~", paste(unlist(selected_list_new["stability_lasso_com_bp_new"]), collapse = "+"))), data = linear_refit)
Map_new_model = lm(as.formula(paste("MAP_bio ~", paste(unlist(selected_list_new["stability_lasso_MAP_new"]), collapse = "+"))), data = linear_refit)
Lipoprotein_new_model = lm(as.formula(paste("bio_blood_Lipoprotein_A ~", paste(unlist(selected_list_new["stability_lasso_Lipoprotein_A_new"]), collapse = "+"))), data = linear_refit)
Migraine_new_model = glm(as.formula(paste("com_has_migraine1 ~", paste(unlist(selected_list_new["stability_lasso_com_migraine_new"]), collapse = "+"))), data = linear_refit)
SLE_new_model = glm(as.formula(paste("com_has_sle1 ~", paste(unlist(selected_list_new["stability_lasso_com_sle_new"]), collapse = "+"))), data = linear_refit)


#doing the prediction for the varaibles 
##making the function 
#Doing prediction 
get_confusion_matrix <- function(model, test_data, y_var) {
  # Step 4: Use predict() to generate predictions
  predictions <- predict(model, newdata = test_data, type = "response")
  
  # Step 5: Convert predicted probabilities to classes
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  predicted_classes <- factor(predicted_classes, levels = c(0, 1))
  
  # Step 6: Create confusion matrix
  test_data[[y_var]] <- factor(test_data[[y_var]], levels = c(0, 1))
  confusion_matrix <- confusionMatrix(data = predicted_classes, reference = test_data[[y_var]])
  
  # Step 7: Create confusion matrix plot using ggplot2
  library(ggplot2)
  
  precision <- confusion_matrix$byClass["Pos Pred Value"]
  recall <- confusion_matrix$byClass["Sensitivity"]
  f_score <- confusion_matrix$byClass["F1"]
  
  precision_formatted <- format(precision, nsmall = 2, decimal.mark = '.')
  recall_formatted <- format(recall, nsmall = 2, decimal.mark = '.')
  f_score_formatted <- format(f_score, nsmall = 2, decimal.mark = '.')
  
  confusion_plot <- ggplot(data = as.data.frame(confusion_matrix$table), aes(x = Prediction, y = Reference, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), color = "black", size = 10) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5), axis.text.y = element_text(angle = 0, hjust = 0.5)) +
    labs(x = "Prediction", y = "Reference", fill = "Count") +
    ggtitle("Confusion Matrix") +
    geom_text(aes(x = 1.5, y = 0.6, label = paste("Precision:", precision_formatted)), size = 5) +
    geom_text(aes(x = 1.5, y = 0.7, label = paste("Recall:", recall_formatted)), size = 5) +
    geom_text(aes(x = 1.5, y = 0.8, label = paste("F-score:", f_score_formatted)), size = 5)
  
  # Step 8: Add F-score to confusion matrix object
  confusion_matrix$f_score <- f_score
  #step 9 library proc curve 
  library(pROC)
  roc_curve <- roc(test_data[[y_var]], predictions)
  auc <- auc(roc_curve)
  ci <- ci.auc(roc_curve)
  roc_data <- data.frame(
    specificity = 1 - roc_curve$specificities,
    sensitivity = roc_curve$sensitivities
  )
  roc_plot <- ggplot(data = roc_data, aes(x = specificity, y = sensitivity)) +
    geom_line(color = "blue") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    labs(x = "False Positive Rate", y = "True Positive Rate", title = paste("ROC Curve (AUC = ", round(auc, 2), "; 95% CI: [", round(ci[1], 2), ", ", round(ci[2], 2), "])")) +
    geom_text(aes(x = 0.5, y = 0.5, label = paste("AUC = ", round(auc, 2), "; 95% CI: [", round(ci[1], 2), ", ", round(ci[2], 2), "]")), size = 5) +
    theme_bw()
  
  return(list(confusion_matrix = confusion_matrix, confusion_plot = confusion_plot, roc_plot = roc_plot))
}                                                                          

#running to prediciton for the glm models models for 
get_confusion_matrix(com_t2d_pfer_model,linear_predict,"com_has_t2d1")
get_confusion_matrix(com_t2d_pfer_model,linear_predict,"com_has_t2d1")
#will do later 
### Evaluating the prediciton ###
cleaned_df$case
#refitting model 






#lipoproteinA_model_new = lm(as.formula(paste("bio_blood_Lipoprotein_A ~", paste(unlist(selected_list_new["stability_lasso_Lipoprotein_A"]), collapse = "+"))), data = linear_refit)
#PRS_model = lm(as.formula(paste("PRS_bio ~", paste(unlist(selected_list["stability_lasso_PRS"]), collapse = "+"))), data = linear_refit)
#Testosterone_model = lm(as.formula(paste("bio_blood_Testosterone ~", paste(unlist(selected_list["stability_lasso_Testosterone"]), collapse = "+"))), data = linear_refit)
#com_AF_model = glm(as.formula(paste("com_has_AF1 ~", paste(unlist(selected_list["stability_lasso_com_AF"]), collapse = "+"))), data = linear_refit)
#com_bp_model = glm(as.formula(paste("com_bp1 ~", paste(unlist(selected_list["stability_lasso_com_bp"]), collapse = "+"))), data = linear_refit)
#com_t2d_model =glm(as.formula(paste("com_has_t2d1 ~", paste(unlist(selected_list["stability_lasso_com_t2d"]), collapse = "+"))), data = linear_refit)
#com_ckd_model = glm(as.formula(paste("com_has_ckd1 ~", paste(unlist(selected_list["stability_lasso_com_ckd"]), collapse = "+"))), data = linear_refit)
#MAP_model = lm(as.formula(paste("MAP_bio ~", paste(unlist(selected_list["stability_lasso_MAP"]), collapse = "+"))), data = linear_refit)

#sle_model = glm(as.formula(paste("com_has_sle1 ~", paste(unlist(selected_list["stability_lasso_com_sle"]), collapse = "+"))), data = linear_refit)
#RA_model = glm(as.formula(paste("com_has_RA1 ~", paste(unlist(selected_list["stability_lasso_com_RA"]), collapse = "+"))), data = linear_refit)
#psychiatric_model = glm(as.formula(paste("com_has_psychiatric1 ~", paste(unlist(selected_list["stability_lasso_com_psychiatric"]), collapse = "+"))), data = linear_refit)
#migraine_model = glm(as.formula(paste("com_has_migraine1 ~", paste(unlist(selected_list["stability_lasso_com_migraine"]), collapse = "+"))), data = linear_refit)
#CystatinC_model = lm(as.formula(paste("bio_blood_Cystatin_C ~", paste(unlist(selected_list["stability_lasso_Cystatin_C"]), collapse = "+"))), data = linear_refit)
#AlkalineCox_model = lm(as.formula(paste("bio_blood_Alkaline_phosphatase ~", paste(unlist(selected_list["stability_lasso_Alkaline_phosphatase_cox"]), collapse = "+"))), data = linear_refit)
#CreactiveCox_model = lm(as.formula(paste("bio_blood_Creactive_protein ~", paste(unlist(selected_list["stability_lasso_C_reactive_cox"]), collapse = "+"))), data = linear_refit)


T#riglyceridesCox_model = lm(as.formula(paste("bio_blood_Triglycerides ~", paste(unlist(selected_list["stability_lasso_Triglycerides_cox"]), collapse = "+"))), data = linear_refit)



