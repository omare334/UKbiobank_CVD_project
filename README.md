# UKbiobank_CVD_project

This repo compiles all the code used for the analysis of 300,000 UK Biobank participants on CVD.

<div>
  <img src="https://github.com/omare334/UKbiobank_CVD_project/assets/98061484/e98d8006-15a6-4df2-92bb-15ba42273a43" alt="Imperial-College-London-logo1" width="200">
  <img src="https://github.com/omare334/UKbiobank_CVD_project/assets/98061484/88f9293e-28c7-411f-8a2c-0de10bbdb7d2" alt="channels4_profile" width="200">
</div>

## Pre-processing 
/This code was used to process data . 
/preprocessing_data was used to deal with the missingess of the data and removing and adjusting
the cases and comorbidities
/data_to_impute is used to set the data up for imputation
/add_dash add dash score to the data 
Comorbidities 
imputed_all_numeric_proper.py is used to actually impute the data 
## Univariate analysis 
UNIVARIATE this code is created for the univariate analysis in which we use glm models and a bonferroni correction to identify varaible most related to outcomes like CVD
## Stability Selection
This folder includes 4 scripts, 2 to perform cox regression and the other 2 to perform logistic regression (as a sensitivity analysis)
These scripts should be run in order
- The *1_* scripts correspond to the main model of our work, selection and refit  were performed: environment and biological variables are set as predictors and CVD as outcome
- The *2_* scripts correspond to the model looking at the indirect effects of the environment on CVD by looking at their effect on the biological variables, selection and refit were performed: environment variables are predictors and biological variables are outcomes.

## Prediction Performance
Prediction performance was assed based on the refited models

## Creating plots
- this folder includes the codes used in order to plot our figures
