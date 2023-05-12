##########This is a script for the final plots for TDS
# Load the required packages


#install.packages("forestplot", dependencies = TRUE)
library(ggplot2)
library(abind)

library(forestplot)

#### First, let's try plotting forest plots for the env variables k nthe biological variables
load("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/4_COX_ALL_MODELS.RData")
library(broom)
library(ggplot2)

# Extract coefficients and other information
hdl_coefs <- tidy(hdl_model_new)
hdl_coefs$term

# define a named character vector with the mapping of old names to new names
new_names <- c( "PRS_conf"= "PRS",
                "age_conf" = "Age",
                "sex_conf1" = "Sex",
                "Ethnic_background_conf2" = "Mixed Ethnicity",
                "Ethnic_background_conf3" = "Asian Ethnicity",
                "Ethnic_background_conf4" = "Black Ethnicity",
                "Ethnic_background_conf5" = "Other Ethnicity",
                "Ethnic_background_confUnknown" = "Unknown Ethnicity",
                "Particulate_matter_air_pollution_pm2.5_absorbance__env" = "Particulate matter 2.5 pm absorbance",
             
                "physical_activity_level_env3" = "Over 4 hours of physical activity",
                "physical_activity_level_envUnknown" = "Unknown Physical activity",
                "Maternal_smoking_around_birth_env2" = "Maternal Smoking at Birth Yes",
                
                "MealsAndAlc_env2" = "Meals with Alcohol Yes",
                "MealsAndAlc_envUnknown" = "Meals with Alcohol Unknown",
                "Alcohol_intake_versus_10_years_previously_env3" = "Alcohol intake vs previously Less Now",
                "alcohol_level_env2" = "Alcohol level 3-4 drinks",
                "alcohol_level_env3" = "Alcohol level 5-6 drinks",
                "Past_tobacco_smoking_env2" = "Past tobacco smoking occasionally",
                "time_winter_outdoors_level_env2" = "Time spent outdoors in winter 3-8 hours",
                "diet_variation_env2" = "Sometimes or Often Varies Diet",
                "Current_tobacco_smoking_env2" = "Current Smoker",
                "Smoking_status_env3" = "Never Smoked",
                "Country_of_birth_env2" = "Country of Birth: Wales",
                "sleep_level_env3" = "Over Eight Hours Sleep",
                "frequency_visits_env3" = "Rare Visits from Family or Friends",
                "frequency_visits_envUnknown" = "Unknown Visits from Family or Friends")



 
                

# change the row names in the first column
hdl_coefs$term[match(names(new_names), hdl_coefs$term)] <- new_names
# check that all names in new_names have a match in hdl_coefs$term
if (!all(names(new_names) %in% hdl_coefs$term)) {
  stop("Not all names in new_names have a match in hdl_coefs$term")
}

# Create forest plot
ggplot(hdl_coefs, aes(x = estimate, xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(height = 0.2, size = 0.5) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(-0.9, 0.7)) +
  labs(title = "Forest plot for HDL Biomarker", x = "Beta coefficient", y = "Variables")


# Extract coefficients and other information
cystatin_coefs <- tidy(cystatin_model_new)
cystatin_coefs$term
new_names_cystatin = c(
  "PRS_conf"= "PRS",
  "age_conf" = "Age",
  "sex_conf1" = "Sex",
  "Ethnic_background_conf2" = "Mixed Ethnicity",
  "Ethnic_background_conf3" = "Asian Ethnicity",
  "Ethnic_background_conf4" = "Black Ethnicity",
  "Ethnic_background_conf5" = "Other Ethnicity",
  "Ethnic_background_confUnknown" = "Unknown Ethnicity",
  "TobaccoExposure_home_env" = "Tobacco exposure at home",
  "TobaccoExposure_outside_env" = "Tobacco exposure outside",
  "Townsend_deprivation_index_at_recruitment_env" = "Townsend deprivation index" , 
  "Distance_Euclidean_to_coast_env" = "Euclidean distance to coast",
  
  "physical_activity_level_env3" = "Over 4 hours of physical activity",
  "physical_activity_level_envUnknown" = "Unknown Physical activity",
  "MealsAndAlc_env2" = "Meals with Alcohol Yes",
  "MealsAndAlc_envUnknown" = "Meals with Alcohol Unknown",
  "alcohol_level_env2" = "Alcohol level 3-4 drinks",
  "alcohol_level_env3" = "Alcohol level 5-6 drinks",
  "smokers_in_household_env2" = "Smokers in household Yes",
  "Past_tobacco_smoking_env3" = "Past tobacco smoking once or twice",
  "time_winter_outdoors_level_envUnknown" = "Time spent outdoors in winter Unknown",
  "diet_variation_env2" = "Sometimes or Often Varies Diet",
  "Current_tobacco_smoking_env2" = "Current Smoker",
  "Smoking_status_env3" = "Never Smoked",
  "Country_of_birth_env5" = "Country of Birth: Republic of Ireland",
  "dash_env2" = "DASH band 2",
  "dash_env3" = "DASH band 3",
  "dash_env4" = "DASH band 4",
  "dash_env5" = "DASH band 5"
  
  
)
cystatin_coefs$term[match(names(new_names_cystatin), cystatin_coefs$term)] <- new_names_cystatin

# Create forest plot
ggplot(cystatin_coefs, aes(x = estimate, xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(height = 0.2, size = 0.5) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(-0.5, 1)) +
  labs(title = "Forest plot for Cystatin", x = "Beta coefficient", y = "Variables")

# Extract coefficients and other information
LDL_coefs <- tidy(LDL_model_new)
LDL_coefs$term

new_names_LDL = c(
  "PRS_conf"= "PRS",
  "age_conf" = "Age",
  "sex_conf1" = "Sex",
  "Ethnic_background_conf2" = "Mixed Ethnicity",
  "Ethnic_background_conf3" = "Asian Ethnicity",
  "Ethnic_background_conf4" = "Black Ethnicity",
  "Ethnic_background_conf5" = "Other Ethnicity",
  "Ethnic_background_confUnknown" = "Unknown Ethnicity",
  "Townsend_deprivation_index_at_recruitment_env" = "Townsend deprivation index" , 
  "Nitrogen_dioxide_air_pollution__env" = "Nitrogen dioxide in the air",
 
  "Natural_environment_percentage_buffer_m_env" = "Natural environment percentage buffer",
  "physical_activity_level_env3" = "Over 4 hours of physical activity",
  "physical_activity_level_envUnknown" = "Unknown Physical activity",
  "MealsAndAlc_envUnknown" = "Meals with Alcohol Unknown",

  "alcohol_level_env2" = "Alcohol level 3-4 drinks",
 
  "time_summer_outdoors_level_env2" = "Time spent outdoors in summer 3-8 hours",
  "diet_variation_env2" = "Sometimes or Often Varies Diet",

  "Country_of_birth_env3" = "Country of Birth: Scotland",
  "Country_of_birth_env5" = "Country of Birth: Republic of Ireland",
  "Country_of_birth_env6" = "Country of Birth: Outside of British Isles",
  "frequency_visits_env3" = "Rare Visits from Family or Friends",
  "dash_env3" = "DASH band 3",
  "dash_env4" = "DASH band 4",
  "dash_env5" = "DASH band 5"
  
  
)

LDL_coefs$term[match(names(new_names_LDL), LDL_coefs$term)] <- new_names_LDL

# Create forest plot
ggplot(LDL_coefs, aes(x = estimate, xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(height = 0.2, size = 0.5) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(-0.5, 1)) +
  labs(title = "Forest plot for LDL", x = "Beta coefficient", y = "Variables")

# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Calculate odds ratios and CIs
AF_coefs <- tidy(AF_new_model, exponentiate = TRUE, conf.int = TRUE) 
AF_coefs$term
new_names_AF = c(
  "PRS_conf"= "PRS",
  "age_conf" = "Age",
  "sex_conf1" = "Sex",
  "Ethnic_background_conf2" = "Mixed Ethnicity",
  "Ethnic_background_conf3" = "Asian Ethnicity",
  "Ethnic_background_conf4" = "Black Ethnicity",
  "Ethnic_background_conf5" = "Other Ethnicity",
  "Ethnic_background_confUnknown" = "Unknown Ethnicity",
  "Smoking_status_env2" = "Previously Smoked",
  
  "Maternal_smoking_around_birth_env2" = "Maternal Smoking at Birth Yes",
  "MealsAndAlc_env2" = "Meals with Alcohol Yes",
  "Alcohol_intake_versus_10_years_previously_env2" = "Alcohol intake vs previously Same",
  "Country_of_birth_env2" = "Country of Birth: Wales",
 
  "sleep_level_env2" = "Six to Eight Hours Sleep"

)

AF_coefs$term[match(names(new_names_AF), AF_coefs$term)] <- new_names_AF

# Create forest plot
ggplot(AF_coefs, aes(x = estimate, xmin = conf.low, xmax = conf.high, y = reorder(term, estimate))) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(height = 0.2, size = 0.5) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(0.8, 1.2), breaks = c(0.8, 0.9, 1, 1.1, 1.2)) +
  labs(title = "Forest plot for atrial fibrillation", x = "Odds ratio", y = "Variables")


# Calculate odds ratios and CIs
t2d_coefs <- tidy(com_t2d_new_model, exponentiate = TRUE, conf.int = TRUE) 


t2d_coefs$term
new_names_t2d = c(
  "PRS_conf"= "PRS",
  "age_conf" = "Age",
  "sex_conf1" = "Sex",
  "Ethnic_background_conf2" = "Mixed Ethnicity",
  "Ethnic_background_conf3" = "Asian Ethnicity",
  "Ethnic_background_conf4" = "Black Ethnicity",
  "Ethnic_background_conf5" = "Other Ethnicity",
  "Ethnic_background_confUnknown" = "Unknown Ethnicity",
  "TobaccoExposure_outside_env" = "Tobacco exposure outside",
  "physical_activity_level_env2" = "Physical activity 1 to 4 hours",
  "physical_activity_level_env3" = "Over 4 hours of physical activity",
  "Maternal_smoking_around_birth_env2" = "Maternal Smoking at Birth Yes",
  "Maternal_smoking_around_birth_envUnknown" = "Maternal Smoking at birth Unknown",
  "MealsAndAlc_env2" = "Meals with Alcohol Yes",
  "MealsAndAlc_envUnknown" = "Meals with Alcohol Unknown",
  "Alcohol_intake_versus_10_years_previously_env3" = "Alcohol intake vs previously Less Now",
  "alcohol_level_env2" = "Alcohol level 3-4 drinks",
  "alcohol_level_env3" = "Alcohol level 5-6 drinks",
  "Past_tobacco_smoking_env2" = "Past tobacco smoking occasionally",
  "diet_variation_env2" = "Sometimes or Often Varies Diet",
  "Smoking_status_env2" = "Previously Smoked",
  "Country_of_birth_env3" = "Country of Birth: Scotland",
  "sleep_level_env2" = "Six to Eight Hours Sleep",
  "frequency_visits_env4" = "Never Recieves Visits from Family or Friends",
  "frequency_visits_envUnknown" = "Unknown Visits from Family or Friends",
  "dash_env5" = "DASH band 5"
  
  
)
t2d_coefs$term[match(names(new_names_t2d), t2d_coefs$term)] <- new_names_t2d


# Create forest plot
ggplot(t2d_coefs, aes(x = estimate, xmin = conf.low, xmax = conf.high, y = reorder(term, estimate))) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(height = 0.2, size = 0.5) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(0.8, 1.2), breaks = c(0.8, 0.9, 1, 1.1, 1.2)) +
  labs(title = "Forest plot for Type 2 diabetes", x = "Odds ratio", y = "Variables")

# Calculate odds ratios and CIs
cdk_coefs <- tidy(com_ckd_new_model, exponentiate = TRUE, conf.int = TRUE) 
cdk_coefs$term
new_names_ckd = c(
  "PRS_conf"= "PRS",
  "age_conf" = "Age",
  "sex_conf1" = "Sex",
  "Ethnic_background_conf2" = "Mixed Ethnicity",
  "Ethnic_background_conf3" = "Asian Ethnicity",
  "Ethnic_background_conf4" = "Black Ethnicity",
  "Ethnic_background_conf5" = "Other Ethnicity",
  "Ethnic_background_confUnknown" = "Unknown Ethnicity",
 
  "Distance_Euclidean_to_coast_env" = "Euclidean distance to coast",
 
  "MealsAndAlc_env2" = "Meals with Alcohol Yes",
  "MealsAndAlc_envUnknown" = "Meals with Alcohol Unknown",

  "Alcohol_intake_versus_10_years_previously_env3" = "Alcohol intake vs previously Less Now",
  "alcohol_level_env2" = "Alcohol level 3-4 drinks",
  "alcohol_level_env3" = "Alcohol level 5-6 drinks",
  "Country_of_birth_env2" = "Country of Birth: Wales",
  "Country_of_birth_env3" = "Country of Birth: Scotland",
  
  "sleep_level_env2" = "Six to Eight Hours Sleep",
  "dash_env5" = "DASH band 5")

cdk_coefs$term[match(names(new_names_ckd), cdk_coefs$term)] <- new_names_ckd


# Create forest plot
ggplot(cdk_coefs, aes(x = estimate, xmin = conf.low, xmax = conf.high, y = reorder(term, estimate))) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(height = 0.2, size = 0.5) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(0.8, 1.2), breaks = c(0.8, 0.9, 1, 1.1, 1.2)) +
  labs(title = "Forest plot for Chronic Kidney Disease", x = "Odds ratio", y = "Variables")


# Calculate odds ratios and CIs
RA_coefs <- tidy(com_RA_new, exponentiate = TRUE, conf.int = TRUE) 

RA_coefs$term
new_names_RA = c ( "PRS_conf"= "PRS",
                   "age_conf" = "Age",
                   "sex_conf1" = "Sex",
                   "Ethnic_background_conf2" = "Mixed Ethnicity",
                   "Ethnic_background_conf3" = "Asian Ethnicity",
                   "Ethnic_background_conf4" = "Black Ethnicity",
                   "Ethnic_background_conf5" = "Other Ethnicity",
                   "Ethnic_background_confUnknown" = "Unknown Ethnicity",
                   "TobaccoExposure_outside_env" = "Tobacco exposure outside",
                   "Townsend_deprivation_index_at_recruitment_env" = "Townsend deprivation index" , 
                   
                   "physical_activity_level_env2" = "Physical activity 1 to 4 hours",
                   "physical_activity_level_env3" = "Over 4 hours of physical activity",
                   
                   "MealsAndAlc_env2" = "Meals with Alcohol Yes",
                   "Alcohol_intake_versus_10_years_previously_env3" = "Alcohol intake vs previously Less Now",
                   "Alcohol_intake_versus_10_years_previously_envUnknown" = "Alcohol intake vs previously Unknown",
                   "alcohol_level_env3" = "Alcohol level 5-6 drinks",
                  
                   "diet_variation_env2" = "Sometimes or Often Varies Diet",
                   "Smoking_status_env3" = "Never Smoked",
                   "Smoking_status_env2" = "Previously Smoked",
                   
                   "Country_of_birth_env6" = "Country of Birth: Outside of British Isles",
                   "sleep_level_env2" = "Six to Eight Hours Sleep",
                   "frequency_visits_env3" = "Rare Visits from Family or Friends"
                )
RA_coefs$term[match(names(new_names_RA), RA_coefs$term)] <- new_names_RA


# Create forest plot
ggplot(RA_coefs, aes(x = estimate, xmin = conf.low, xmax = conf.high, y = reorder(term, estimate))) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(height = 0.2, size = 0.5) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(0.8, 1.2), breaks = c(0.8, 0.9, 1, 1.1, 1.2)) +
  labs(title = "Forest plot for Rheumatoid Arthritis", x = "Odds ratio", y = "Variables")



# Calculate odds ratios and CIs


psychiatric_coefs <- tidy(com_psychiatric_new_model, exponentiate = TRUE, conf.int = TRUE) 

psychiatric_coefs$term
new_names_psychiatric = c(  "PRS_conf"= "PRS",
                            "age_conf" = "Age",
                            "sex_conf1" = "Sex",
                            "Ethnic_background_conf2" = "Mixed Ethnicity",
                            "Ethnic_background_conf3" = "Asian Ethnicity",
                            "Ethnic_background_conf4" = "Black Ethnicity",
                            "Ethnic_background_conf5" = "Other Ethnicity",
                            "Ethnic_background_confUnknown" = "Unknown Ethnicity",
                           
                            "Townsend_deprivation_index_at_recruitment_env" = "Townsend deprivation index" , 
                          
                            "physical_activity_level_envUnknown" = "Unknown Physical activity",
                            "Maternal_smoking_around_birth_env2" = "Maternal Smoking at Birth Yes",
                            "MealsAndAlc_env2" = "Meals with Alcohol Yes",
                            "Alcohol_intake_versus_10_years_previously_env2" = "Alcohol intake vs previously Same",
                            "Alcohol_intake_versus_10_years_previously_envUnknown" = "Alcohol intake vs previously Unknown",
                            "alcohol_level_env3" = "Alcohol level 5-6 drinks",
                           
                           
                            "Country_of_birth_env2" = "Country of Birth: Wales",
                          
                            "sleep_level_env2" = "Six to Eight Hours Sleep",
                            "sleep_level_env3" = "Over Eight Hours Sleep",
                            "sleep_level_envUnknown" = "Unknown Hours of Sleep",
                            "frequency_visits_env4" = "Never Recieves Visits from Family or Friends"
                          )
psychiatric_coefs$term[match(names(new_names_psychiatric), psychiatric_coefs$term)] <- new_names_psychiatric

# Create forest plot
ggplot(psychiatric_coefs, aes(x = estimate, xmin = conf.low, xmax = conf.high, y = reorder(term, estimate))) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(height = 0.2, size = 0.5) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(0.8, 1.2), breaks = c(0.8, 0.9, 1, 1.1, 1.2)) +
  labs(title = "Forest plot for Severe Mental Illness", x = "Odds ratio", y = "Variables")


# Extract coefficients and other information
BP_coefs <-tidy(com_BP_new_model, exponentiate = TRUE, conf.int = TRUE)
BP_coefs$term

new_names_BP = c(  "PRS_conf"= "PRS",
                   "age_conf" = "Age",
                   "sex_conf1" = "Sex",
                   "Ethnic_background_conf2" = "Mixed Ethnicity",
                   "Ethnic_background_conf3" = "Asian Ethnicity",
                   "Ethnic_background_conf4" = "Black Ethnicity",
                   "Ethnic_background_conf5" = "Other Ethnicity",
                   "Ethnic_background_confUnknown" = "Unknown Ethnicity",
                   "Townsend_deprivation_index_at_recruitment_env" = "Townsend deprivation index" , 
                  
                   "physical_activity_level_env3" = "Over 4 hours of physical activity",
                   "physical_activity_level_envUnknown" = "Unknown Physical activity",
                   "Maternal_smoking_around_birth_env2" = "Maternal Smoking at Birth Yes",
                   "MealsAndAlc_env2" = "Meals with Alcohol Yes",
                   "Alcohol_intake_versus_10_years_previously_env3" = "Alcohol intake vs previously Less Now",
                   "alcohol_level_env3" = "Alcohol level 5-6 drinks",
                   
                   "Past_tobacco_smoking_env2" = "Past tobacco smoking occasionally",
                   "Past_tobacco_smoking_env3" = "Past tobacco smoking once or twice",
                 
                   "diet_variation_env2" = "Sometimes or Often Varies Diet",
                  
                   "Smoking_status_env3" = "Never Smoked",
                  
                   "sleep_level_env2" = "Six to Eight Hours Sleep",
                  
                   "frequency_visits_envUnknown" = "Unknown Visits from Family or Friends")

BP_coefs$term[match(names(new_names_BP), BP_coefs$term)] <- new_names_BP

# Create forest plot


ggplot(BP_coefs, aes(x = estimate, xmin = conf.low, xmax = conf.high, y = reorder(term, estimate))) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(height = 0.2, size = 0.5) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(0.8, 1.2), breaks = c(0.8, 0.9, 1, 1.1, 1.2)) +
  labs(title ="Forest plot for Blood pressure medication", x = "Odds ratio", y = "Variables")



# Extract coefficients and other information
map_coefs <- tidy(Map_new_model)
map_coefs$term
new_names_map = c("PRS_conf"= "PRS",
                  "age_conf" = "Age",
                  "sex_conf1" = "Sex",
                  "Ethnic_background_conf2" = "Mixed Ethnicity",
                  "Ethnic_background_conf3" = "Asian Ethnicity",
                  "Ethnic_background_conf4" = "Black Ethnicity",
                  "Ethnic_background_conf5" = "Other Ethnicity",
                  "Ethnic_background_confUnknown" = "Unknown Ethnicity",

                  "Particulate_matter_air_pollution_pm2.5_absorbance__env" = "Particulate matter 2.5 pm absorbance",


                  "Close_to_major_road_env1" = "Close to major road Yes",
                  "Water_percentage_buffer_m_env" = "Water percentage buffer",
                  "Natural_environment_percentage_buffer_m_env" = "Natural environment percentage buffer",
                  "Distance_Euclidean_to_coast_env" = "Euclidean distance to coast",

                  
                  "Maternal_smoking_around_birth_envUnknown" = "Maternal Smoking at birth Unknown",
                  "MealsAndAlc_env2" = "Meals with Alcohol Yes",
                  "MealsAndAlc_envUnknown" = "Meals with Alcohol Unknown",

                  "alcohol_level_env2" = "Alcohol level 3-4 drinks",
                  "smokers_in_household_env2" = "Smokers in household Yes",

                  "Past_tobacco_smoking_env3" = "Past tobacco smoking once or twice",
                  "Past_tobacco_smoking_env4" = "Past tobacco smoking never",
                 
                  "time_winter_outdoors_level_env3" = "Time spent outdoors in winter over 8 hours",
                  "time_winter_outdoors_level_envUnknown" = "Time spent outdoors in winter Unknown",
                  "time_summer_outdoors_level_env2" = "Time spent outdoors in summer 3-8 hours",
                  "time_summer_outdoors_level_env3" = "Time spent outdoors in summer over 8 hours",
                  "diet_variation_env2" = "Sometimes or Often Varies Diet",

                  "Smoking_status_env3" = "Never Smoked",
                  "Country_of_birth_env2" = "Country of Birth: Wales",
                  "Country_of_birth_env4" = "Country of Birth: Northern Ireland",
                  "Country_of_birth_env5" = "Country of Birth: Republic of Ireland",

                  "frequency_visits_env3" = "Rare Visits from Family or Friends",
                  "frequency_visits_envUnknown" = "Unknown Visits from Family or Friends",
                  "dash_env4" = "DASH band 4",
                  "dash_env5" = "DASH band 5")

map_coefs$term[match(names(new_names_map), map_coefs$term)] <- new_names_map

# Create forest plot
ggplot(map_coefs, aes(x = estimate, xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(height = 0.2, size = 0.5) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  labs(title = "Forest plot for MAP model", x = "Beta coefficient", y = "Variables")


# Extract coefficients and other information
Lipoprotein_coefs <- tidy(Lipoprotein_new_model)
Lipoprotein_coefs$term
new_names_lipoprotein = c("PRS_conf"= "PRS",
                          "age_conf" = "Age",
                          "sex_conf1" = "Sex",
                          "Ethnic_background_conf2" = "Mixed Ethnicity",
                          "Ethnic_background_conf3" = "Asian Ethnicity",
                          "Ethnic_background_conf4" = "Black Ethnicity",
                          "Ethnic_background_conf5" = "Other Ethnicity",
                          "Ethnic_background_confUnknown" = "Unknown Ethnicity",
                          
                          "Distance_Euclidean_to_coast_env" = "Euclidean distance to coast",
                        
                          "Country_of_birth_env3" = "Country of Birth: Scotland",
                          "dash_env4" = "DASH band 4"
                          )

Lipoprotein_coefs$term[match(names(new_names_lipoprotein), Lipoprotein_coefs$term)] <- new_names_lipoprotein

# Create forest plot
ggplot(Lipoprotein_coefs, aes(x = estimate, xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(height = 0.2, size = 0.5) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(-0.1, 0.7)) +
  labs(title = "Forest plot for Lipoprotein A Biomarker", x = "Beta coefficient", y = "Variables")

# Extract coefficients and other information
phos_coefs <- tidy(Phosphatase_new_model)
phos_coefs$term
new_names_phos = c("PRS_conf"= "PRS",
                   "age_conf" = "Age",
                   "sex_conf1" = "Sex",
                   "Ethnic_background_conf2" = "Mixed Ethnicity",
                   "Ethnic_background_conf3" = "Asian Ethnicity",
                   "Ethnic_background_conf4" = "Black Ethnicity",
                   "Ethnic_background_conf5" = "Other Ethnicity",
                   "Ethnic_background_confUnknown" = "Unknown Ethnicity",
                   "TobaccoExposure_outside_env" = "Tobacco exposure outside",
                   "Townsend_deprivation_index_at_recruitment_env" = "Townsend deprivation index" , 
                   "Particulate_matter_air_pollution_pm2.5_absorbance__env" = "Particulate matter 2.5 pm absorbance",
                   "Maternal_smoking_around_birth_env2" = "Maternal Smoking at Birth Yes",
                   "Maternal_smoking_around_birth_envUnknown" = "Maternal Smoking at birth Unknown",
                   "MealsAndAlc_env2" = "Meals with Alcohol Yes",
                   "Alcohol_intake_versus_10_years_previously_env3" = "Alcohol intake vs previously Less Now",
                   "alcohol_level_env2" = "Alcohol level 3-4 drinks",
                   "alcohol_level_env3" = "Alcohol level 5-6 drinks",
                   "Past_tobacco_smoking_env2" = "Past tobacco smoking occasionally",
                   "Past_tobacco_smoking_env3" = "Past tobacco smoking once or twice",
                   "time_winter_outdoors_level_env3" = "Time spent outdoors in winter over 8 hours",
                   "time_winter_outdoors_level_envUnknown" = "Time spent outdoors in winter Unknown",
                   "diet_variation_env2" = "Sometimes or Often Varies Diet",
                   "Country_of_birth_env6" = "Country of Birth: Outside of British Isles",
                   "sleep_level_env2" = "Six to Eight Hours Sleep",
                   "frequency_visits_env4" = "Never Recieves Visits from Family or Friends",
                   "frequency_visits_envUnknown" = "Unknown Visits from Family or Friends",
                   "dash_env3" = "DASH band 3",
                   "dash_env4" = "DASH band 4",
                   "dash_env5" = "DASH band 5")

phos_coefs$term[match(names(new_names_phos), phos_coefs$term)] <- new_names_phos

# Create forest plot
ggplot(phos_coefs, aes(x = estimate, xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(height = 0.2, size = 0.5) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  labs(title = "Forest plot for Phosphate Biomarker", x = "Beta coefficient", y = "Variables")



# Extract coefficients and other information
crea_coefs <- tidy(C_reactive_new_model)
crea_coefs$term
new_names_crea = c( "PRS_conf"= "PRS",
                    "age_conf" = "Age",
                    "sex_conf1" = "Sex",
                    "Ethnic_background_conf2" = "Mixed Ethnicity",
                    "Ethnic_background_conf3" = "Asian Ethnicity",
                    "Ethnic_background_conf4" = "Black Ethnicity",
                    "Ethnic_background_conf5" = "Other Ethnicity",
                    "Ethnic_background_confUnknown" = "Unknown Ethnicity",
                    "TobaccoExposure_outside_env" = "Tobacco exposure outside",
                    "Townsend_deprivation_index_at_recruitment_env" = "Townsend deprivation index" , 

                    "physical_activity_level_env2" = "Physical activity 1 to 4 hours",
                    "physical_activity_level_env3" = "Over 4 hours of physical activity",
                    "physical_activity_level_envUnknown" = "Unknown Physical activity",
                    "Maternal_smoking_around_birth_env2" = "Maternal Smoking at Birth Yes",
                    "MealsAndAlc_env2" = "Meals with Alcohol Yes",
                    "Alcohol_intake_versus_10_years_previously_env2" = "Alcohol intake vs previously Same",
                    "Alcohol_intake_versus_10_years_previously_env3" = "Alcohol intake vs previously Less Now",
                    "alcohol_level_env2" = "Alcohol level 3-4 drinks",
                    "alcohol_level_env3" = "Alcohol level 5-6 drinks",
                    "Past_tobacco_smoking_env2" = "Past tobacco smoking occasionally",
                    "Past_tobacco_smoking_env3" = "Past tobacco smoking once or twice",
                    "smokers_in_household_env2" = "Smokers in household Yes",
                    
                    "Smoking_status_env2" = "Previously Smoked",
                    "Smoking_status_env3" = "Never Smoked",
                    "Country_of_birth_env2" = "Country of Birth: Wales",
                  "Country_of_birth_env6" = "Country of Birth: Outside of British Isles",
             "sleep_level_env2" = "Six to Eight Hours Sleep",
                    "sleep_level_env3" = "Over Eight Hours Sleep",
                    "frequency_visits_env3" = "Rare Visits from Family or Friends",
                    "frequency_visits_envUnknown" = "Unknown Visits from Family or Friends",
                    "dash_env3" = "DASH band 3",
                    "dash_env4" = "DASH band 4",
                    "dash_env5" = "DASH band 5")
crea_coefs$term[match(names(new_names_crea), crea_coefs$term)] <- new_names_crea


# Create forest plot
ggplot(crea_coefs, aes(x = estimate, xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(height = 0.2, size = 0.5) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  labs(title = "Forest plot for C-reactive protein Biomarker", x = "Beta coefficient", y = "Variables")


# Calculate odds ratios and CIs
migraine_coefs <- tidy(Migraine_new_model, exponentiate = TRUE, conf.int = TRUE)
migraine_coefs$term

new_names_migraine = c("PRS_conf"= "PRS",
                       "age_conf" = "Age",
                       "sex_conf1" = "Sex",
                       "Ethnic_background_conf2" = "Mixed Ethnicity",
                       "Ethnic_background_conf3" = "Asian Ethnicity",
                       "Ethnic_background_conf4" = "Black Ethnicity",
                       "Ethnic_background_conf5" = "Other Ethnicity",
                       "Ethnic_background_confUnknown" = "Unknown Ethnicity",
                     
                       "MealsAndAlc_envUnknown" = "Meals with Alcohol Unknown",
                      
                       "Alcohol_intake_versus_10_years_previously_envUnknown" = "Alcohol intake vs previously Unknown",
                       "alcohol_level_env2" = "Alcohol level 3-4 drinks",
                       "time_summer_outdoors_level_env2" = "Time spent outdoors in summer 3-8 hours",

                       "Smoking_status_env3" = "Never Smoked",

                       "Country_of_birth_env3" = "Country of Birth: Scotland",

                       "sleep_level_env2" = "Six to Eight Hours Sleep"

                   )
migraine_coefs$term[match(names(new_names_migraine), migraine_coefs$term)] <- new_names_migraine

# Create forest plot
ggplot(migraine_coefs, aes(x = estimate, xmin = conf.low, xmax = conf.high, y = reorder(term, estimate))) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(height = 0.2, size = 0.5) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(0.8, 1.2), breaks = c(0.8, 0.9, 1, 1.1, 1.2)) +
  labs(title = "Forest plot for Migraine", x = "Odds ratio", y = "Variables")

# Calculate odds ratios and CIs
SLE_coefs <- tidy(SLE_new_model, exponentiate = TRUE, conf.int = TRUE)
SLE_coefs$term
new_names_sle = c (
  "PRS_conf"= "PRS",
  "age_conf" = "Age",
  "sex_conf1" = "Sex",
  "Ethnic_background_conf2" = "Mixed Ethnicity",
  "Ethnic_background_conf3" = "Asian Ethnicity",
  "Ethnic_background_conf4" = "Black Ethnicity",
  "Ethnic_background_conf5" = "Other Ethnicity",
  "Ethnic_background_confUnknown" = "Unknown Ethnicity",
  "TobaccoExposure_home_env" = "Tobacco exposure at home",
  "Townsend_deprivation_index_at_recruitment_env" = "Townsend deprivation index" , 
  
  "TobaccoExposure_outside_env" = "Tobacco exposure outside",
  "Traffic_intensity_on_the_nearest_road_env" = "Traffic intensity on the nearest road",
  "Total_traffic_load_on_major_roads_env" = "Total traffic loads on major roads",
  "Close_to_major_road_env0.2" = "Close to major road Unknown1",
  "Close_to_major_road_env0.4" = "Close to major road Unknown2",
  "Water_percentage_buffer_m_env" = "Water percentage buffer",
  "Domestic_garden_percentage_buffer_300m_env" = "Domestic garden percentage buffer",
  "Distance_Euclidean_to_coast_env" = "Euclidean distance to coast",
  "physical_activity_level_env2" = "Physical activity 1 to 4 hours",
  "Maternal_smoking_around_birth_env2" = "Maternal Smoking at Birth Yes",
  "Maternal_smoking_around_birth_envUnknown" = "Maternal Smoking at birth Unknown",
  "MealsAndAlc_env2" = "Meals with Alcohol Yes",
  "Alcohol_intake_versus_10_years_previously_env3" = "Alcohol intake vs previously Less Now",
  "Alcohol_intake_versus_10_years_previously_envUnknown" = "Alcohol intake vs previously Unknown",
  "alcohol_level_env2" = "Alcohol level 3-4 drinks",
  "alcohol_level_env3" = "Alcohol level 5-6 drinks",
  "alcohol_level_envUnknown" = "Unknown alcohol level",
  "smokers_in_household_env2" = "Smokers in household Yes",
  "Past_tobacco_smoking_env2" = "Past tobacco smoking occasionally",
  "time_winter_outdoors_level_env2" = "Time spent outdoors in winter 3-8 hours",
  "time_winter_outdoors_level_env3" = "Time spent outdoors in winter over 8 hours",
  "time_winter_outdoors_level_envUnknown" = "Time spent outdoors in winter Unknown",
  "time_summer_outdoors_level_env2" = "Time spent outdoors in summer 3-8 hours",
  "time_summer_outdoors_level_env3" = "Time spent outdoors in summer over 8 hours",
  "time_summer_outdoors_level_envUnknown" = "Time spent outdoors in summer Unknown",
  "Smoking_status_env2" = "Previously Smoked",
  "Ever_smoked_envUnknown" = "Declined to Answer re: Ever smoked",
  "Country_of_birth_env2" = "Country of Birth: Wales",
  "Country_of_birth_env3" = "Country of Birth: Scotland",
  "Country_of_birth_env4" = "Country of Birth: Northern Ireland",
  "Country_of_birth_env5" = "Country of Birth: Republic of Ireland",
  "Country_of_birth_env6" = "Country of Birth: Outside of British Isles",
  "Country_of_birth_envUnknown" = "Country of Birth Unknown",
  "sleep_level_env2" = "Six to Eight Hours Sleep",
  "sleep_level_env3" = "Over Eight Hours Sleep",
  "sleep_level_envUnknown" = "Unknown Hours of Sleep",
  "frequency_visits_env2" = "More Than Weekly Visits from Family or Friends",
  "frequency_visits_env4" = "Never Recieves Visits from Family or Friends",
  "frequency_visits_envUnknown" = "Unknown Visits from Family or Friends",
  "dash_env4" = "DASH band 4"
)


SLE_coefs$term[match(names(new_names_sle), SLE_coefs$term)] <- new_names_sle

# Create forest plot
ggplot(SLE_coefs, aes(x = estimate, xmin = conf.low, xmax = conf.high, y = reorder(term, estimate))) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(height = 0.2, size = 0.5) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(0.98, 1.02), breaks = c( 0.98, 0.99, 1, 1.01, 1.02)) +
  labs(title = "Forest plot for Systemic lupus erythematosus", x = "Odds ratio", y = "Variables")





############################ALL ON CVD

all_coefs <- tidy(refitted_cox_new, exponentiate = TRUE, conf.int = TRUE)

all_coefs$term
new_names_ALL = c(
  "PRS_conf"= "PRS",
  "age_conf" = "Age",
  "sex_conf1" = "Sex",
  "Ethnic_background_conf2" = "Mixed Ethnicity",
  "Ethnic_background_conf3" = "Asian Ethnicity",
  "Ethnic_background_conf4" = "Black Ethnicity",
  "Ethnic_background_conf5" = "Other Ethnicity",
  "Ethnic_background_confUnknown" = "Unknown Ethnicity",
  "Townsend_deprivation_index_at_recruitment_env" = "Townsend deprivation index" ,
  "bio_blood_Alkaline_phosphatase"= "Alkaline Phosphatase Biomarker",
  "bio_blood_Creactive_protein" = "C-reactive protein Biomarker",
  "bio_blood_Cystatin_C"= "Cystatin C Biomarker",
  "bio_blood_LDL_direct"  = "LDL Biomarker",
  "bio_blood_Lipoprotein_A"  = "Lipoprotein A Biomarker",
  "bio_blood_HDL_cholesterol" = "HDL Biomarker",
  "com_has_AF1" = "Atrial Fibrillation",
  "com_has_migraine1" = "Migraine",
    "com_has_t2d1"=  "Type 2 diabetes",
    "com_has_sle1" = "Systemic Lupus Erythematosus",
    "com_has_psychiatric1"    ="Mental Illness",
    "com_bp1" ="Blood pressure medication",
  "com_has_RA1" = "Rheumatoid Arthritis",

  "alcohol_level_env3" = "Alcohol level 5-6 drinks",

  "time_winter_outdoors_level_env3" = "Time spent outdoors in winter over 8 hours",
  "sleep_level_env2" = "Six to Eight Hours Sleep",
  
  "Smoking_status_env2" = "Previously Smoked",
  "Smoking_status_env3" = "Never Smoked",
  "Country_of_birth_env2" = "Country of Birth: Wales",


  "MAP_bio" = "MAP"
)

all_coefs$term[match(names(new_names_ALL), all_coefs$term)] <- new_names_ALL

ggplot(all_coefs, aes(x = estimate, xmin = conf.low, xmax = conf.high, y = reorder(term, estimate))) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(height = 0.2, size = 0.5) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(0.6, 4), breaks = c(0.6, 0.8,1,1.2,1.4,1.6,1.8,2,3,3.5,4 )) +
  labs(title = "Forest plot for Direct effects on CVD - Cox regression", x = "Odds ratio", y = "Variables")




#####################for logisitic, sensitivity analysis
logistic_coef <- tidy(refitted_logistic, exponentiate = TRUE, conf.int = TRUE)
migraine_coefs <- tidy(Migraine_new_model, exponentiate = TRUE, conf.int = TRUE)


logistic_coef$term
new_names_logistic = c(
  "PRS_conf"= "PRS",
  "age_conf" = "Age",
  "sex_conf1" = "Sex",
  "Ethnic_background_conf2" = "Mixed Ethnicity",
  "Ethnic_background_conf3" = "Asian Ethnicity",
  "Ethnic_background_conf4" = "Black Ethnicity",
  "Ethnic_background_conf5" = "Other Ethnicity",
  "Ethnic_background_confUnknown" = "Unknown Ethnicity",
  "Townsend_deprivation_index_at_recruitment_env" = "Townsend deprivation index" ,
  "bio_blood_Creactive_protein" = "C-reactive protein Biomarker",
  "bio_blood_Cystatin_C"= "Cystatin C Biomarker",
  "bio_blood_LDL_direct"  = "LDL Biomarker",
  "bio_blood_Lipoprotein_A"  = "Lipoprotein A Biomarker",
  "bio_blood_HDL_cholesterol" = "HDL Biomarker",
  "com_has_AF1" = "Atrial Fibrillation",
  "com_has_migraine1" = "Migraine",
  "com_has_t2d1"=  "Type 2 diabetes",
  "com_has_t1d1"=  "Type 1 diabetes",
  
  "com_has_ckd1"  = "Chronic Kidney Disease",
  "com_has_sle1" = "Systemic Lupus Erythematosus",
  "com_has_psychiatric1"    ="Mental Illness",
  "com_bp1" ="Blood pressure medication",
  "com_has_RA1" = "Rheumatoid Arthritis",
  
  "alcohol_level_env3" = "Alcohol level 5-6 drinks",
  
  "time_winter_outdoors_level_env3" = "Time spent outdoors in winter over 8 hours",
  "sleep_level_env2" = "Six to Eight Hours Sleep",
  

  "Smoking_status_env3" = "Never Smoked",
  "Country_of_birth_env2" = "Country of Birth: Wales",
  
  
  "MAP_bio" = "MAP"
)

logistic_coef$term[match(names(new_names_logistic), logistic_coef$term)] <- new_names_logistic
migraine_coefs$term[match(names(new_names_migraine), migraine_coefs$term)] <- new_names_migraine


ggplot(logistic_coef, aes(x = estimate, xmin = conf.low, xmax = conf.high, y = reorder(term, estimate))) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(height = 0.2, size = 0.5) +
  geom_point(size = 2) +
  scale_x_log10(limits =  c(0.038, 4.3), breaks =  c(0.03,0.1,0.25,0.5,1,1.5,2,2.5,3,3.5, 4)) +
  labs(title = "Forest plot for Direct effects on CVD - Logistic regression", x = "Odds ratio", y = "Variables")


