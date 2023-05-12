# this is the script to rename the columns in the dataframe
cleaned_df <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/cleaned_df.RDS")
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



#### Renaming the columns in data dataframe
my_data=  data %>%  rename(  Age = age_conf,
                             Sex = sex_conf1,
                             PRS= PRS_conf, 
                             "Mixed Ethnicity" = Ethnic_background_conf2,
                             "Asian Ethnicity" = Ethnic_background_conf3, 
                             "Black Ethnicity" = Ethnic_background_conf4, 
                             "Other Ethnicity" = Ethnic_background_conf5,
                             "Unknown Ethnicity" = Ethnic_background_confUnknown,
                             BMI = BMI_bio,
                             "Tobacco exposure at home" = TobaccoExposure_home_env, 
                             "Tobacco exposure outside" = TobaccoExposure_outside_env,
                             "Nitrogen dioxide in the air" = Nitrogen_dioxide_air_pollution__env,
                             "Nitrogen oxides in the air" = Nitrogen_oxides_air_pollution__env,
                             "Particulate matter at 10pm" = Particulate_matter_air_pollution_pm10__env,
                             "Particulate matter at 2.5pm" = Particulate_matter_air_pollution_pm2.5__env,
                             "Particulate matter 2.5 pm absorbance" = Particulate_matter_air_pollution_pm2.5_absorbance__env, 
                            "Particulate air matter 2.510 um" = Particulate_matter_air_pollution_2.510um__env, 
                            "Traffic intensity on the nearest road"= Traffic_intensity_on_the_nearest_road_env,
                             "Inverse distance to the nearest road" = Inverse_distance_to_the_nearest_road_env, 
                            "Total traffic loads on major roads" = Total_traffic_load_on_major_roads_env, 
                            "Close to major road Unknown1" = Close_to_major_road_env0.2,
                             "Close to major road Unknown2" = Close_to_major_road_env0.4,
                            "Close to major road Unknown3" = Close_to_major_road_env0.6,
                            "Close to major road Unknown4" = Close_to_major_road_env0.8, 
                             "Close to major road Yes" = Close_to_major_road_env1, 
                            "Night time noise pollution" = Average_nighttime_sound_level_of_noise_pollution_env,
                             "Water percentage buffer" = Water_percentage_buffer_m_env,
                            "Domestic garden percentage buffer" = Domestic_garden_percentage_buffer_300m_env, 
                             "Natural environment percentage buffer" = Natural_environment_percentage_buffer_m_env, 
                            "Euclidean distance to coast" = Distance_Euclidean_to_coast_env,
                            "White blood cells count" = White_blood_cell_leukocyte_count_bio,
                             "Red blood cells count" = Red_blood_cell_erythrocyte_count_bio,
                            "Haemoglobin concentration" = Haemoglobin_concentration_bio, 
                            "Haematocrit concentration" = Haematocrit_percentage_bio, 
                             "Mean corpuscular haemoglobin concentration" = Mean_corpuscular_haemoglobin_concentration_bio,
                            "Red blood cells width" = Red_blood_cell_erythrocyte_distribution_width_bio, 
                             "Patelet count" = Red_blood_cell_erythrocyte_distribution_width_bio, 
                            "Platelet crit" = Platelet_crit_bio, 
                            "Platelet width" = Platelet_distribution_width_bio, "Lymphocyte count" = Lymphocyte_count_bio,
                             "Monocyte count" = Monocyte_count_bio, "Neutrophill count" = Neutrophill_count_bio, "Eosinophill count" = Eosinophill_count_bio, "Basophill count" = Basophill_count_bio, "Reticulocyte count" = Reticulocyte_count_bio, 
                             "Immature Reticulocyte Fraction" = Immature_reticulocyte_fraction_bio, "Alkaline Phosphatase Biomarker" = bio_blood_Alkaline_phosphatase, "Alanine aminotransferase Biomarker"= bio_blood_Alanine_aminotransferase,
                             "Urea Biomarker" = bio_blood_Urea, "Calcium Biomarker" = bio_blood_Calcium, "Creatinine Biomarker" = bio_blood_creatinine, "C-reactive protein Biomarker" = bio_blood_Creactive_protein, "Cystatin C Biomarker" = bio_blood_Cystatin_C,
                             "Gamma glutamyltransferase Biomarker" = bio_blood_Gamma_glutamyltransferase, "Glucose Biomarker" = bio_blood_Glucose, "HbA1c Biomarker" = bio_blood_Glycated_haemoglobin_HbA1c,
                            "IGF1 Biomarker" = bio_blood_IGF1, "LDL Biomarker" = bio_blood_LDL_direct, "Lipoprotein A Biomarker" =bio_blood_Lipoprotein_A, "Phosphate Biomarker" = bio_blood_Phosphate, "SHBG Biomarker" = bio_blood_SHBG, "Bilirubin Biomarker" = bio_blood_Total_bilirubin,
                            "Testosterone Biomarker" = bio_blood_Testosterone, "Total protein Biomarker" = bio_blood_Total_protein, "Triglycerides Biomarker" = bio_blood_Triglycerides, "Urate Biomarker"= bio_blood_Urate,
                            "Vitamin D Biomarker" = bio_blood_Vitamin_D, "HDL Biomarker" = bio_blood_HDL_cholesterol, "Townsend deprivation index" = Townsend_deprivation_index_at_recruitment_env, 
                            "Atrial Fibrillation" = com_has_AF1, "Migraine" = com_has_migraine1, "Type 1 diabetes" = com_has_t1d1, "Type 2 diabetes"= com_has_t2d1, "Chronic Kidney Disease" = com_has_ckd1,
                            "Rheumatoid arthritis"= com_has_RA1, "Systemic lupus erythematosus" = com_has_sle1, "Severe Mental Illness" = com_has_psychiatric1, "Atypical Antipsychotic medication" = com_antipsychotic1,
                            "CAD in the family" = com_herit_CAD1, "Erectile dysfunction" = com_has_ED1, "Steroid medication" = com_steroid1, "Blood pressure medication" = com_bp1, 
                            "1-4 hours of physical activity" = physical_activity_level_env2, 
                            "Over 4 hours of physical activity" = physical_activity_level_env3,
                            "Unknown Physical activity" = physical_activity_level_envUnknown,
                            "Maternal Smoking at Birth Yes" = Maternal_smoking_around_birth_env2,
                            "Maternal Smoking at birth Unknown" = Maternal_smoking_around_birth_envUnknown,
                            "Meals with Alcohol Yes" = MealsAndAlc_env2, 
                            "Meals with Alcohol Unknown" = MealsAndAlc_envUnknown, 
                            "Alcohol intake vs previously Same" = Alcohol_intake_versus_10_years_previously_env2, 
                            "Alcohol intake vs previously Less Now" = Alcohol_intake_versus_10_years_previously_env3,
                            "Alcohol intake vs previously Unknown" = Alcohol_intake_versus_10_years_previously_envUnknown, 
                            "Alcohol level 3-4 drinks" = alcohol_level_env2,
                            "Alcohol level 5-6 drinks" = alcohol_level_env3, 
                            "Unknown alcohol level" = alcohol_level_envUnknown, 
                            "Smokers in household Yes" = smokers_in_household_env2,
                            "Smokers in household Unknown" = smokers_in_household_envUnknown,
                            "Past tobacco smoking occasionally" = Past_tobacco_smoking_env2,
                            "Past tobacco smoking once or twice" = Past_tobacco_smoking_env3, 
                            "Past tobacco smoking never"= Past_tobacco_smoking_env4,
                            "Past tobacco smoking Unknown" = Past_tobacco_smoking_envUnknown,
                            "Time spent outdoors in winter 3-8 hours" = time_winter_outdoors_level_env2, 
                            "Time spent outdoors in winter over 8 hours" = time_winter_outdoors_level_env3, 
                            "Time spent outdoors in winter Unknown" = time_winter_outdoors_level_envUnknown,
                            "Time spent outdoors in summer 3-8 hours" = time_summer_outdoors_level_env2, 
                            "Time spent outdoors in summer over 8 hours" = time_summer_outdoors_level_env3,
                            "Time spent outdoors in summer Unknown" = time_summer_outdoors_level_envUnknown, 
                            
                            "Sometimes or Often Varies Diet" = diet_variation_env2, 
                            "Unknown Variation in Diet" = diet_variation_envUnknown,
                            "Current Smoker" = Current_tobacco_smoking_env2, 
                            "Occasional Current Smoker" = Current_tobacco_smoking_env3,
                            "Declined to Answer re: Current Smoker" = Current_tobacco_smoking_envUnknown, 
                            "Previously Smoked" = Smoking_status_env2, 
                            "Ever Smoked Yes" = Ever_smoked_env2,
                            "Never Smoked" = Smoking_status_env3,
                            "Declined to Answer re: Smoking Status" = Smoking_status_envUnknown,
                            "Declined to Answer re: Ever smoked" = Ever_smoked_envUnknown,
                            "Country of Birth: Wales" = Country_of_birth_env2,
                            "Country of Birth: Scotland" = Country_of_birth_env3,
                            "Country of Birth: Northern Ireland" = Country_of_birth_env4,
                            "Country of Birth: Republic of Ireland" = Country_of_birth_env5,
                            "Country of Birth: Outside of British Isles" = Country_of_birth_env6,
                            "Country of Birth Unknown" = Country_of_birth_envUnknown,
                            "Six to Eight Hours Sleep" = sleep_level_env2,
                            "Over Eight Hours Sleep" = sleep_level_env3,
                            "Unknown Hours of Sleep" = sleep_level_envUnknown,
                            "More Than Weekly Visits from Family or Friends" = frequency_visits_env2,
                            "Rare Visits from Family or Friends" = frequency_visits_env3,
                            "Never Recieves Visits from Family or Friends" = frequency_visits_env4,
                            "Unknown Visits from Family or Friends" = frequency_visits_envUnknown,
                            "DASH band 2" = dash_env2,
                            "DASH band 3" = dash_env3,
                            "DASH band 4" = dash_env4,
                            "DASH band 5" = dash_env5,
                            MAP = MAP_bio
                            
                             )
# Reference categories:
# Ethnic background -->  white


colnames(data)


