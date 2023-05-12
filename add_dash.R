# Adding the dash score column to the dataframe

Diet_data <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Diet_data.RDS")

Diet_data = Diet_data %>% select (-c("fruit", "lowfat.dairy", "processed.red.meat", "vegetables", "whole.grain"))

cleaned_df = Diet_data


#data$physical_activity_level = data$physical_activity_level_env
cleaned_df <- rename(cleaned_df, PRS_bio = PRS, physical_activity_level_env = physical_activity_level,
               BMI_bio = 'Body_mass_index_(BMI)_env.0.0',
               age_conf = Age_at_recruitment_env.0.0, sex_conf =Sex.0.0, 
               alcohol_level_env = alcohol_level,smokers_in_household_env = smokers_in_household,
               MAP_bio = MAP, Ethnic_background_bio = Ethnic_background, 
               diet_variation_env = diet_variation,  sleep_level_env =sleep_level,
               frequency_visits_env = frequency_visits,  
               time_summer_outdoors_level_env = time_summer_outdoors_level, 
               time_winter_outdoors_level_env= time_winter_outdoors_level, 
               Past_tobacco_smoking_env = Past_tobacco_smoking.0.0,  
               smokers_in_household_env = smokers_in_household, dash_env = dash)

names(cleaned_df) <- gsub(".0.0", "", names(cleaned_df))
names(cleaned_df) <- gsub(";|,", "", names(cleaned_df))
saveRDS(cleaned_df, file ="/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/cleaned_df.RDS" )
