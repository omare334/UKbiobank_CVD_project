library(gtsummary)
#table one final 
#add the data you want in the table
meme<- cleaned_df%>%
  select(case, sex_conf, age_conf, BMI_bio, Smoking_status_env, MAP_bio, alcohol_level_env, Ethnic_background_bio, PRS_bio)


DT <- meme %>%
  mutate(
    "Sex" = recode(#recode main name and levels of variables
      sex_conf,
      "0" = "Male",
      "1" = "Female"
    ),
    "Smoking status" = recode(
      Smoking_status_env,
      "1" = "Never",
      "2" = "Previous",
      "3" = "Current"
    ),
    "Alcohol levels"= recode(
      alcohol_level_env,
      "1" = "High",
      "2" = "Medium",
      "3" = "Low"
    ),
    "Ethnic background" = recode(
      Ethnic_background_bio,
      "1" = "White",
      "2" = "Mixed",
      "3" = "Asian",
      "4"= "Black",
      "5"="Other"
    ),
    case=recode(
      case,
      "0"= "Control",
      "1"="CVD"
    )
  ) %>%
  select(-c("Smoking_status_env","sex_conf","alcohol_level_env",  "Ethnic_background_bio")) %>%
  tbl_summary (
    by = case,
    label = list(age_conf ~ "Age", BMI_bio~ "Body mass index (kg/m2)", MAP_bio ~ "MAP", PRS_bio~ "PRS"),
    statistic = list(all_continuous() ~ "{mean} ({sd})")
  ) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("**Table 1. Descriptive Statistics**") %>%
  bold_labels()

#print table
DT

#balc ethnicity subanalysis

#table one final 
subby<- sub_group%>%
  select(case, sex_conf, age_conf, BMI_bio, Smoking_status_env, MAP_bio, alcohol_level_env, Ethnic_background_bio, PRS_bio, Townsend_deprivation_index_at_recruitment_env)


DT2 <- subby %>%
  mutate(
    "Sex" = recode(
      sex_conf,
      "0" = "Male",
      "1" = "Female"
    ),
    "Smoking status" = recode(
      Smoking_status_env,
      "1" = "Never",
      "2" = "Previous",
      "3" = "Current"
    ),
    "Alcohol levels"= recode(
      alcohol_level_env,
      "1" = "High",
      "2" = "Medium",
      "3" = "Low"
    ),
    case=recode(
      case,
      "0"= "Control",
      "1"="CVD"
    )
  ) %>%
  select(-c("Smoking_status_env","sex_conf","alcohol_level_env",  "Ethnic_background_bio")) %>%
  tbl_summary (
    by = case,
    label = list(age_conf ~ "Age", BMI_bio~ "Body mass index (kg/m2)", MAP_bio ~ "MAP", PRS_bio~ "PRS"),
    statistic = list(all_continuous() ~ "{mean} ({sd})")
  ) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("**Table 2. Descriptive Statistics for Black ethnic background**") %>%
  bold_labels()
  
 #printtable
DT2
