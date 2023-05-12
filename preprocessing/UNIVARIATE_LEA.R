library(caret)
library(dplyr)
library(ggrepel)
library(ggplot2)

################ N.B. Run RENAMING_COLUMNS.R in LEA_Workspace before running this code. #######

# Step 2: load the data set and the train test split workspace - prepare the data
cleaned_df <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/cleaned_df.RDS")

# Splitting the data into training and testing
#load("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/workspaces/Train_test_workspace.RData")

load("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/R_Scripts/Sam_Scripts/Univar_cox/my_data_RENAMING_COLUMNS.RData")


# data is ready 
# preparing the data for cox

######################

data = my_data

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


#load("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/calibrated_new_lasso/stability_cox_pfer.RData")

data = df_final

univar <- data

univar<- univar%>%
  select(-c(time_to_diagnosis, left_the_study,years_to_lost_to_follow_up, years_to_death, years_to_diagnosis, Date_lost_to_follow_up))


# install.packages("survival")
library(survival)

univar <- as.data.frame(univar)



univar$case <- as.numeric(univar$case)

univar$case <- ifelse(univar$case == 1, 0, 1)

names_cox = colnames(univar)
# Replace variable names with underscores
colnames(univar) <- gsub("[ |:|-|.]","_", colnames(univar))
colnames(univar) <- gsub("-","_", colnames(univar))

# Fit Cox models
cox <- lapply(colnames(univar)[!names(univar) %in% c("eid", "time_to_event", "case", "Sex", "Age", "PRS", "Mixed_Ethnicity", "Other_Ethnicity", "Asian_Ethnicity",                                                     "Black_Ethnicity", "Unknown_Ethnicity")],
              function(var) {
                formula <- as.formula(paste("Surv(time_to_event, case) ~ PRS + Sex + Age +Mixed_Ethnicity + Other_Ethnicity + Asian_Ethnicity+ Black_Ethnicity + Unknown_Ethnicity +", var))
                res.cox <- survival::coxph(formula, data = univar)
                return(res.cox)
              })

#model = coxph(formula = Surv(time_to_event, case) ~ PRS + Sex + Age +Mixed_Ethnicity + Other_Ethnicity + Asian_Ethnicity+ Black_Ethnicity + Unknown_Ethnicity + DASH_band_2, data = univar)
#summary(model)$coefficients[9,5]


# 
# # Fit Cox regression models
# cox <- lapply(cols, function(var) {
#   print(var)
#   formula <- as.formula(paste("Surv(time_to_event, case) ~ PRS + Sex + Age +
# Mixed_Ethnicity + Other_Ethnicity + Asian_Ethnicity
# + Black_Ethnicity + Unknown_Ethnicity+", var))
#   print(formula) # Check formula before running coxph()
#   res.cox <- coxph(formula, data = univar)
#   return(res.cox)
# })
# 
# formula <- as.formula(paste("Surv(time_to_event, case) ~ PRS + Sex + Age +Mixed_Ethnicity + Other_Ethnicity + Asian_Ethnicity + Black_Ethnicity + Unknown_Ethnicity+ BMI"))
# res.cox <- coxph(formula, data = univar)

# hazard ratios
hr <- vector("numeric", 0)
for (i in 1:length(cox)) {
  hr[i] <- exp(coef(cox[[i]]))
}

hr

# p-values
pvalues <- vector("numeric", 0)
for (i in 1:length(cox)) {
  pvalues[i] <- summary(cox[[i]])$coefficients[9, 5]
}

pvalues

# standard error
se <- vector("numeric", 0)
for (i in 1:length(cox)) {
  se[i] <- summary(cox[[i]])$coefficients[9, 2]
}

se


names_cox2 <- names_cox[!names_cox %in% c("eid", "time_to_event", "case", "Sex", "Age", "PRS",
                                                 "Mixed Ethnicity", "Other Ethnicity","Asian Ethnicity",
                                                 "Black Ethnicity","Unknown Ethnicity")]

# create a data frame with the results
results_df_cox <- data.frame(hr, pvalues, se, variable = names_cox2)

# set significance level
sig_level <- 0.05
bonferroni <- sig_level/nrow(results_df_cox)

# identify variables with p-values above the Bonferroni threshold
above_threshold <- which(-log10(pvalues) > -log10(bonferroni))

# extract names of variables
variable_names <- names_cox[above_threshold]

path <- "/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/R_Scripts/Sam_Scripts/Univar_cox/variable_names.txt"
write.table(variable_names, file = path, col.names = FALSE, row.names = FALSE, quote = FALSE)

results_df_cox <- results_df_cox%>%
  mutate(type = "")


results_df_cox$type[1] <- "Biological"
results_df_cox$type[2:22] <- "Environment"
results_df_cox$type[23:60] <- "Biological"
results_df_cox$type[61] <- "Environment"
results_df_cox$type[62:74] <- "Comorbidities"
results_df_cox$type[75:122] <- "Environment"
results_df_cox$type[123] <- "Biological"
results_df_cox$type[124:127] <- "Environment"

# Manhattan plot of CVD risk

# # Saving extracted dataset
ggsave("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/R_Scripts/Sam_Scripts/Univar_cox/Manhattan_plot.png")


Manhattan_plot <- ggplot(data=results_df_cox,aes(x= names_cox2, y= -log10(pvalues), colour = type))+
  geom_point()+
  geom_hline(yintercept = -log10(bonferroni), linetype="dashed", col="red3")+
  geom_hline(yintercept = -log10(0.05), linetype="dashed", col="royalblue4") +
  annotate("text",x=15,y=3.4,color="red3",label="Bonferroni",size=3)+
  theme_classic() +
  labs(title="Manhattan plot of CVD risk",
       x="Variable", y="-log 10(p-values)")+
  geom_text_repel(aes(label=ifelse(pvalues<bonferroni,names_cox2,"")),size=2.5)+
  theme(axis.text.x=element_blank())

Manhattan_plot
