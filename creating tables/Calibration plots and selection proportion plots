cleaned_df <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/cleaned_df.RDS")

#load evnvironment with saved stability selction results


CalibrationPlot(stability_lasso_alkaline_phosphatase_new)
# Calibrated selection proportions
selprop <- SelectionProportions(stability_lasso_alkaline_phosphatase_new)
# Calibrated parameters
hat_params <- Argmax(stability_lasso_alkaline_phosphatase_new)

selprophe<-selprop
selprophe<- as.data.frame(selprophe)

# Visualisation of selection proportions
par(mar = c(10, 5, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", col = ifelse(selprop >=
                                                               hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = rownames(selprophe)[i],
       las = 2, col = ifelse(selprop[i] >= hat_params[2],
                             yes = "red", no = "grey"), col.axis = ifelse(selprop[i] >=
                                                                            hat_params[2], yes = "red", no = "grey"))
}
# Convert row names to a column
selprophe <- rownames_to_column(selprophe, var = "RowNames")


rownames(selprophe) <- c(  "Tobacco exposure at home",
                           "Tobacco exposure outside",
                           "Nitrogen dioxide in the air",
                           "Nitrogen oxides in the air",
                           "Particulate matter at 10pm",
                           "Particulate matter at 2.5pm",
                           "Particulate matter 2.5 pm absorbance",
                           "Particulate air matter 2.510 um",
                           "Traffic intensity on the nearest road",
                           "Inverse distance to the nearest road",
                           "Total traffic loads on major roads",
                           "Close to major road Unknown1",
                           "Close to major road Unknown2",
                           "Close to major road Unknown3",
                           "Close to major road Unknown4",
                           "Close to major road Yes",
                           "Night time noise pollution",
                           "Water percentage buffer",
                           "Domestic garden percentage buffer",
                           "Natural environment percentage buffer",
                           "Euclidean distance to coast",
                           "Townsend deprvation",
                           "Physical activity level 2",
                           "Physical activity level 3",
                           "Physical activity Unknown",
                           "Maternal Smoking at Birth Yes",
                           "Maternal Smoking at birth Unknown",
                           "Meals with Alcohol Yes",
                           "Meals with Alcohol Unknown",
                           "Alcohol intake vs previously Same",
                           "Alcohol intake vs previously Less Now",
                           "Alcohol intake vs previously Unknown",
                           "Alcohol level 3-4 drinks" ,
                           "Alcohol level 5-6 drinks",
                           "Unknown alcohol level",
                           "Smokers in household Yes",
                           "Smokers in household Unknown",
                           "Past tobacco smoking occasionally",
                           "Past tobacco smoking once or twice",
                           "Past tobacco smoking never",
                           "Past tobacco smoking Unknown",
                           "Time spent outdoors in winter 3-8 hours",
                           "Time spent outdoors in winter over 8 hours",
                           "Time spent outdoors in winter Unknown",
                           "Time spent outdoors in summer 3-8 hours",
                           "Time spent outdoors in summer over 8 hours",
                           "Time spent outdoors in summer Unknown",
                           "More Than Weekly Visits from Family or Friends",
                           "Rare Visits from Family or Friends",
                           "Never Recieves Visits from Family or Friends",
                           "Unknown Visits from Family or Friends",
                           "Six to Eight Hours Sleep",
                           "Over Eight Hours Sleep",
                           "Unknown Hours of Sleep",
                           "Sometimes or Often Varies Diet" ,
                           "Unknown Variation in Diet",
                           "Country of Birth: Wales",
                           "Country of Birth: Scotland",
                           "Country of Birth: Northern Ireland",
                           "Country of Birth: Republic of Ireland",
                           "Country of Birth: Outside of British Isles",
                           "Country of Birth Unknown",
                           "Previously Smoked",
                           "Never Smoked",
                           "Declined to Answer re: Smoking Status",
                           "Ever Smoked Yes",
                           "Declined to Answer re: Ever smoked",
                           "Current Smoker" ,
                           "Occasional Current Smoker",
                           "Declined to Answer re: Current Smoker",
                           "DASH band 2",
                           "DASH band 3",
                           "DASH band 4",
                           "DASH band 5")
                           


###C-recative                    
CalibrationPlot(stability_lasso_C_reactive_new)
# Calibrated selection proportions
selprop <- SelectionProportions(stability_lasso_C_reactive_new)
# Calibrated parameters
hat_params <- Argmax(stability_lasso_C_reactive_new)


# Visualisation of selection proportions
par(mar = c(12, 6, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", col = ifelse(selprop >=
                                                               hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = rownames(selprophe)[i],
       las = 2, col = ifelse(selprop[i] >= hat_params[2],
                             yes = "red", no = "grey"), col.axis = ifelse(selprop[i] >=
                                                                            hat_params[2], yes = "red", no = "grey"))
}

###AF
CalibrationPlot(stability_lasso_com_AF_new)
# Calibrated selection proportions
selprop <- SelectionProportions(stability_lasso_com_AF_new)
# Calibrated parameters
hat_params <- Argmax(stability_lasso_com_AF_new)


# Visualisation of selection proportions
par(mar = c(12, 6, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", col = ifelse(selprop >=
                                                               hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = rownames(selprophe)[i],
       las = 2, col = ifelse(selprop[i] >= hat_params[2],
                             yes = "red", no = "grey"), col.axis = ifelse(selprop[i] >=
                                                                            hat_params[2], yes = "red", no = "grey"))
}
####BP
CalibrationPlot(stability_lasso_com_bp_new)
# Calibrated selection proportions
selprop <- SelectionProportions(stability_lasso_com_bp_new)
# Calibrated parameters
hat_params <- Argmax(stability_lasso_com_bp_new)


# Visualisation of selection proportions
par(mar = c(15, 8, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", col = ifelse(selprop >=
                                                               hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = rownames(selprophe)[i],
       las = 2, col = ifelse(selprop[i] >= hat_params[2],
                             yes = "red", no = "grey"), col.axis = ifelse(selprop[i] >=
                                                                            hat_params[2], yes = "red", no = "grey"))
}

###ckd
CalibrationPlot(stability_lasso_com_ckd_new)
# Calibrated selection proportions
selprop <- SelectionProportions(stability_lasso_com_ckd_new)
# Calibrated parameters
hat_params <- Argmax(stability_lasso_com_ckd_new)


# Visualisation of selection proportions
par(mar = c(15, 8, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", col = ifelse(selprop >=
                                                               hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = rownames(selprophe)[i],
       las = 2, col = ifelse(selprop[i] >= hat_params[2],
                             yes = "red", no = "grey"), col.axis = ifelse(selprop[i] >=
                                                                            hat_params[2], yes = "red", no = "grey"))
}

####migrane

CalibrationPlot(stability_lasso_com_migraine_new)
# Calibrated selection proportions
selprop <- SelectionProportions(stability_lasso_com_migraine_new)
# Calibrated parameters
hat_params <- Argmax(stability_lasso_com_migraine_new)


# Visualisation of selection proportions
par(mar = c(16, 5, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", col = ifelse(selprop >=
                                                               hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = rownames(selprophe)[i],
       las = 2, col = ifelse(selprop[i] >= hat_params[2],
                             yes = "red", no = "grey"), col.axis = ifelse(selprop[i] >=
                                                                            hat_params[2], yes = "red", no = "grey"))
}


###psy

CalibrationPlot(stability_lasso_com_psychiatric_new)
# Calibrated selection proportions
selprop <- SelectionProportions(stability_lasso_com_psychiatric_new)
# Calibrated parameters
hat_params <- Argmax(stability_lasso_com_psychiatric_new)


# Visualisation of selection proportions
par(mar = c(16, 5, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", col = ifelse(selprop >=
                                                               hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = rownames(selprophe)[i],
       las = 2, col = ifelse(selprop[i] >= hat_params[2],
                             yes = "red", no = "grey"), col.axis = ifelse(selprop[i] >=
                                                                            hat_params[2], yes = "red", no = "grey"))
}

###RA

CalibrationPlot(stability_lasso_com_RA_new)
# Calibrated selection proportions
selprop <- SelectionProportions(stability_lasso_com_RA_new)
# Calibrated parameters
hat_params <- Argmax(stability_lasso_com_RA_new)


# Visualisation of selection proportions
par(mar = c(16, 5, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", col = ifelse(selprop >=
                                                               hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = rownames(selprophe)[i],
       las = 2, col = ifelse(selprop[i] >= hat_params[2],
                             yes = "red", no = "grey"), col.axis = ifelse(selprop[i] >=
                                                                            hat_params[2], yes = "red", no = "grey"))
}

###sle

CalibrationPlot(stability_lasso_com_sle_new)
# Calibrated selection proportions
selprop <- SelectionProportions(stability_lasso_com_sle_new)
# Calibrated parameters
hat_params <- Argmax(stability_lasso_com_sle_new)


# Visualisation of selection proportions
par(mar = c(16, 5, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", col = ifelse(selprop >=
                                                               hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = rownames(selprophe)[i],
       las = 2, col = ifelse(selprop[i] >= hat_params[2],
                             yes = "red", no = "grey"), col.axis = ifelse(selprop[i] >=
                                                                            hat_params[2], yes = "red", no = "grey"))
}
###t2d

CalibrationPlot(stability_lasso_com_t2d_new)
# Calibrated selection proportions
selprop <- SelectionProportions(stability_lasso_com_t2d_new)
# Calibrated parameters
hat_params <- Argmax(stability_lasso_com_t2d_new)


# Visualisation of selection proportions
par(mar = c(16, 5, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", col = ifelse(selprop >=
                                                               hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = rownames(selprophe)[i],
       las = 2, col = ifelse(selprop[i] >= hat_params[2],
                             yes = "red", no = "grey"), col.axis = ifelse(selprop[i] >=
                                                                            hat_params[2], yes = "red", no = "grey"))
}


###cystatinc
CalibrationPlot(stability_lasso_Cystatin_C_new)
# Calibrated selection proportions
selprop <- SelectionProportions(stability_lasso_Cystatin_C_new)
# Calibrated parameters
hat_params <- Argmax(stability_lasso_Cystatin_C_new)


# Visualisation of selection proportions
par(mar = c(16, 5, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", col = ifelse(selprop >=
                                                               hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = rownames(selprophe)[i],
       las = 2, col = ifelse(selprop[i] >= hat_params[2],
                             yes = "red", no = "grey"), col.axis = ifelse(selprop[i] >=
                                                                            hat_params[2], yes = "red", no = "grey"))
}

###HDL
CalibrationPlot(stability_lasso_HDL_cholesterol_new)
# Calibrated selection proportions
selprop <- SelectionProportions(stability_lasso_HDL_cholesterol_new)
# Calibrated parameters
hat_params <- Argmax(stability_lasso_HDL_cholesterol_new)


# Visualisation of selection proportions
par(mar = c(16, 5, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", col = ifelse(selprop >=
                                                               hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = rownames(selprophe)[i],
       las = 2, col = ifelse(selprop[i] >= hat_params[2],
                             yes = "red", no = "grey"), col.axis = ifelse(selprop[i] >=
                                                                            hat_params[2], yes = "red", no = "grey"))
}


###LDL
CalibrationPlot(stability_lasso_LDL_direct_new)
# Calibrated selection proportions
selprop <- SelectionProportions(stability_lasso_LDL_direct_new)
# Calibrated parameters
hat_params <- Argmax(stability_lasso_LDL_direct_new)


# Visualisation of selection proportions
par(mar = c(16, 5, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", col = ifelse(selprop >=
                                                               hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = rownames(selprophe)[i],
       las = 2, col = ifelse(selprop[i] >= hat_params[2],
                             yes = "red", no = "grey"), col.axis = ifelse(selprop[i] >=
                                                                            hat_params[2], yes = "red", no = "grey"))
}

###Lipoprotein
CalibrationPlot(stability_lasso_Lipoprotein_A_new)
# Calibrated selection proportions
selprop <- SelectionProportions(stability_lasso_Lipoprotein_A_new)
# Calibrated parameters
hat_params <- Argmax(stability_lasso_Lipoprotein_A_new)


# Visualisation of selection proportions
par(mar = c(16, 5, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", col = ifelse(selprop >=
                                                               hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = rownames(selprophe)[i],
       las = 2, col = ifelse(selprop[i] >= hat_params[2],
                             yes = "red", no = "grey"), col.axis = ifelse(selprop[i] >=
                                                                            hat_params[2], yes = "red", no = "grey"))
}

###MAP
CalibrationPlot(stability_lasso_MAP_new)
# Calibrated selection proportions
selprop <- SelectionProportions(stability_lasso_MAP_new)
# Calibrated parameters
hat_params <- Argmax(stability_lasso_MAP_new)


# Visualisation of selection proportions
par(mar = c(16, 5, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", col = ifelse(selprop >=
                                                               hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = rownames(selprophe)[i],
       las = 2, col = ifelse(selprop[i] >= hat_params[2],
                             yes = "red", no = "grey"), col.axis = ifelse(selprop[i] >=
                                                                            hat_params[2], yes = "red", no = "grey"))
}

###cox
CalibrationPlot(stability_lasso_cox_new)
# Calibrated selection proportions
selprop <- SelectionProportions(stability_lasso_cox_new)
# Calibrated parameters
hat_params <- Argmax(stability_lasso_cox_new)

selpropcox<-as.data.frame(selprop)

rownames(selpropcox) <- c(  "BMI",
                           "Tobacco exposure at home",
                           "Tobacco exposure outside",
                           "Nitrogen dioxide in the air",
                           "Nitrogen oxides in the air",
                           "Particulate matter at 10pm",
                           "Particulate matter at 2.5pm",
                           "Particulate matter 2.5 pm absorbance",
                           "Particulate air matter 2.510 um",
                           "Traffic intensity on the nearest road",
                           "Inverse distance to the nearest road",
                           "Total traffic loads on major roads",
                           "Close to major road Unknown1",
                           "Close to major road Unknown2",
                           "Close to major road Unknown3",
                           "Close to major road Unknown4",
                           "Close to major road Yes",
                           "Night time noise pollution",
                           "Water percentage buffer",
                           "Domestic garden percentage buffer",
                           "Natural environment percentage buffer",
                           "Euclidean distance to coast",
                           "White blood cells count",
                           "Red blood cells count",
                           "Haemoglobin concentration",
                           "Haematocrit concentration",
                           "Mean corpuscular haemoglobin concentration",
                           "Red blood cells width",
                           "Patelet count",
                           "Platelet crit",
                           "Platelet width",
                           "Lymphocyte count",
                           "Monocyte count" ,
                           "Neutrophill count",
                           "Eosinophill count",
                           "Basophill count",
                           "Reticulocyte count",
                           "Immature Reticulocyte Fraction",
                           "Alkaline Phosphatase Biomarker",
                           "Alanine aminotransferase Biomarker",
                           "Urea Biomarker",
                           "Calcium Biomarker",
                           "Creatinine Biomarker" ,
                           "C-reactive protein Biomarker",
                           "Cystatin C Biomarker",
                           "Gamma glutamyltransferase Biomarker",
                           "Glucose Biomarker",
                           "HbA1c Biomarker",
                           "IGF1 Biomarker",
                           "LDL Biomarker",
                           "Lipoprotein A Biomarker",
                           "Phosphate Biomarker",
                           "SHBG Biomarker",
                           "Bilirubin Biomarker",
                           "Testosterone Biomarker",
                           "Total protein Biomarker",
                           "Triglycerides Biomarker",
                           "Urate Biomarker",
                           "Vitamin D Biomarker",
                           "HDL Biomarker",
                           "Townsend deprvation index",
                           "Atrial Fibrillation",
                           "Migraine",
                           "Type 1 diabetes",
                           "Type 2 diabetes",
                           "Chronic Kidney Disease",
                           "Rheumatoid Arthritis",
                           "Systemic lupus erythematosus",
                           "Severe Mental Illness",
                           "Erectile dysfunction",
                           "CAD in the family" ,
                           "Atypical Antipsychotic medication",
                           "Steroid medication",
                           "Blood pressure medication",
                           "Physical activity level 2",
                           "Physical activity level 3",
                           "Physical activity Unknown",
                           "Maternal Smoking at Birth Yes",
                           "Maternal Smoking at birth Unknown",
                           "Meals with Alcohol Yes",
                           "Meals with Alcohol Unknown",
                           "Alcohol intake vs previously Same",
                           "Alcohol intake vs previously Less Now",
                           "Alcohol intake vs previously Unknown",
                           "Alcohol level 3-4 drinks" ,
                           "Alcohol level 5-6 drinks",
                           "Unknown alcohol level",
                           "Smokers in household Yes",
                           "Smokers in household Unknown",
                           "Past tobacco smoking occasionally",
                           "Past tobacco smoking once or twice",
                           "Past tobacco smoking never",
                           "Past tobacco smoking Unknown",
                           "Time spent outdoors in winter 3-8 hours",
                           "Time spent outdoors in winter over 8 hours",
                           "Time spent outdoors in winter Unknown",
                           "Time spent outdoors in summer 3-8 hours",
                           "Time spent outdoors in summer over 8 hours",
                           "Time spent outdoors in summer Unknown",
                           "More Than Weekly Visits from Family or Friends",
                           "Rare Visits from Family or Friends",
                           "Never Recieves Visits from Family or Friends",
                           "Unknown Visits from Family or Friends",
                           "Six to Eight Hours Sleep",
                           "Over Eight Hours Sleep",
                           "Unknown Hours of Sleep",
                           "Sometimes or Often Varies Diet" ,
                           "Unknown Variation in Diet",
                           "Country of Birth: Wales",
                           "Country of Birth: Scotland",
                           "Country of Birth: Northern Ireland",
                           "Country of Birth: Republic of Ireland",
                           "Country of Birth: Outside of British Isles",
                           "Country of Birth Unknown",
                           "Previously Smoked",
                           "Never Smoked",
                           "Declined to Answer re: Smoking Status",
                           "Ever Smoked Yes",
                           "Declined to Answer re: Ever smoked",
                           "Current Smoker" ,
                           "Occasional Current Smoker",
                           "Declined to Answer re: Current Smoker",
                           "MAP",
                           "DASH band 2",
                           "DASH band 3",
                           "DASH band 4",
                           "DASH band 5",
                           "left")

# Visualisation of selection proportions
par(mar = c(18, 8, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", col = ifelse(selprop >=
                                                               hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = rownames(selpropcox)[i],
       las = 2, col = ifelse(selprop[i] >= hat_params[2],
                             yes = "red", no = "grey"), col.axis = ifelse(selprop[i] >=
                                                                            hat_params[2], yes = "red", no = "grey"))
}

