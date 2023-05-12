# x_data =  matrix of predictors with observations as rows and variables as columns.
# optional vector or matrix of outcome(s). If family is set to "binomial" or "multinomial", ydata can be a vector with character/numeric values or a fac- tor.

load("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/2_stability_cox_for2ndlayer.RData")

#cases <- predicting$case
#incremental plot 
# uninstall package "fake"
#remove.packages("fake")
#install.packages("fake")

# Detach the package
#detach(package:sharp)
#detach(package:fake)

#uninstall.packages("fake")
# Reload the package
library(sharp)
library(fake)
library(plotrix)

#stability_lasso_cox_new

inc <- Incremental(xdata = cox_prediction , ydata = CVD.surv_predicting,
                   stability = stability_lasso_cox_new, n_predictors = 50, family = "cox")

#install.packages("plotrix")
library(plotrix)
graphics.off()
dev.off() 
plot.new()

plot(inc)

# Save the 'inc' object to a file
saveRDS(inc, file = "/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/Sam_Incremental_plot/inc.rds")
