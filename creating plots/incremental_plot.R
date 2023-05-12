# Incremental plot Sam

# x_data =  matrix of predictors with observations as rows and variables as columns.
# optional vector or matrix of outcome(s). If family is set to "binomial" or "multinomial", ydata can be a vector with character/numeric values or a fac- tor.

load("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/2_stability_cox_for2ndlayer.RData")

#cases <- predicting$case

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

cutoff= 30

# 
# par(mar = c(5, 4, 4, 2), las = 2, cex.axis = 0.8)  # Adjust margins, rotate axis labels, and reduce font size
# plot(inc, pch = 19, col = "dark blue", lty = 2, col.lab = "black", col.axis = "black")  # Customize symbol, color, line type, and label color
# title(main = "Incremental Plot")  # Add title and axis labels
# grid()  # Add gridlines
# abline(v = cutoff +0.25, col = "red", lty = 2)  # Add vertical dashed red line
# 
# # Add horizontal grey line and label
# abline(h = 0.73, col = "grey", lwd = 2)
# text(x = par("usr")[2] - 0.05, y = 0.73 - 0.01, "QRISK3 C-Index 0.73", adj = c(1, 0))

# # Add title next to the vertical line
# 
# text(x = cutoff - 3.5, y = par("usr")[4] - 0.005, "Cox LASSO C-Statistic 0.77", adj = c(0.5, 1), col = "red")

# Start the PDF device
png("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/Uncalibrated_FINAL/Sam_Incremental_plot/Incremental_plot.png")

plot.new()
# Set margins and axis label rotation
par(mar = c(5, 4, 4, 2), las = 2, cex.axis = 0.8)  

# Draw the plot
plot(inc, pch = 19, col = "dark blue", lty = 2, col.lab = "black", col.axis = "black")
title(main = "Incremental Plot") 
grid()  
abline(v = cutoff +0.25, col = "red", lty = 2)  
abline(h = 0.73, col = "grey", lwd = 2)
text(x = par("usr")[2] - 0.1, y = 0.73 - 0.01, "QRISK3 C-Index 0.73", adj = c(1, 0))
text(x = cutoff - 7, y = par("usr")[4] - 0.0025, "Cox LASSO C-Statistic 0.77", adj = c(0.5, 1), col = "red")

# Close the PDF device
dev.off()
