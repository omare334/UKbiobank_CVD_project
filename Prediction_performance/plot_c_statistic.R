#getting c statistic for each 
## qrisk3 
c_QRISK <- ExplanatoryPerformance(xdata = cox_refit_QRISK, ydata = CVD.surv_refitting_QRISK,
                                        new_xdata = cox_prediction_QRISK, new_ydata = CVD.surv_predicting_QRISK,
                                        ij_method = TRUE,
                                        stability = stability_lasso_cox_QRISK, family = "cox")
#stability selected 
c_stab = ExplanatoryPerformance(cox_refit,CVD.surv_refitting,new_xdata = cox_prediction,new_ydata = CVD.surv_predicting
                           , stability = stability_lasso_cox_new , ij_method = TRUE , K = 20, family = "cox",
                           time = 10,verbose = TRUE )
#confounders
c_conf <- ExplanatoryPerformance(xdata = cox_refit_conf, ydata = CVD.surv_refitting_conf,
                                       new_xdata = cox_prediction_conf, new_ydata = CVD.surv_predicting_conf, 
                                 ij_method = TRUE,
                                       stability = stability_lasso_cox_conf, family = "cox")
#putting all of them in one plot 
# Plot all models in one plot
# Extract C-index and bounds from each model's output
c_QRISK_data <- c_QRISK$concordance
c_QRISK_upper <- c_QRISK$upper
c_QRISK_lower <- c_QRISK$lower

c_stab_data <- c_stab$concordance
c_stab_upper <- c_stab$upper
c_stab_lower <- c_stab$lower

c_conf_data <- c_conf$concordance
c_conf_upper <- c_conf$upper
c_conf_lower <- c_conf$lower

# First, create a data frame with your data
data <- data.frame(Model = c("QRISK", "Stability Selected", "Confounders"),
                   ConcordanceIndex = c(c_QRISK_data, c_stab_data, c_conf_data),
                   Upper = c(c_QRISK_upper, c_stab_upper, c_conf_upper),
                   Lower = c(c_QRISK_lower, c_stab_lower, c_conf_lower))

# Load the ggplot2 package
library(ggplot2)

# Create the plot
ggplot(data, aes(x = Model, y = ConcordanceIndex)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  labs(x = "Model", y = "Concordance Index", title = "Concordance Index for Three Models")
ggplot(data, aes(x = Model, y = ConcordanceIndex)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  geom_text(aes(label = round(ConcordanceIndex, 3)), vjust = 1.5, hjust = 1.5) +
  labs(x = "Model", y = "Concordance Index", title = "Concordance Index plot") +
  theme(axis.text.x = element_text(size = 14, face = "bold"))


