selected_list_new
#creating heatmaps of the list library(tidyr)
library(dplyr)
#trying anothe dataframe 
all_vars <- unique(unlist(selected_list_new))
df <- data.frame(variable_name = all_vars, stringsAsFactors = FALSE)

for (i in 1:length(selected_list_new)) {
  list_name <- names(selected_list_new)[i]
  df[[list_name]] <- df$variable_name %in% selected_list_new[[list_name]]
}
df <- df %>% filter(!grepl("conf", variable_name))
#create the plot 
df_long <- df %>%
  pivot_longer(cols = -variable_name, names_to = "list_name", values_to = "selected")
#creating heat map
ggplot(df_long, aes(x = list_name, y = variable_name, fill = selected)) +
  geom_tile(color = "black", size = 0.5) +
  scale_fill_manual(values = c("white", "red")) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
###



