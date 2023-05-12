
### Heatmap
install.packages("corrr")
library(corrr)


install.packages("pheatmap")

library(pheatmap)
packageVersion("pheatmap")
#correlation <- cor(my_data[, -which(names(my_data) == "eid")])
# 
numeric_data <- my_data[, sapply(my_data, is.numeric)]
# correlation <- cor(numeric_data)
# 

# Find variables with zero standard deviation
zero_sd_vars <- names(numeric_data)[apply(numeric_data, 2, sd) == 0]

# Exclude variables with zero standard deviation
filtered_data <- numeric_data[, !(names(numeric_data) %in% zero_sd_vars)]

# Calculate the correlation matrix
correlation <- cor(filtered_data)



pheatmap(
  correlation,
  color = colorRampPalette(c("navy", "white", "firebrick3"))(100),
  main = "Correlation Heatmap of Outcome Variables",
  border_color = NA,
  xlab = "Outcome Variables",
  ylab = "Outcome Variables",
  fontsize = 3.5,
  fontfamily = "Helvetica",
  legend = FALSE,
  margins = c(10, 10),
  border = NA,
  treeheight_row = 0,
  treeheight_col = 0
)

# Create the correlation heatmap
pheatmap(correlation,
         color = colorRampPalette(c("navy", "white", "firebrick3"))(100),
         main = "Correlation Heatmap of Outcome Variables",
         border_color = NA,
         xlab = "Outcome Variables",
         ylab = "Outcome Variables",
         fontsize = 10,
         fontfamily = "Helvetica",
         legend = FALSE,
         margins = c(10, 10))

# Create the correlation heatmap with smaller axis labels
pheatmap(correlation,
         color = colorRampPalette(c("navy", "white", "firebrick3"))(100),
         main = "Correlation Heatmap of Outcome Variables",
         border_color = NA,
         xlab = "Outcome Variables",
         ylab = "Outcome Variables",
         fontsize = 4,  # Adjust the font size as desired
         fontfamily = "Helvetica",
         legend = FALSE,
         margins = c(10, 10),
         angle_col = 45,  # Adjust the rotation angle of column labels
         angle_row = 45,
         border = NA)  # Adjust the rotation angle of row labels


# Create the correlation heatmap without arrows/lines on the top and left side
pheatmap(correlation,
         color = colorRampPalette(c("navy", "white", "firebrick3"))(100),
         main = "Correlation Heatmap of Outcome Variables",
         border_color = NA,
         xlab = "Outcome Variables",
         ylab = "Outcome Variables",
         fontsize = 4,
         fontfamily = "Helvetica",
         legend = FALSE,
         margins = c(10, 10),
         angle_col = 45,
         angle_row = 45)  # Remove arrows/lines on the top and left side






###

# Create the correlation heatmap without connecting lines
# Create the correlation heatmap without dendrograms
pheatmap(correlation,
         color = colorRampPalette(c("navy", "white", "firebrick3"))(100),
         main = "Correlation Heatmap of Outcome Variables",
         border_color = NA,
         xlab = "Outcome Variables",
         ylab = "Outcome Variables",
         fontsize = 3.5,
         fontfamily = "Helvetica",
         legend = FALSE,
         margins = c(10, 10),
         border = NA,
         treeheight_row = 0,
         treeheight_col = 0)





#########
library(dplyr)
df <- my_data
df$type <- my_data%>%
  mutate(type = 0)

df$type[1] <- "Biological"
df$type[2:22] <- "Environment"
df$type[23:60] <- "Biological"
df$type[61] <- "Environment"
df$type[61:74] <- "Comorbidities"
df$type[75:121] <- "Environment"
df$type[122] <- "Biological"
df$type[123:126] <- "Environment"



# Define a vector of colors that maps to the 'type' column in your data frame
color_vector <- c("Biological" = "blue", "Environment" = "green", "Comorbidities" = "red")

# Create a data frame for annotation
annotation_df <- data.frame(Type = factor(df$type))

# Create a list of colors for annotation
annotation_colors <- list(Type = color_vector)

#Generate the heatmap
pheatmap(correlation,
         color = colorRampPalette(c("navy", "white", "firebrick3"))(100),
         main = "Correlation Heatmap of Outcome Variables",
         border_color = NA,
         xlab = "Outcome Variables",
         ylab = "Outcome Variables",
         fontsize = 3.5,
         fontfamily = "Helvetica",
         legend = FALSE,
         margins = c(10, 10),
         border = NA,
         treeheight_row = 0,
         treeheight_col = 0,
         annotation_col = annotation_df, # Add annotation here
         annotation_colors = annotation_colors) # Add annotation colors here





### Colouring variables names

# Define a vector of colors that maps to the 'type' column in your data frame
color_vector <- c("Biological" = "blue", "Environment" = "green", "Comorbidities" = "red")

# Get the colors for your variables
var_colors <- color_vector[results_df_cox$type]

# Generate the heatmap without variable names
heatmap <- pheatmap(correlation,
                    color = colorRampPalette(c("navy", "white", "firebrick3"))(100),
                    main = "Correlation Heatmap of Outcome Variables",
                    border_color = NA,
                    xlab = "Outcome Variables",
                    ylab = "Outcome Variables",
                    fontsize = 3.5,
                    fontfamily = "Helvetica",
                    legend = FALSE,
                    margins = c(10, 10),
                    border = NA,
                    treeheight_row = 0,
                    treeheight_col = 0,
                    show_colnames = FALSE, # Don't show variable names
                    show_rownames = FALSE) # Don't show variable names

# Add the colored variable names
text(x = 1:length(correlation), y = rep(0, length(correlation)), labels = names(correlation), col = var_colors, srt = 45, adj = c(1,1), xpd = TRUE, cex = 0.8)
text(x = rep(0, length(correlation)), y = 1:length(correlation), labels = names(correlation), col = var_colors, srt = 45, adj = c(1,1), xpd = TRUE, cex = 0.8)


install.packages("grid")
library(grid)

packageVersion("grid")


install.packages("png")
library(png)


png("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/heatmap.png", width = 800, height = 800)

heatmap <- pheatmap(correlation,
                    color = colorRampPalette(c("navy", "white", "firebrick3"))(100),
                    main = "Correlation Heatmap of Outcome Variables",
                    border_color = NA,
                    xlab = "Outcome Variables",
                    ylab = "Outcome Variables",
                    fontsize = 3.5,
                    fontfamily = "Helvetica",
                    legend = FALSE,
                    margins = c(10, 10),
                    border = NA,
                    treeheight_row = 0,
                    treeheight_col = 0,
                    show_colnames = FALSE, # Don't show variable names
                    show_rownames = FALSE) # Don't show variable names
dev.off()

png("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/heatmap_with_labels.png", width = 800, height = 800)
grid.raster(readPNG("/rds/general/project/hda-22-23/live/TDS/Group3_Temporary/Lea_Workspace/heatmap.png"))
grid.text(labels = names(correlation),
          x = unit(0.5, "npc") + unit(0.5 * sin(45 * pi / 180), "npc") * seq_along(correlation),
          y = unit(0.5, "npc") - unit(0.5 * cos(45 * pi / 180), "npc") * seq_along(correlation),
          gp = gpar(col = var_colors, fontfamily = "Helvetica", fontsize = 3.5 * 0.8),
          rot = 45,
          just = c("right", "bottom"))
dev.off()

############################################

# Define a custom heatmap function
custom_heatmap <- function(correlation, colors) {
  # Draw the heatmap
  image(1:nrow(correlation), 1:ncol(correlation), t(correlation), axes = FALSE, col = colorRampPalette(c("navy", "white", "firebrick3"))(100))
  
  # Add the color bar
  legend("topright", legend = c("1", "0", "-1"), fill = c("firebrick3", "white", "navy"), title = "Correlation")
  
  # Add the variable names with different colors on the x axis
  for (i in 1:length(colors)) {
    text(i, -0.5, labels = names(correlation)[i], col = colors[i], srt = 45, adj = c(1,1), xpd = TRUE, cex = 0.8)
  }
  
  # Add the variable names with different colors on the y axis
  for (i in 1:length(colors)) {
    text(-0.5, i, labels = names(correlation)[i], col = colors[i], srt = 45, adj = c(1,1), xpd = TRUE, cex = 0.8)
  }
}

# Define a vector of colors that maps to the 'type' column in your data frame
color_vector <- c("Biological" = "blue", "Environment" = "green", "Comorbidities" = "red")

# Get the colors for your variables
var_colors <- color_vector[results_df_cox$type]

# Generate the custom heatmap
i <- custom_heatmap(correlation, var_colors)
install.packages("ComplexHeatmap")
library(ComplexHeatmap)

