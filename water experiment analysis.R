# 96-well plate per row
# Code to load plate reader data from csv file.  

# Create a data frame with the provided growth rates, strains, and conditions
data_growth_water_GR <- data.frame(
  GrowthRate = c(
    0.0628, 0.0567, 0.222, 0.0336, 0.169,
    0.275, 0.0409, 0.104, 0.217, 0.317, # HPLC water
    0.0469, 0.0453, 0.0506, 0.0852, 0.134,
    0.2, 0.0523, 0.0571, 0.233, 0.219, # MBL-N
    0.058, 0.0974, 0.219, 0.0733, 0.169, 0.261,
    0.0619, 0.0798, 0.261, 0.269 #filtered miliQ
  ),
  Strain = c(
    "1A01", "3D05", "B3M02", "3B05", "A2M03", "4H09", "3F01", "4A10", "4D01", "12B01", 
    "1A01", "3D05", "B3M02", "3B05", "A2M03", "4H09", "3F01", "4A10", "4D01", "12B01", 
    "1A01", "3D05", "B3M02", "3B05", "A2M03", "4H09", "3F01", "4A10", "4D01", "12B01"
  ),
  Condition = c(
    "HPLC", "HPLC", "HPLC", "HPLC", 
    "HPLC", "HPLC", "HPLC", "HPLC", 
    "HPLC", "HPLC","Mili-Q", "Mili-Q", 
    "Mili-Q", "Mili-Q", "Mili-Q", "Mili-Q", "Mili-Q", "Mili-Q", "Mili-Q", "Mili-Q",
    "Filtered mili-Q", "Filtered mili-Q", "Filtered mili-Q", 
    "Filtered mili-Q", "Filtered mili-Q", "Filtered mili-Q", 
    "Filtered mili-Q", "Filtered mili-Q",
    "Filtered mili-Q", "Filtered mili-Q"
  )
)

# Plot the growth rate distribution
ggplot(data_growth_water_GR, aes(x = Condition, y = GrowthRate)) +
  geom_boxplot() +
  labs(title = "Growth Rates no-N control Different Water Conditions", x = "Condition", y = "Growth Rate") +
  theme_minimal()

# Load max OD data water experiments
data_growth_water_MAXOD <- data.frame(
  MAXOD = c(
    0.284
    ,0.343
    ,0.613
    ,0.291
    ,0.179
    ,0.887
    ,0.257
    ,0.23
    ,0.255
    ,0.56
    ,0.312
    ,0.368
    ,0.347
    ,0.373
    ,0.174
    ,0.678
    ,0.269
    ,0.307
    ,0.382
    ,0.735
    ,0.368
    ,0.504
    ,0.711
    ,0.579
    ,0.219
    ,0.622
    ,0.282
    ,0.439
    ,0.631
    ,0.855
  ),
  Strain = c(
    "1A01", "3D05", "B3M02", "3B05", "A2M03", "4H09", "3F01", "4A10", "4D01", "12B01", 
     "1A01", "3D05", "B3M02", "3B05", "A2M03", "4H09", "3F01", "4A10", "4D01", 
    "12B01", "1A01", "3D05", "B3M02", "3B05", "A2M03", "4H09", "3F01", "4A10", 
    "4D01", "12B01"
  ),
  Condition = c(
    "HPLC", "HPLC", "HPLC", "HPLC", 
    "HPLC", "HPLC", "HPLC", "HPLC", 
    "HPLC", "HPLC","Mili-Q", "Mili-Q", 
    "Mili-Q", "Mili-Q", "Mili-Q", "Mili-Q", "Mili-Q", "Mili-Q", "Mili-Q", "Mili-Q",
    "Filtered mili-Q", "Filtered mili-Q", "Filtered mili-Q", 
    "Filtered mili-Q", "Filtered mili-Q", "Filtered mili-Q", 
    "Filtered mili-Q", "Filtered mili-Q",
    "Filtered mili-Q", "Filtered mili-Q"
  )
)

# Create the grouped bar plot
ggplot(data_growth_water_MAXOD, aes(x = Strain, y = MAXOD, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.8) +
  scale_fill_brewer(palette = "Set1") + # Use a color palette for better distinction
  labs(
    title = "Yield per Strain Different Water Conditions",
    x = "Strain",
    y = "Yield",
    fill = "Condition"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

# Plot boxplot for maxOD for each condition
ggplot(data_growth_water_MAXOD, aes(x = Condition, y = MAXOD)) +
  geom_boxplot() +
  labs(title = "Boxplot of yields by condition", x = "Condition", y = "Yield") +
  theme_minimal()

# Plot them as points for each strain
gradient_colors <- colorRampPalette(c("blue", "#FDE725FF"))(length(unique(data_growth_water_GR$Condition)))
ggplot(data_growth_water_GR, aes(x = Strain, y = GrowthRate, color = Condition)) +
  geom_point(size = 3) +
  scale_color_manual(values = gradient_colors) +  # Apply gradient colors
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Growth Rates by Strain and Condition", x = "Strain", y = "Growth Rate") +
  theme_minimal()

# Filter data for each condition
data_HPLC <- subset(data_growth_water_GR, Condition == "HPLC")
data_N <- subset(data_growth_water_GR, Condition == "Mili-Q")
data_filtered <- subset(data_growth_water_GR, Condition == "Filtered mili-Q")

# Create separate dataframes for pairwise comparisons
comparison_1 <- merge(data_HPLC, data_N, by = "Strain", suffixes = c("_HPLC", "_N"))
comparison_2 <- merge(data_HPLC, data_filtered, by = "Strain", suffixes = c("_HPLC", "_Filtered"))
comparison_3 <- merge(data_N, data_filtered, by = "Strain", suffixes = c("_N", "_Filtered"))

# Add a new column for identifying the comparison in each dataset
comparison_1$comparison <- "MBL -N HPLC water vs MBL -N"
comparison_2$comparison <- "MBL -N HPLC water vs MBL -N filtered mili-Q"
comparison_3$comparison <- "MBL -N vs MBL -N filtered mili-Q"

# Pick columns to remove
comparison_3 <- comparison_3[, c(-3,-5)]
comparison_1 <- comparison_1[, c(-3,-5)]
comparison_2 <- comparison_2[, c(-3,-5)]

# Change column names
colnames(comparison_2) <- c('Strain', 'GR1', 'GR2', 'comparison')
colnames(comparison_1) <- c('Strain', 'GR1', 'GR2', 'comparison')
colnames(comparison_3) <- c('Strain', 'GR1', 'GR2', 'comparison')

# Combine all three dataframes into one
combined_data <- rbind(comparison_1, comparison_2, comparison_3)

# Pick colors
gradient_colors <- colorRampPalette(c("blue", "#FDE725FF"))(length(unique(combined_data$comparison)))
gradient_colors <- brewer.pal(3, "Set1")  # Set3 has distinct, easily distinguishable colors

# Plot with strain names next to the dots
ggplot(combined_data, aes(x = GR1, y = GR2, color = comparison, group = comparison)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  geom_text(aes(label = Strain), hjust = 0, vjust = -0.5, size = 2, color = 'black') +  # Add strain names
  scale_color_manual(values = gradient_colors) +  # Apply gradient colors
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Comparison of growth rates between water conditions",
       x = "Growth Rate (Condition 1)",
       y = "Growth Rate (Condition 2)") +
  theme_minimal() +
  theme(legend.position = "bottom", # Adjust legend position
        legend.title = element_text(size = 7), # Adjust legend title size
        legend.text = element_text(size = 5.5)) # Adjust legend text size

# Filter data for each condition
data_HPLC_MAXOD <- subset(data_growth_water_MAXOD, Condition == "HPLC")
data_N_MAXOD <- subset(data_growth_water_MAXOD, Condition == "Mili-Q")
data_filtered_MAXOD <- subset(data_growth_water_MAXOD, Condition == "Filtered mili-Q")

# Create separate dataframes for pairwise comparisons
comparison_1_MAXOD <- merge(data_HPLC_MAXOD, data_N_MAXOD, by = "Strain", suffixes = c("_HPLC", "_N"))
comparison_2_MAXOD <- merge(data_HPLC_MAXOD, data_filtered_MAXOD, by = "Strain", suffixes = c("_HPLC", "_Filtered"))
comparison_3_MAXOD <- merge(data_N_MAXOD, data_filtered_MAXOD, by = "Strain", suffixes = c("_N", "_Filtered"))

# Add a new column for identifying the comparison in each dataset
comparison_1_MAXOD$comparison <- "MBL -N HPLC water vs MBL -N"
comparison_2_MAXOD$comparison <- "MBL -N HPLC water vs MBL -N filtered mili-Q"
comparison_3_MAXOD$comparison <- "MBL -N vs MBL -N filtered mili-Q"

# Remove columns
comparison_3_MAXOD <- comparison_3_MAXOD[, c(-3,-5)]
comparison_1_MAXOD <- comparison_1_MAXOD[, c(-3,-5)]
comparison_2_MAXOD <- comparison_2_MAXOD[, c(-3,-5)]

# Change column names
colnames(comparison_2_MAXOD) <- c('Strain', 'OD1', 'OD2', 'comparison')
colnames(comparison_1_MAXOD) <- c('Strain', 'OD1', 'OD2', 'comparison')
colnames(comparison_3_MAXOD) <- c('Strain', 'OD1', 'OD2', 'comparison')

# Combine all three dataframes into one
combined_data_MAXOD <- rbind(comparison_1_MAXOD, comparison_2_MAXOD, comparison_3_MAXOD)
gradient_colors <- colorRampPalette(c("blue", "#FDE725FF"))(length(unique(combined_data_MAXOD$comparison)))

# Plot with strain names next to the dots
ggplot(combined_data_MAXOD, aes(x = OD1, y = OD2, color = comparison, group = comparison)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  geom_text(aes(label = Strain), hjust = 0, vjust = -0.5, size = 2, color = 'black') +  # Add strain names
  scale_color_manual(values = gradient_colors) +  # Apply gradient colors
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Comparison of max OD between water conditions",
       x = "Yield (Condition 1)",
       y = "Yield (Condition 2)") +
  theme_minimal() +
  theme(legend.position = "bottom", # Adjust legend position
        legend.title = element_text(size = 7), # Adjust legend title size
        legend.text = element_text(size = 5.5)) # Adjust legend text size

# Create a bar plot of GrowthRate by Condition
ggplot(data_growth_water_GR, aes(x = Condition, y = GrowthRate)) +
  geom_bar(stat = "summary", fun = "mean", fill = "blue", alpha = 0.6) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Growth Rate per Water Source", 
       x = "Condition", 
       y = "Average Growth Rate") +
  theme_minimal()
  
# Combine growth rate and yield data
combined_data <- rbind(
  data.frame(Condition = data_growth_water_GR$Condition, 
             Value = data_growth_water_GR$GrowthRate, 
             Metric = "Growth Rate", 
             Mean = NA),  # Growth rates
  data.frame(Condition = data_growth_water_MAXOD$Condition, 
             Value = data_growth_water_MAXOD$MAXOD, 
             Metric = "Yield", 
             Mean = NA)   # Yields
)

# Calculate the means for each Condition and Metric
mean_values <- aggregate(Value ~ Condition + Metric, data = combined_data, FUN = mean)

# Add the mean values to the combined data for plotting
combined_data$Mean <- ifelse(combined_data$Metric == "Growth Rate", 
                             mean_values$Value[match(paste(combined_data$Condition, "Growth Rate"), 
                                                     paste(mean_values$Condition, mean_values$Metric))], 
                             mean_values$Value[match(paste(combined_data$Condition, "Yield"), 
                                                     paste(mean_values$Condition, mean_values$Metric))])
# Plot the combined graph
ggplot(combined_data, aes(x = Condition, y = Value)) +  
  # Plot individual jitter points with a color gradient based on the Value
  geom_jitter(aes(color = Value, shape = Metric), 
              width = 0.2, height = 0, size = 2, alpha = 0.7) +  
  # Color gradient for individual jitter points
  scale_color_viridis_c(option = "cividis") + 
  # Plot mean points (larger) with a fixed color (blue for growth rate, red for yield)
  geom_point(data = combined_data[!is.na(combined_data$Mean),], 
             aes(x = Condition, y = Mean), 
             shape = 16, size = 4, color = "darkblue") +  # Black color for mean points
  # Use different shapes for growth rates and yields (square for growth rates, circle for yields)
  scale_shape_manual(values = c(15, 16)) +  # 15 is square, 16 is circle
  # Customize axis labels, title, and theme
  labs(title = "Growth Rates and Yields for Different Water Conditions",
       x = "Condition", y = "Value", fill = "Metric") + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(size = 12)
  ) + 
  facet_wrap(~ Metric, scales = "free_y") 
