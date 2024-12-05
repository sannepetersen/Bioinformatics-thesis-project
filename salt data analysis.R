# Load packages
library(dplyr)
library(ggplot2)
library(viridis)
library(tidyr)
library(car)  # For ANOVA and post-hoc tests

# Create the vectors for growth rates, conditions, preconditions, and strains
growth_rates <- c(
  0.0775, 0.0846, 0.0716, 0.0933, 0.0533,
  0.0484, 0.0316, 0.0746, 0, 0.174,
  0.0519, 0.0842, 0.0857, 0.0708, 0.0984,
  0.0218, 0, 0.0714, 0.0516, 0,
  0.0293, 0.0368, 0.102, 0.195, 0.127,
  0.0619, 0, 0, 0.051, 0.0516,
  0, 0.0206, 0.0237, 0.114, 0.0765,
  0.119, 0, 0, 0, 0,
  0, 0, 0, 0, 0.075,
  0.0545, 0, 0.0676, 0.0563, 0.0535,
  0, 0, 0.053, 0, 0,
  0.132, 0.0741, 0, 0.074, 0.0433,
  0.0236, 0, 0, 0.0428, 0,
  0, 0.0773, 0.083, 0, 0.0719,
  0.0543, 0.0279, 0, 0, 0,
  0, 0, 0.0476, 0.0386, 0.0384, 0.0608, 0.0549, 0.0304,
  0, 0, 0, 0, 0
)

conditions <- c(
  "100% salt", "100% salt", "100% salt", "100% salt", "100% salt", 
  "100% salt", "100% salt", "100% salt", "100% salt", "100% salt", 
  "100% salt", "70% salt", "70% salt", "70% salt", "70% salt", 
  "70% salt", "70% salt", "70% salt", "70% salt", "70% salt", 
  "70% salt", '70% salt', "50% salt", "50% salt", "50% salt", "50% salt", 
  "50% salt", "50% salt", "50% salt", "50% salt", "50% salt", 
  "50% salt", '50% salt', "20% salt", "20% salt", "20% salt", "20% salt", 
  "20% salt", "20% salt", "20% salt", "20% salt", "20% salt", 
  "20% salt", '20% salt', 
  "100% salt", "100% salt", "100% salt", "100% salt", "100% salt", 
  "100% salt", "100% salt", "100% salt", "100% salt", "100% salt", 
  "100% salt",
  "70% salt", "70% salt", "70% salt", "70% salt", 
  "70% salt", "70% salt", "70% salt", "70% salt", "70% salt", 
  "70% salt", "70% salt", "50% salt", "50% salt", "50% salt", 
  "50% salt", "50% salt", "50% salt", "50% salt", "50% salt", 
  "50% salt", "50% salt", "50% salt", "20% salt", "20% salt", 
  "20% salt", "20% salt", "20% salt", "20% salt", "20% salt", 
  "20% salt", "20% salt", "20% salt", "20% salt"
)

preconditions <- c(
  "100% salt", "60% salt", "20% salt", "100% salt", "60% salt", 
  "20% salt", "100% salt", "60% salt", "20% salt", "MBL -N", 
  "MBL -N", 
  "100% salt", "60% salt", "20% salt", "100% salt", 
  "60% salt", "20% salt", "100% salt", "60% salt", "20% salt", 
  "MBL -N", "MBL -N",
  "100% salt", "60% salt", "20% salt", 
  "100% salt", "60% salt", "20% salt", "100% salt", "60% salt", 
  "20% salt", "MBL -N", "MBL -N", 
  "100% salt", "60% salt", 
  "20% salt", "100% salt", "60% salt", "20% salt", "100% salt", 
  "60% salt", "20% salt", "MBL -N", "MBL -N", 
  "100% salt", "60% salt", "20% salt", "100% salt", "60% salt", 
  "20% salt", "100% salt", "60% salt", "20% salt", "MBL -N", 
  "MBL -N", 
  "100% salt", "60% salt", "20% salt", "100% salt", "60% salt", 
  "20% salt", "100% salt", "60% salt", "20% salt", "MBL -N", 
  "MBL -N", 
  "100% salt", "60% salt", "20% salt", "100% salt", 
  "60% salt", "20% salt", "100% salt", "60% salt", "20% salt", 
  "MBL -N", "MBL -N",
  "100% salt", "60% salt", "20% salt", "100% salt", 
  "60% salt", "20% salt", "100% salt", "60% salt", "20% salt", 
  "MBL -N", "MBL -N"
)

strains <- c(
  "4F10", "4F10", "4F10", "4H09", "4H09", "4H09", "1A01", "1A01", 
  "1A01", "4F10", "4H09", 
  "4F10", "4F10", "4F10", "4H09", "4H09", 
  "4H09", "1A01", "1A01", "1A01", "4F10", "4H09", 
  "4F10", "4F10", "4F10", "4H09", "4H09", "4H09", "1A01", "1A01", "1A01", 
  "4F10", "4H09", 
  "4F10", "4F10", "4F10", "4H09", "4H09", "4H09", "1A01", 
  "1A01", "1A01", "4F10", "4H09", 
  "4D01", "4D01", "4D01", "A2R16", 
  "A2R16", "A2R16", "3D05", "3D05", "3D05", "4D01", "A2R16", 
  "4D01", "4D01", "4D01", "A2R16", "A2R16", "A2R16", "3D05", "3D05", "3D05", 
  "4D01", "A2R16", 
  "4D01", "4D01", "4D01", "A2R16", "A2R16", "A2R16", 
  "3D05", "3D05", "3D05", "4D01", "A2R16",
  "4D01", "4D01", "4D01", "A2R16", "A2R16", "A2R16", 
  "3D05", "3D05", "3D05", "4D01", "A2R16"
)

max_OD <- c(0.315, 0.265, 0.281, 0.471, 0.175, 0.122, 0.172, 0.149, 0, 
    0.75, 0.235, 0.351, 0.322, 0.485, 0.311, 0.133, 0, 0.126, 
    0.111, 0, 0.438, 0.187, 0.397, 0.623, 1.02, 0.275, 0, 0, 
    0.134, 0.134, 0, 0.183, 0.145, 0.847, 0.57, 0.88, 0, 0, 0, 
    0, 0, 0, 0, 0, 0.117, 0.127, 0, 0.191, 0.211, 0.176, 0, 
    0, 0.248, 0, 0, 0.135, 0.152, 0, 0.199, 0.154, 0.136, 0, 
    0, 0.143, 0, 0, 0.125, 0.131, 0, 0.214, 0.15, 0.115, 0, 
    0, 0, 0, 0, 0.129, 0.15, 0.107, 0.189, 0.187, 0.164, 0, 
    0, 0, 0, 0)


lag_time <- c(3.07, 3.33, 2.89, 13.2, 40.7, 62.5, 4.16, 3.16, 0, 
              3.51, 2.19, 3.38, 3.3, 2.98, 13.2, 53.1, 0, 3.16, 
              7.6, 0, 8.2, 1.5, 23, 11.6, 4.95, 14.2, 0, 0, 2.32, 
              7.69, 0, 4.67, 1.95, 45.9, 3.25, 7.55, 0, 0, 0, 0, 
              0, 0, 0, 0, 8.28, 9.17, 0, 6.49, 3.84, 5.64, 0, 0, 
              5.65, 0, 0, 8.81, 8.03, 0, 4.79, 7.07, 4.41, 0, 0, 
              12.9, 0, 0, 9.98, 11.7, 0, 6.51, 7.75, 5.96, 0, 0, 
              0, 0, 0, 22.4, 40.9, 68.5, 7.86, 6.12, 7.81, 0, 0, 
              0, 0, 0)

# Combine data
data_list_total <- list(
  GrowthRates = growth_rates,
  OD = max_OD,
  lag = lag_time,
  Condition = conditions,
  Precondition = preconditions,
  Strain = strains
)

# Create dataframe
data_list_total <- as.data.frame(data_list_total)

# Remove rows with "MBL - N" in the Precondition column
data_cleanedtotal <- data_list_total %>%
  filter(Precondition != "MBL -N")

# Define the order for Condition and Precondition
condition_order <- c("100% salt", "70% salt", "50% salt", "20% salt")
precondition_order <- c("100% salt", "60% salt", "20% salt")

# Clean the data and remove rows with 'MBL - N'
data_cleanedtotal <- data_cleanedtotal %>%
  filter(Precondition != "MBL - N") %>%
  mutate(Condition = factor(Condition, levels = condition_order),
         Precondition = factor(Precondition, levels = precondition_order))

# Determine scale factor 
scale_factor <- 10

# Create the faceted plot
ggplot(data_cleanedtotal, aes(x = Precondition)) + 
  geom_point(aes(y = OD), size = 3, color = "#932667FF") + 
  geom_point(aes(y = GrowthRates * scale_factor), size = 3, color = "#FCA50AFF") + # scale_factor to adjust the GR axis
  facet_wrap(~ Strain) + 
  theme_minimal() + 
  labs(title = "Growth Rates and yields per precondition",
       x = "Precondition",
       y = "Yield") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(name = "OD",
                     sec.axis = sec_axis(~./scale_factor, name = "Growth Rate * 10")) # Adjust scale_factor as necessary

# Create plot of growth rates and OD
ggplot(data_cleanedtotal, aes(x = Precondition)) + 
  # Line for OD
  geom_line(aes(y = OD, color = "OD"), size = 1, group = 1) + 
  # Line for Growth Rates (scaled)
  geom_line(aes(y = GrowthRates * scale_factor, color = "Growth Rate"), size = 1, group = 1) + 
  # Facet by Strain
  facet_wrap(~ Strain) + 
  theme_minimal() + 
  labs(
    title = "Growth Rates and OD by Precondition", 
    x = "Precondition", 
    y = "OD",
    color = "Measurement Type"
  ) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "top"
  ) + 
  scale_y_continuous(
    name = "OD",
    sec.axis = sec_axis(~ . / scale_factor, name = "Growth Rate") # Adjust secondary axis for growth rates
  ) + 
  scale_color_manual(values = c("OD" = "#932667FF", "Growth Rate" = "#FCA50AFF")) # Color for the lines

# Calculate the average OD and Growth Rates for each strain and precondition
averaged_data <- data_cleanedtotal %>%
  group_by(Strain, Precondition) %>%
  summarise(
    avg_OD = mean(OD, na.rm = TRUE),
    avg_GrowthRate = mean(GrowthRates, na.rm = TRUE)
  )

# Plot average growth rates
ggplot(averaged_data, aes(x = Precondition)) + 
  # Line for average OD
  geom_line(aes(y = avg_OD, color = "OD"), size = 1, group = 1) + 
  # Line for average Growth Rates (scaled)
  geom_line(aes(y = avg_GrowthRate * scale_factor, color = "Growth Rate"), size = 1, group = 1) + 
  # Facet by Strain
  facet_wrap(~ Strain) + 
  theme_minimal() + 
  labs(
    title = "Average Growth Rates and OD by Precondition", 
    x = "Precondition", 
    y = "Average OD",
    color = "Measurement Type"
  ) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "top"
  ) + 
  scale_y_continuous(
    name = "Average OD",
    sec.axis = sec_axis(~ . / scale_factor, name = "Average Growth Rate") # Adjust secondary axis for growth rates
  ) + 
  scale_color_manual(values = c("OD" = "#932667FF", "Growth Rate" = "#FCA50AFF")) # Color for the lines

# Calculate the average OD and Growth Rates for each strain and precondition
averaged_data <- data_cleanedtotal %>%
  group_by(Strain, Condition) %>%
  summarise(
    avg_OD = mean(OD, na.rm = TRUE),
    avg_GrowthRate = mean(GrowthRates, na.rm = TRUE)
  )

# Plot average growth rates and OD
ggplot(averaged_data, aes(x = Condition)) + 
  # Line for average OD
  geom_line(aes(y = avg_OD, color = "OD"), size = 1, group = 1) + 
  # Line for average Growth Rates (scaled)
  geom_line(aes(y = avg_GrowthRate * scale_factor, color = "Growth Rate"), size = 1, group = 1) + 
  # Facet by Strain
  facet_wrap(~ Strain) + 
  theme_minimal() + 
  labs(
    title = "Average Growth Rates and OD by Condition", 
    x = "Condition", 
    y = "Average OD",
    color = "Measurement Type"
  ) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "top"
  ) + 
  scale_y_continuous(
    name = "Average OD",
    sec.axis = sec_axis(~ . / scale_factor, name = "Average Growth Rate") # Adjust secondary axis for growth rates
  ) + 
  scale_color_manual(values = c("OD" = "#932667FF", "Growth Rate" = "#FCA50AFF")) # Color for the lines

# Summarize the data to calculate average growth rates by Condition, Precondition, and Strain
data_summary <- data_cleanedtotal %>%
  group_by(Strain, Condition, Precondition) %>%
  summarise(AverageGrowthRate = mean(GrowthRates, na.rm = TRUE), .groups = 'drop')

# Create a bar plot with facets for each Strain
ggplot(data_summary, aes(x = Condition, y = AverageGrowthRate, fill = Precondition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Average Growth Rates by Condition and Precondition per Strain",
       x = "Condition",
       y = "Average Growth Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1") +  # Use a color palette for fill
  facet_wrap(~ Strain, ncol = 2)  # Facet by Strain with 2 columns

# Pick colors
gradient_colors <- colorRampPalette(c("blue", "#FDE725FF"))(3)

# Create a point plot with facets for each Strain
ggplot(data_cleanedtotal, aes(x = Condition, y = GrowthRates, color = Precondition)) +
  geom_point(size = 3, position = position_jitter(width = 0.2)) +  # Jitter to avoid overplotting
  theme_minimal() +
  labs(title = "Growth Rates by Condition and Precondition per Strain",
       x = "Condition",
       y = "Growth Rates") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = gradient_colors) +  # Apply gradient colors
  facet_wrap(~ Strain, ncol = 2)  # Facet by Strain with 2 columns

# Create a point plot with facets for each Strain
ggplot(data_cleanedtotal, aes(x = Condition, y = OD, color = Precondition)) +
  geom_point(size = 3, position = position_jitter(width = 0.2)) +  # Jitter to avoid overplotting
  theme_minimal() +
  labs(title = "Growth Rates by Condition and Precondition per Strain",
       x = "Condition",
       y = "Growth Rates") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = gradient_colors) +  # Apply gradient colors
  facet_wrap(~ Strain, ncol = 2)  # Facet by Strain with 2 columns

# Pick colors
gradient_colors <- colorRampPalette(c("blue", "#FDE725FF"))(6)

# Create plot
ggplot(data_cleanedtotal, aes(x = log(GrowthRates), y = log(OD), color = Strain)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Yield vs Growth Rates",
       x = "log(GR)",
       y = "log(yield)") +
  scale_color_manual(values = gradient_colors) +  # Apply gradient colors
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create plot
ggplot(data_cleanedtotal, aes(x = log(OD), y = log(lag), color = Strain)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Yield vs Growth Rates",
       x = "log(GR)",
       y = "log(LT)") +
  scale_color_manual(values = gradient_colors) +  # Apply gradient colors
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate Spearman correlation
spearman_corr <- cor(data_cleanedtotal$GrowthRates, data_cleanedtotal$OD, method = "spearman")
print(paste("Spearman correlation: ", spearman_corr))

# LM
lm_model <- lm(log(OD) ~ log(GrowthRates), data = data_cleanedtotal)
summary(lm_model)

# Pick colors
gradient_colors <- viridis(6, option = 'B')

# Plot the regression line
ggplot(data_cleanedtotal, aes(x = log(GrowthRates), y = log(OD))) +
  geom_point(aes(color = Strain), size = 3) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +  # Add regression line
  theme_minimal() +
  labs(title = "Regression of Yield on Growth Rates",
       x = "log(Growth Rates)",
       y = "log(OD)") +
  scale_color_manual(values = gradient_colors) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Assuming `data_list_total` is a list with the relevant vectors
# Extract the data from the list
growth_rates <- data_list_total$GrowthRates
max_OD <- data_list_total$OD
lag_time <- data_list_total$lag
conditions <- data_list_total$Condition
preconditions <- data_list_total$Precondition
strains <- data_list_total$Strain

# Combine the data into a data frame for easier manipulation
data <- data.frame(growth_rates, max_OD, lag_time, conditions, preconditions, strains)

# Loop through each unique Precondition
unique_preconditions <- unique(data$preconditions)
for(precondition in unique_preconditions) {
  
  # Subset the data for the current precondition
  data_subset <- filter(data, preconditions == precondition)
  
  # Create the heatmap
  heatmap <- ggplot(data_subset, aes(x = conditions, y = strains, fill = max_OD)) +
    geom_tile() +
    scale_fill_viridis_c() +  # You can use any color scale, e.g., viridis
    theme_minimal() +
    labs(title = paste("Heatmap for Precondition:", precondition),
         x = "Condition",
         y = "Strain",
         fill = "Max OD") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
  
  # Display the heatmap
  print(heatmap)
}

# Assuming `data_list_total` is a list with the relevant vectors
# Extract the data from the list
growth_rates <- data_list_total$GrowthRates
max_OD <- data_list_total$OD
lag_time <- data_list_total$lag
conditions <- data_list_total$Condition
preconditions <- data_list_total$Precondition
strains <- data_list_total$Strain

# Combine the data into a data frame for easier manipulation
data <- data.frame(growth_rates, max_OD, lag_time, conditions, preconditions, strains)

# Remove rows with "MBL - N" in the Precondition column
data <- data %>%
  filter(preconditions != "MBL -N")

# Specify the desired order for the conditions
precondition_order <- c("100% salt", "60% salt", "20% salt")

# Convert the 'conditions' variable to a factor with the specified order
data$preconditions <- factor(data$preconditions, levels = precondition_order)

# Create the heatmap for all preconditions combined into one plot
heatmap_all <- ggplot(data, aes(x = preconditions, y = strains, fill = max_OD)) +
  geom_tile() +
  scale_fill_viridis_c(option = 'cividis') +  # You can use any color scale
  theme_minimal() +
  labs(title = "Heatmap for All conditions",
       x = "Precondition",
       y = "Strain",
       fill = "Max OD") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  facet_wrap(~ conditions, scales = "free_y")  # Facet by precondition

# Display the heatmap
print(heatmap_all)

# Create the heatmap for all preconditions combined into one plot
heatmap_all <- ggplot(data, aes(x = preconditions, y = strains, fill = growth_rates)) +
  geom_tile() +
  scale_fill_viridis_c(option = 'cividis') +  # You can use any color scale
  theme_minimal() +
  labs(title = "Heatmap for All conditions",
       x = "Precondition",
       y = "Strain",
       fill = "GR") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  facet_wrap(~ conditions, scales = "free_y")  # Facet by precondition

# Display the heatmap
print(heatmap_all)

# Assuming `data` is the data frame with your original data
# Normalize the max_OD by the maximum value for each strain per condition and precondition
data_normalized <- data %>%
  group_by(strains, conditions) %>%
  mutate(max_OD_normalized = max_OD / max(max_OD)) %>%
  ungroup() %>%
  mutate(max_OD_normalized = ifelse(is.nan(max_OD_normalized), 0, max_OD_normalized))

# Create the heatmap with normalized max_OD
heatmap_all <- ggplot(data_normalized, aes(x = preconditions, y = strains, fill = max_OD_normalized)) +
  geom_tile() +
  scale_fill_viridis_c(option = 'cividis') +  # You can use any color scale
  theme_minimal() +
  labs(title = "Heatmap for All Conditions ",
       x = "Precondition",
       y = "Strain",
       fill = "Normalized Max OD") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  facet_wrap(~ conditions, scales = "free_y")  # Facet by condition

# Display the heatmap
print(heatmap_all)

# Normalize data
data_normalized <- data %>%
  group_by(strains, conditions) %>%
  mutate(GR_normalized = growth_rates / max(growth_rates)) %>%
  ungroup() %>%
  mutate(GR_normalized = ifelse(is.nan(GR_normalized), 0, GR_normalized))

# Create the heatmap with normalized max_OD
heatmap_all <- ggplot(data_normalized, aes(x = preconditions, y = strains, fill = GR_normalized)) +
  geom_tile() +
  scale_fill_viridis_c(option = 'cividis') +  # You can use any color scale
  theme_minimal() +
  labs(title = "Heatmap for All Conditions ",
       x = "Precondition",
       y = "Strain",
       fill = "Normalized GR") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  facet_wrap(~ conditions, scales = "free_y")  # Facet by condition

# Display the heatmap
print(heatmap_all)

# Assuming data is already available as `data`
# Calculate the mean max_OD per precondition and condition
mean_max_OD <- data %>%
  group_by(conditions, preconditions) %>%
  summarise(mean_max_OD = mean(max_OD, na.rm = TRUE))  # Mean max_OD per condition and precondition

# Visualize the data with a heatmap
heatmap_all <- ggplot(data, aes(x = preconditions, y = strains, fill = max_OD)) +
  geom_tile() +
  scale_fill_viridis_c(option = 'cividis') +
  theme_minimal() +
  labs(title = "Heatmap for All Conditions",
       x = "Precondition",
       y = "Strain",
       fill = "Max OD") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ conditions, scales = "free_y")  # Facet by condition

# Display the heatmap
print(heatmap_all)

# Perform ANOVA to test if max_OD differs significantly by precondition for each condition
anova_results <- data %>%
  group_by(conditions) %>%
  do({
    aov_result <- aov(growth_rates ~ preconditions, data = .)  # ANOVA for each condition
    aov_summary <- summary(aov_result)[[1]]  # Extract the summary of the ANOVA
    data.frame(aov_summary)  # Convert the summary to a data frame
  })

# Display ANOVA results for each condition
print(anova_results)

# If ANOVA is significant, perform post-hoc pairwise comparisons (Tukey's HSD)
posthoc_results <- data %>%
  group_by(conditions) %>%
  do({
    aov_result <- aov(growth_rates ~ preconditions, data = .)  # ANOVA for each condition
    tukey_result <- TukeyHSD(aov_result)  # Tukey HSD test for post-hoc comparisons
    tukey_df <- as.data.frame(tukey_result$preconditions)  # Extract results as a data frame
    tukey_df$condition <- unique(.$conditions)  # Add the condition to the results
    tukey_df  # Return the Tukey results as a data frame
  })

# Display post-hoc results
print(posthoc_results)

# Perform ANOVA to test whether 'condition' influences the growth rate
anova_result <- aov(growth_rates ~ conditions, data = data)

# Summarize the ANOVA result
summary(anova_result)

# Perform ANOVA to test whether 'condition' influences the growth rate
anova_result <- aov(max_OD ~ conditions, data = data)

# Summarize the ANOVA result
summary(anova_result)

# Perform ANOVA to test whether 'condition' influences the growth rate
anova_result <- aov(lag_time ~ conditions, data = data)

# Summarize the ANOVA result
summary(anova_result)

# Normalize max_OD by the maximum value for each strain per condition and precondition
data_normalized <- data %>%
  group_by(strains, conditions) %>%
  mutate(max_OD_normalized = max_OD / max(max_OD)) %>%
  ungroup() %>%
  mutate(Yield = ifelse(is.nan(max_OD_normalized), 0, max_OD_normalized))

# Normalize growth rates by the maximum value for each strain per condition and precondition
data_normalized <- data_normalized %>%
  group_by(strains, conditions) %>%
  mutate(GR = growth_rates / max(growth_rates)) %>%
  ungroup() %>%
  mutate(GR = ifelse(is.nan(GR), 0, GR))

# Combine the data into a long format for both max_OD_normalized and GR_normalized
data_long <- data_normalized %>%
  gather(key = "metric", value = "value", Yield, GR)

order_salt <- c('100% salt', '70% salt', '50% salt', '20% salt')

# Create the heatmap with both normalized max_OD and GR, using facets for clear separation
heatmap_combined <- ggplot(data_long, aes(x = preconditions, y = strains, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(option = 'cividis') +  # You can use any color scale
  theme_minimal() +
  labs(title = "Heatmap for Normalized Max OD and Growth Rates",
       x = "Precondition",
       y = "Strain",
       fill = "Normalized Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  facet_grid(metric ~ conditions, scales = "free_y")  # Facet by both metric and condition

# Display the heatmap
print(heatmap_combined)

# Convert 'conditions' column to a factor with the desired order
data_long$conditions <- factor(data_long$conditions, levels = order_salt)

# Create the heatmap with both normalized max_OD and GR, using facets for clear separation
heatmap_combined <- ggplot(data_long, aes(x = preconditions, y = strains, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(option = 'cividis') +  # You can use any color scale
  theme_minimal() +
  labs(title = "Heatmap for Normalized Max OD and Growth Rates",
       x = "Precondition",
       y = "Strain",
       fill = "Normalized Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  facet_grid(metric ~ conditions, scales = "free_y")  # Facet by both metric and condition

# Display the heatmap
print(heatmap_combined)


