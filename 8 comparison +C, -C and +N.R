# Comparison +C, -C and +N data

# Install packages
install.packages("ggridges")

# Load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggridges)

# Combine datasets organic amino acids + C
info_plusC <- bind_rows(alanine_plusC, glycine_plusC, 
                        serine_plusC, glutamate_plusC, 
                        proline_plusC, aspartate_plusC, 
                        threonine_plusC, 
                        glutamine_plusC, arginine_plusC, 
                        asparagine_plusC)


# Combine both datasets organic amino acids - C
info_minC <- bind_rows(alanine_minC, glycine_minC, 
                       serine_minC, glutamate_minC, 
                       proline_minC, aspartate_minC, 
                       threonine_minC, 
                       glutamine_minC, arginine_minC, 
                       asparagine_minC)




# Combine both datasets organic amino acids + N
info_plusN <- bind_rows(alanine_plusN, glycine_plusN,
                        serine_plusN, glutamate_plusN, 
                        proline_plusN, aspartate_plusN, 
                        asparagine_plusN, threonine_plusN, 
                        glutamine_plusN, arginine_plusN)

# Obtain distinct data
info_plusN_distinct <- info_plusN %>%
  distinct(Name, Treatment, Description, .keep_all = TRUE)
info_plusC_distinct <- info_plusC %>%
  distinct(Name, Treatment, Description, .keep_all = TRUE)
info_minC_distinct <- info_minC %>%
  distinct(Name, Treatment, Description, .keep_all = TRUE)

# Combine the three datasets into one for plotting
info_combined <- bind_rows(
  info_plusC_distinct %>% mutate(Condition = "N-source"),
  info_minC_distinct %>% mutate(Condition = "N and C-source"),
  info_plusN_distinct %>% mutate(Condition = "C-source")
)

# Filter the dataset and adjust factors for each order
info_combined$Name <- factor(info_combined$Name, levels = unique(info_combined$Name))
info_combined$Treatment <- factor(info_combined$Treatment, levels = unique(info_combined$Treatment))

# Ensure that Name is recalculated within each order
info_combined <- info_combined %>%
  group_by(Description) %>%
  mutate(Name = factor(Name, levels = unique(Name))) %>%
  ungroup()

# Create the heatmap
ggplot(info_combined, aes(x = Name, y = Treatment, fill = as.factor(growth))) +
  geom_tile(color = "white") + 
  scale_fill_manual(
    values = c("0" = "blue", "1" = "yellow"), 
    name = "Growth",
    labels = c("No Growth (0)", "Growth (1)")
  ) + 
  labs(
    title = "Growth Heatmap by Strain and Condition",
    x = "Strain",
    y = "Condition"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=3), 
    axis.text.y = element_text(size = 4), 
    strip.text = element_text(size = 8, face = "bold") 
  ) +
  facet_wrap(~ Description, ncol = 3, scales = "free_x")  

# Calculate percentage of growth per order and treatment
growth_percentages_plusC <- info_plusC %>%
  group_by(Treatment) %>%
  summarise(
    growth_count = sum(growth),  
    total_count = n(),                 
    growth_percentage = (growth_count / total_count) * 100  
  ) %>%
  ungroup()

# Change order to descending
growth_percentages_plusC <- growth_percentages_plusC %>%
  arrange(Treatment, desc(growth_percentage))

# Plot growth percentages +C
ggplot(growth_percentages_plusC, aes(x = reorder(Treatment, growth_percentage), y = growth_percentage, fill = growth_percentage)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = growth_percentage)) + 
  labs(
    title = "Percentage of growth as N-source",
    x = "Treatment",
    y = "Growth Percentage (%)",
    fill = "Growth Percentage"
  ) +
  theme_minimal() +
  ylim(0,100)+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=8),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Calculate percentage of growth -C
growth_percentages_minC <- info_minC %>%
  group_by(Treatment) %>%
  summarise(
    growth_count = sum(growth),  
    total_count = n(),               
    growth_percentage = (growth_count / total_count) * 100 
  ) %>%
  ungroup()

# Change order to descending
growth_percentages_minC <- growth_percentages_minC %>%
  arrange(Treatment, desc(growth_percentage))

# Plot growth percentages -C
ggplot(growth_percentages_minC, aes(x = reorder(Treatment, growth_percentage), y = growth_percentage, fill = growth_percentage)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = growth_percentage)) +  
  labs(
    title = "Percentage of growth as N and C-source",
    x = "Treatment",
    y = "Growth Percentage (%)",
    fill = "Growth Percentage"
  ) +
  theme_minimal() +
  ylim(0,100)+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=8),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Calculate percentage of growth per order and treatment
growth_percentages_plusN <- info_plusN %>%
  group_by(Treatment) %>%
  summarise(
    growth_count = sum(growth), 
    total_count = n(),                
    growth_percentage = (growth_count / total_count) * 100 
  ) %>%
  ungroup()

# Change order to descending
growth_percentages_plusN <- growth_percentages_plusN %>%
  arrange(Treatment, desc(growth_percentage))

# Plot growth percentages +N
ggplot(growth_percentages_plusN, aes(x = reorder(Treatment, growth_percentage), y = growth_percentage, fill = growth_percentage)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = growth_percentage)) +  
  labs(
    title = "Percentage of growth as C-source",
    x = "Treatment",
    y = "Growth Percentage (%)",
    fill = "Growth Percentage"
  ) +
  theme_minimal() +
  ylim(0,100)+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=8),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# Combine data into one frame and extract the condition (the first word in Treatment)
growth_percentages_combined <- bind_rows(
  growth_percentages_plusC %>%
    mutate(Treatment_type = "N-source"),
  
  growth_percentages_minC %>%
    mutate(Treatment_type = "N and C-source"),
  
  growth_percentages_plusN %>%
    mutate(Treatment_type = "C-source")
) %>%
  # Extract the first word of Treatment column (condition)
  mutate(Condition = word(Treatment, 1))

# Plot with colored dots for each Treatment type
ggplot(growth_percentages_combined, aes(x = reorder(Condition, growth_percentage), y = growth_percentage, color = Treatment_type)) + 
  geom_point(size = 4) + 
  scale_color_manual(values = c("N-source" = "#F0E442", "N and C-source" = "#E69F00", "C-source" =  "#56B4E9")) + # Color the dots
  labs(
    title = "Comparison of Growth Percentages Across Conditions",
    x = "Condition",
    y = "Growth Percentage (%)",
    color = "Condition Type"
  ) +
  theme_minimal() +
  ylim(0, 100) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, size = 12),
    legend.position = "top"
  )

# Boxplot for max_OD
ggplot(info_combined, aes(x = Condition, y = max_OD, fill = Condition)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.2) +  
  labs(title = "Distribution of yield",
       x = "Condition",
       y = "Yield") +
  scale_fill_manual(values = c("N-source" = "#F0E442", "N and C-source" = "#E69F00", "C-source" = "#56B4E9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none')

# Boxplot for max_derivative
ggplot(info_combined, aes(x = Condition, y = max_derivative, fill = Condition)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.2) +  
  labs(title = "Distribution of max GR",
       x = "Condition",
       y = "Max GR") +
  scale_fill_manual(values = c("N-source" = "#F0E442", "N and C-source" = "#E69F00", "C-source" = "#56B4E9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Plot boxplots per order
ggplot(info_combined, aes(x = Condition, y = max_derivative, fill = Condition)) +
  geom_boxplot() +
  labs(title = "Distribution of GR",
       x = "Condition",
       y = "GR") +
  scale_fill_manual(values = c("N-source" = "#F0E442", "N and C-source" = "#E69F00", "C-source" = "#56B4E9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') + 
  facet_wrap(~ Description, scales = "free_y") 

# Obtain only strains that grow
info_combined_positive <- info_combined[info_combined$max_derivative>0, ]

# Plot growth rates for strains that grow across all orders
ggplot(info_combined_positive, aes(x = Condition, y = max_derivative, color = Condition)) + 
  geom_jitter(width = 0.2, alpha = 0.5, size = 1) + 
  stat_summary(fun = "mean", geom = "point", size = 3, color = "black", shape = 21, fill = "white") + 
  labs(
    title = "Distribution of GR",
    x = "Condition",
    y = "GR"
  ) + 
  scale_color_manual(values = c("N-source" = "#F0E442", "N and C-source" = "#E69F00", "C-source" = "#56B4E9")) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = 'none'
  ) + 
  facet_wrap(~ Description, scales = "free_y") 

# Make density plot of growth rates across the three conditions
ggplot(info_combined, aes(x = max_derivative, fill = Condition)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Density plot GR",
       x = "GR",
       y = "Density") +
  scale_fill_manual(values = c("N-source" = "#F0E442", "N and C-source" = "#E69F00", "C-source" = "#56B4E9")) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Create ridgeline plot of growth rates across three conditions
ggplot(info_combined_positive, aes(x = max_derivative, y = Condition, fill = Condition)) +
  geom_density_ridges(alpha = 0.7, scale = 1.5, size = 1) + 
  labs(
    title = "Ridgeline Plot of Growth Rates",
    x = "GR",
    y = "Condition"
  ) +
  scale_fill_manual(
    values = c("N-source" = "#F0E442", "N and C-source" = "#E69F00", "C-source" = "#56B4E9"),
    name = "Condition"
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "none",  
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14), 
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 10) 
  )  

# Create ridgeline plot of growth rates across three conditions for all orders
ggplot(info_combined, aes(x = max_derivative, y = Condition, fill = Condition)) +
  geom_density_ridges(alpha = 0.7, scale = 1.5, size = 1) + 
  labs(
    title = "Ridgeline Plot of Growth Rates (max_derivative) Across Conditions by Description",
    x = "Growth Rate (GR)",
    y = "Condition"
  ) +
  scale_fill_manual(
    values = c("N-source" = "#F0E442", "N and C-source" = "#E69F00", "C-source" = "#56B4E9"),
    name = "Condition"
  ) +
  facet_wrap(~ Description, scales = "free_y") + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "none",  
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 10),  
    plot.title = element_text(hjust = 0.5, size = 14),  
    axis.title = element_text(size = 12), 
    axis.text = element_text(size = 10), 
    strip.text = element_text(size = 10)  
  )

# Step 1: Combine datasets
info_combined <- bind_rows(
  info_plusN_distinct %>% mutate(Group = "plusN"),
  info_plusC_distinct %>% mutate(Group = "plusC"),
  info_minC_distinct %>% mutate(Group = "minC")
)

# Step 2: Perform ANOVA
anova_result <- aov(max_derivative ~ Group, data = info_combined)
summary(anova_result)

# Step 3: Perform Tukey's HSD for pairwise comparisons
post_hoc <- TukeyHSD(anova_result)
print(post_hoc)

# Create empty list
results_list <- list()

# Loop through each unique order
for (name in unique(info_combined$Description)) {
  
  # Subset the data for the current 'Description'
  subset_data <- info_combined[info_combined$Description == name, ]
  
  # Create a contingency table for 'growth' vs 'order' for the current 'Description'
  contingency_table <- table(subset_data$growth, subset_data$Description)
  
  # Perform the Chi-squared test
  chi_squared_result <- chisq.test(contingency_table)
  
  # Store the result in the list
  results_list[[name]] <- chi_squared_result
}

# Logistic regression to see the effect of yield and order on growth
model <- glm(growth ~ Description, data = info_combined, family = binomial)

# Summarize the model
summary(model)

# ANOVA to test if max_OD differs across the levels of Description
anova_result <- aov(max_OD ~ Description, data = info_combined)

# Summarize ANOVA results
summary(anova_result)

# Kruskal-Wallis test to compare max_OD across Description groups
kruskal_result <- kruskal.test(max_OD ~ Description, data = info_combined)

# Print the result
kruskal_result

# Perform Tukey's HSD test for pairwise comparisons
tukey_result <- TukeyHSD(anova_result)

# Print the Tukey result
summary(tukey_result)

# Step 2: Split the data by Description
results <- info_combined %>%
  group_by(Description) %>%
  group_split()

# Initialize lists to store results
anova_results <- list()
tukey_results <- list()

# Step 3: Perform ANOVA and Tukey's HSD for each Description
for (desc_data in results) {
  # Get the Description name
  description_name <- unique(desc_data$Description)
  
  # Perform ANOVA
  anova_result <- aov(max_derivative ~ Group, data = desc_data)
  anova_results[[description_name]] <- summary(anova_result)
  
  # Perform Tukey's HSD
  tukey_result <- TukeyHSD(anova_result)
  tukey_results[[description_name]] <- tukey_result
}

# Print or access results for each Description
for (desc in names(anova_results)) {
  cat("\nANOVA results for Description:", desc, "\n")
  print(anova_results[[desc]])
  
  cat("\nTukey's HSD results for Description:", desc, "\n")
  print(tukey_results[[desc]])
}

