
# Select data and condition
selected_data <- combined_t_test_data[combined_t_test_data$Treatment == 'Alanine + C', ]
selected_data <- selected_data %>% drop_na()

# Create long format
selected_data_long <- selected_data %>%
  pivot_longer(cols = starts_with("K"),  
               names_to = "KO",
               values_to = "present") %>%
  filter(!is.na(present))  

# Obtain group that grows and not grows
selected_data_growth <- selected_data[selected_data$growth_OD == 1,]
selected_data_no_growth <- selected_data[selected_data$growth_OD == 0,]

# Long format with data of group that grows
selected_data_long_growth <- selected_data_growth %>%
  pivot_longer(cols = starts_with("K"), 
               names_to = "KO", 
               values_to = "present") %>%
  filter(!is.na(present)) 

# Long format with data of group that does not grow
selected_data_long_no_growth <- selected_data_no_growth %>%
  pivot_longer(cols = starts_with("K"), 
               names_to = "KO", 
               values_to = "present") %>%
  filter(!is.na(present)) 

# Perform a t-test (independent two-sample t-test)
KOs <- grep("^K", colnames(selected_data_growth), value = TRUE)

# Create an empty data frame to store results
p_values <- data.frame(KO = character(), p_value = numeric())

# Loop through each KO (column) that starts with 'K'
for (ko in KOs) {
  # Extract the data for the current KO from both dataframes
  group1 <- selected_data_growth[[ko]]
  group2 <- selected_data_no_growth[[ko]]
  
  # Check if both groups have more than one unique value
  if (length(unique(group1)) > 1 || length(unique(group2)) > 1) {
    # Perform the t-test
    t_test_result <- t.test(group1, group2)
    
    # Store the KO and p-value in the results data frame
    p_values <- rbind(p_values, data.frame(KO = ko, p_value = t_test_result$p.value))
  } else {
    # If either group is constant (no variation), return NA for p-value
    p_values <- rbind(p_values, data.frame(KO = ko, p_value = NA))
  }
}

# View the p-values for each KO
print(p_values)

# see if there are kos that are specific for growth
result_yes <- selected_data_growth %>%
  group_by(KO) %>%
  summarise(
    all_present_positive = all(present > 0), 
    .groups = "drop"
  ) %>%
  filter(all_present_positive == TRUE)

# see if there are kos that do not occur in no growth
result_no <- selected_data_no_growth %>%
  group_by(KO) %>%
  summarise(
    all_present_positive = all(present == 0),  
    .groups = "drop"
  ) %>%
  filter(all_present_positive == TRUE)

# Look at the intersection
KO_growth <- result_yes$KO
KO_nogrowth <- result_no$KO
selection_ko <- intersect(KO_growth, KO_nogrowth)

# Adjust for multiple testing using the Benjamini-Hochberg method
p_values <- p_values %>%
  mutate(
    p_value_adjusted = p.adjust(p_value, method = "BH")  #
  )

# Filter for significant KOs with adjusted p-value < 0.05
significant_KOs <- p_values %>%
  filter(!is.na(p_value_adjusted) & p_value_adjusted < 0.05)

# Look at the intersection with the selected KOs from nitrogen conversion pathways
p_values_order_nitro <- p_values[p_values$KO %in% intersect(nitrogen_kos, significant_KOs$KO), ]
intersect(nitrogen_kos, significant_KOs$KO)

# Filter the top 15 significant KOs based on adjusted p-value
top_50_KOs <- significant_KOs %>%
  arrange(p_value_adjusted) %>%  
  head(20) 

# Plot the top 15 significant KOs with their adjusted p-values
ggplot(top_50_KOs, aes(x = reorder(KO, p_value_adjusted), y = p_value_adjusted)) +
  geom_bar(stat = "identity", aes(fill = p_value_adjusted), show.legend = FALSE) +  
  labs(
    title = "Top 50 Significant KOs",
    x = "KO",
    y = "Adjusted P-value (BH)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Filter your data to keep only the significant KOs
significant_ko_data <- selected_data_long %>%
  filter(KO %in% top_50_KOs$KO) %>%
  mutate(
    growth_label = ifelse(growth_OD == 1, "Growth", "No Growth")  # Add label for growth facet
  )

# Define custom colors for presence (grey for absent, blue for present)
custom_colors <- c("0" = "grey90", "1" = "steelblue")

cor_results <- selected_data_long %>%
  filter(KO != 'o') %>%  # Exclude rows where KO is 'o'
  group_by(KO) %>%  # Group by KO
  summarise(correlation = cor(present, growth_OD, method = "pearson", use = "complete.obs")) %>%  # Pearson correlation
  filter(!is.na(correlation))  # Remove rows where correlation is NA

# Obtain top 20 correlations
top_20_correlations_high <- cor_results %>%
  arrange(desc(correlation)) %>%  # Sort by correlation in descending order
  head(20)  # Get the top 20 with highest correlation

# Obtain top 50 correlations
top_50_correlations_high <- cor_results %>%
  arrange(desc(correlation)) %>%  # Sort by correlation in descending order
  head(50)  # Get the top 20 with highest correlation

# Plot the top 20 highest and lowest correlations
ggplot(top_20_correlations_high, aes(x = reorder(KO, correlation), y = correlation)) +
  geom_bar(stat = "identity", aes(fill = correlation), show.legend = FALSE) +  # Bar plot for correlations
  labs(
    title = "Top 20 KOs with Highest Correlations to Growth",
    x = "KO",
    y = "Pearson Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    axis.text.y = element_text(size = 10)
  )

# Step 1: Merge the data frames by KO
merged_df <- inner_join(
  cor_results %>% select(KO, correlation),
  p_values %>% select(KO, p_value_adjusted),
  by = "KO"
)

# Step 2: Transform the p-values
merged_df <- merged_df %>%
  mutate(
    neg_log10_pval = -log10(p_value_adjusted),
    significant = p_value_adjusted < 0.05 & abs(correlation) > 0.35  # Adjust thresholds as needed
  )

# Optional: Add labels for significant points
merged_df_significant <- merged_df %>% filter(significant)
ggplot(merged_df, aes(x = correlation, y = neg_log10_pval)) +
  geom_point(aes(color = significant), alpha = 0.8) +
  geom_text_repel(data = merged_df_significant, aes(label = KO), size = 3) +
  scale_color_manual(values = c("gray", "blue")) +
  geom_hline(yintercept = 1.30103, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 0.35, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -0.35, linetype = "dashed", color = "black") + 
  theme_minimal() +
  labs(
    title = "Volcano Plot",
    x = "Correlation",
    y = "-log10(p-value adjusted)",
    color = "Significant"
  ) +
  theme(legend.position = "top")

# Select KOs
table <- merged_df[merged_df$KO %in% c('K00884'), ]
intersect(top_50_correlations_high$KO, top_50_KOs$KO)
strains_significant <- unique(significant_ko_data$Name)
matched_strains_data <- strains_significant[strains_significant %in% allstrains_order]
matched_strains_ordered <- allstrains_order[allstrains_order %in% matched_strains_data]
significant_ko_data_ordered <- significant_ko_data[matched_strains_ordered,]

# Set the Name column in significant_ko_data1 to be a factor with levels in the desired order
significant_ko_data_ordered$Name <- factor(significant_ko_data_ordered$Name, levels = matched_strains_ordered)

# Arrange the data frame by the ordered Name column
significant_ko_data_ordered <- significant_ko_datae %>%
  arrange(Name)
significant_ko_data_ordered1 <- significant_ko_data_ordered[significant_ko_data_ordered$KO %in% top_50_KOs$KO, ]

# Plot heatmap with specified y-axis order
ggplot(significant_ko_data_ordered1, aes(x = KO, y = Name, fill = factor(present))) + 
  geom_tile(color = "grey") +  # Create tiles for heatmap
  scale_fill_manual(
    values = c("0" = "white", "1" = "steelblue", '2'= 'blue', '3'='blue',  
               '4'='blue',  '5'='blue',  '6'='blue', 
               '10'='blue',  '7'='blue',  '8'='blue'),
    name = "Presence",
    labels = c("Absent", "Present")
  ) + 
  facet_grid(growth_OD ~ ., scales = "free_y", labeller = labeller(growth_OD = c("0" = "No Growth", "1" = "Growth"))) +  # Arrange growth and no-growth vertically
  labs(
    title = "Top 50 Significant KOs aspartate t-test",
    x = "KO",
    y = "Strain"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  # Rotate x-axis labels
    axis.text.y = element_text(size = 1)  # Reduce size of y-axis labels for readability
  )
