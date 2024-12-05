# No-nitrogen control genomic data analysis between growth and no-growth group

# specific pathways
nitrate_nitrite <- c("K02568", 'K02567', 'K00367', 'K00360', 'K00370', 'K00371', 'K00372', 'K00374', 'K02567', 'K10534')  # Replace with the word/substring you're looking for
nitrite_ammonia <- c( "K00361", "K00362", "K00363", "K17877", "K26138", "K26139", "K00366", "K03385", "K04016", "K15876", "K00368", "K15864",
                      "K27148", "K02164", "K02305", "K02448", "K04561", "K04747", "K04748", "K15877", "K00376", "K00531", "K02586", "K02588",
                      "K02591", "K22896", "K22897", "K22898", "K22899")
glutamine_glutamate <- c('K00265', 'K00266', 'K00261', 'K00262', 'K00264', 'K00284', 'K01425', 
                         'K01954', 'K01955', 'K01956', 'K05597', 'K11540', 'K11541', 'K23265', 'K08590', 'K13566')

urea_deaminating <- c('K01427', 'K01428', 'K01429', 'K01430', 'K14048', 'K01941', 'K14541', 'K01457')
glutamate_ammonia <- c('K00260', 'K00261', 'K15371', 'K00262', 'K00265', 'K00266')

proline_glutamate <- c('K00294', 'K13821')
proline_deaminating <- c('K00824', 'K00811', 'K00812', 'K00813', 'K11358', 'K14454', 'K14455', 
                         'K00815' ,'K00817', 'K00832', 'K00838', 'K05821', 'K15849')
alanine_glutamate <- c('K00814', 'K14260')
glycine_alanine_glutamate <- c('K14272', 'K00827', 'K00830')
glycine_deaminating <- c('K00273', 'K00274', 'K11182', 'K00276', 'K13371', 'K13372', 'K25271', 
                         'K01752', 'K17989', 'K01754', 'K25272')

glycine_serine <- c('K00600', 'K00830')
alanine_serine <- c('K00830', 'K13290')
serine_deaminating <- c('K01753', 'K20498')


# Use Reduce with intersect to find the common elements in all the lists
nitrogen_kos <- Reduce(union, list(glutamate_ammonia, 
                                   proline_glutamate, 
                                   proline_deaminating, 
                                   alanine_glutamate, 
                                   glycine_alanine_glutamate, 
                                   glycine_deaminating, nitrate_nitrite,
                                   nitrite_ammonia, glutamine_glutamate, urea_deaminating, 
                                   glycine_serine, alanine_serine, serine_deaminating))

# Obtain data for no-N control of genomic and growth data
no_N_test <- combined_t_test_data[combined_t_test_data$Treatment == 'no N control + C', ]

# Drop missing values if present
no_N_test <- no_N_test %>% drop_na()

# Create a df with data of strains that grow on no-N control
no_N_testgrow <- no_N_test[no_N_test$growth_OD == 1,]

# Create a df with data of strains that do not grow on no-N control
no_N_testnogrow <- no_N_test[no_N_test$growth_OD == 0,]

# Check by hand the difference between KO presence values
no_N_testgrow$K01753
no_N_testnogrow$K01753

# Obtain list with names of KOs in growth group
KOs <- grep("^K", colnames(no_N_testgrow), value = TRUE)

# Create an empty data frame to store results
p_values <- data.frame(KO = character(), p_value = numeric())

# Loop through each KO (column) that starts with 'K'
for (ko in KOs) {
  group1 <- no_N_testgrow[[ko]]
  group2 <- no_N_testnogrow[[ko]]
  # Check if one of both groups has more than one unique value
  if (length(unique(group1)) > 1 || length(unique(group2)) > 1) {
    t_test_result <- t.test(group1, group2)
    p_values <- rbind(p_values, data.frame(KO = ko, p_value = t_test_result$p.value))
  } else {
    p_values <- rbind(p_values, data.frame(KO = ko, p_value = NA))
  }
}

# Name the p-values
p_values_noN <- p_values

# Obtain p-values that are in the selected nitrogen KOs list
p_values_non_order_nitro <- p_values[p_values$KO %in% intersect(nitrogen_kos, significant_KOs_noN$KO), ]

# See if there are KOs that are specific for growth
result_yes <- no_N_test_long_grow %>%
  group_by(KO) %>%
  summarise(
    all_present_positive = all(present > 0),  
    .groups = "drop"
  ) %>%
  filter(all_present_positive == TRUE)

# See if there are kos that are all 0 for no-growth group
result_no <- no_N_test_long_not_grow %>%
  group_by(KO) %>%
  summarise(
    all_present_positive = all(present == 0),  #
    .groups = "drop"
  ) %>%
  filter(all_present_positive == TRUE)

# See which KOs are in their intersection
KO_growth <- result_yes$KO
KO_nogrowth <- result_no$KO
selection_non_ko <- intersect(KO_growth, KO_nogrowth)

# Adjust for multiple testing using the Benjamini-Hochberg method
p_values_noN <- p_values_noN %>%
  mutate(
    p_value_adjusted = p.adjust(p_value, method = "BH") 
  )

# Filter for significant KOs with adjusted p-value < 0.05
significant_KOs_noN <- p_values_noN %>%
  filter(!is.na(p_value_adjusted) & p_value_adjusted < 0.05)

# See which KOs are significant and in the selected nitrogen conversion group
intersect(nitrogen_kos, significant_KOs_noN$KO)

# Filter the top 50 significant KOs based on adjusted p-value
top_50_KOs_noN <- significant_KOs_noN %>%
  arrange(p_value_adjusted) %>% 
  head(50) 

# Plot the top 15 significant KOs with their adjusted p-values
ggplot(top_50_KOs_noN, aes(x = reorder(KO, p_value_adjusted), y = p_value_adjusted)) +
  geom_bar(stat = "identity", aes(fill = p_value_adjusted), show.legend = FALSE) +  
  labs(
    title = "Top 50 Significant KOs no-N control",
    x = "KO",
    y = "Adjusted P-value (BH)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Filter your data to keep only the significant KOs
significant_ko_data_noN <- no_N_test_long %>%
  filter(KO %in% top_50_KOs_noN$KO) %>%
  mutate(
    growth_label = ifelse(growth_OD == 1, "Growth", "No Growth")  
  )

# Define custom colors for presence (grey for absent, blue for present)
custom_colors <- c("0" = "grey90", "1" = "steelblue")

# Perform pearson correlation based on growth or no growth and presence of KOs
cor_results_noN <- no_N_test_long %>%
  filter(KO != 'o') %>%  
  group_by(KO) %>% 
  summarise(correlation = cor(present, growth_OD, method = "pearson", use = "complete.obs")) %>%  
  filter(!is.na(correlation))  

# Obtain the top 20 most correlated KOs with growth on no-N control
top_20_correlations_high_noN <- cor_results_noN %>%
  arrange(desc(correlation)) %>% 
  head(20) 

# Obtain the top 50 most correlated KOs with growth on no-N control
top_50_correlations_high_noN <- cor_results_noN %>%
  arrange(desc(correlation)) %>%  
  head(50)  

# Plot the top 20 highest correlations
ggplot(top_20_correlations_high_noN, aes(x = reorder(KO, correlation), y = correlation)) +
  geom_bar(stat = "identity", aes(fill = correlation), show.legend = FALSE) + 
  labs(
    title = "Top 20 KOs with Highest Correlations to Growth no-N",
    x = "KO",
    y = "Pearson Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 10)
  )

# Merge correlation and t-test data
merged_df_non <- inner_join(
  cor_results_noN %>% select(KO, correlation),
  p_values_noN %>% select(KO, p_value_adjusted),
  by = "KO"
)

# Transform the p-values
merged_df_non <- merged_df_non %>%
  mutate(
    neg_log10_pval = -log10(p_value_adjusted),
    significant = p_value_adjusted < 0.05 & abs(correlation) > 0.3  # Adjust thresholds as needed
  )

# Plot volcano plot correlation and t-test
merged_df_significant <- merged_df_non %>% filter(significant)
ggplot(merged_df_non, aes(x = correlation, y = neg_log10_pval)) +
  geom_point(aes(color = significant), alpha = 0.8) +
  geom_text_repel(data = merged_df_significant, aes(label = KO), size = 3) +
  scale_color_manual(values = c("gray", "blue")) +
  geom_hline(yintercept = 1.30103, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 0.3, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -0.3, linetype = "dashed", color = "black") + 
  theme_minimal() +
  labs(
    title = "Volcano Plot",
    x = "Correlation",
    y = "-log10(p-value adjusted)",
    color = "Significant"
  ) +
  theme(legend.position = "top")

# Put the strains in taxonomic order
strains_significant_noN <- unique(significant_ko_data_noN$Name)
matched_strains_data_noN <- strains_significant_noN[strains_significant_noN %in% allstrains_order]
matched_strains_ordered_noN <- allstrains_order[allstrains_order %in% matched_strains_data_noN]
significant_ko_data_noN_ordered <- significant_ko_data_noN[matched_strains_ordered_noN,]
significant_ko_data_noN_ordered$Name <- factor(significant_ko_data_noN_ordered$Name, levels = matched_strains_ordered_noN)
significant_ko_data_noN_ordered <- significant_ko_data_noN %>%
  arrange(Name)

# Plot heatmap of significant KOs between growth and no-growth group
ggplot(significant_ko_data_noN_ordered, aes(x = KO, y = Name, fill = factor(present))) + 
  geom_tile(color = "grey") +  # Create tiles for heatmap
  scale_fill_manual(
    values = c("0" = "white", "1" = "steelblue"),
    name = "Presence",
    labels = c("Absent", "Present")
  ) + 
  facet_grid(growth_OD ~ ., scales = "free_y", labeller = labeller(growth_OD = c("0" = "No Growth", "1" = "Growth"))) +  # Arrange growth and no-growth vertically
  labs(
    title = "Top 50 Significant KOs no-N t-test",
    x = "KO",
    y = "Strain"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 5),  # Rotate x-axis labels
    axis.text.y = element_text(size = 2)  # Reduce size of y-axis labels for readability
  )

# Obtain KOs from correlation and t-test that are in selected nitrogen conversion pathways.
both_noN <- union(top_50_correlations_high_noN$KO, significant_KOs_noN$KO)
intersect(nitrogen_kos, ko_diff_noN)

# Filter data for KO 'K02014'
significantko <- no_N_test_long[no_N_test_long$KO %in% significant_KOs_noN$KO, ]

# Group by Description and growth_OD, then count the number of present genes (present == 1)
present_counts <- significantko %>%
  group_by(Description, growth_OD) %>%
  summarise(count_present = sum(present >= 1, na.rm = TRUE))

# Plot the result
ggplot(present_counts, aes(x = factor(Description, levels = desired_order), y = count_present, fill = factor(growth_OD))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count of Significant Genes no-N Growth",
       x = "",
       y = "Number of Present Genes",
       fill = "Growth") +
  scale_fill_manual(values = c("0" = "#FCA636FF", "1" = "steelblue"), 
                    labels = c("No Growth", "Growth")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Group by Description and growth_OD, then count the number of present genes
present_counts <- significantko %>%
  group_by(Description, growth_OD) %>%
  summarise(
    count_present = sum(present >= 1, na.rm = TRUE),  
    total_elements = n() 
  ) %>%
  # Merge with the unique name counts based on Description and growth_OD
  left_join(unique_names_per_description_info, by = "Description") %>%
  left_join(unique_names_per_description_no_info, by = "Description") %>%
  # Calculate the proportion present based on unique Name counts
  mutate(
    proportion_present = case_when(
      growth_OD == 1 ~ count_present / unique_names_count_info,  # For growth_OD == 1
      growth_OD == 0 ~ count_present / unique_names_count_no_info  # For growth_OD == 0
    )
  )

# Plot the result with two distinct colors from Cividis palette
ggplot(present_counts, aes(x = factor(Description, levels = desired_order), y = proportion_present, fill = factor(growth_OD))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Proportion of Present KOs per Strain (PPKS) no-N growth",
       x = "",
       y = "PPKS",
       fill = "Growth") + 
  scale_fill_manual(values = c("0" = "#3F4D76", "1" = "#FDCB24"), 
                    labels = c("No Growth", "Growth")) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
