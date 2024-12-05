

# Create column names
ko_columns1 <- c("strain", nitrate_nitrite, 'o')
ko_columns2 <- c("strain", nitrite_ammonia, 'o')
ko_columns4 <- c("strain", glutamine_glutamate, 'o')
ko_columns5 <- c("strain", urea_deaminating, 'o')
ko_columns6 <- c("strain", glutamate_ammonia, 'o')
ko_columns7 <- c("strain", alanine_deaminating, 'o')
ko_columns8 <- c("strain", proline_glutamate, 'o')
ko_columns9 <- c("strain", proline_deaminating, 'o')
ko_columns10 <- c("strain", alanine_glutamate, 'o')
ko_columns11 <- c("strain", glycine_alanine_glutamate, 'o')
ko_columns12 <- c("strain", glycine_deaminating, 'o')
ko_columns13 <- c("strain", glycine_serine, 'o')
ko_columns14 <- c("strain", alanine_serine, 'o')
ko_columns15 <- c("strain", serine_deaminating, 'o')

# Remove second column
emapper_final1 <- emapper_final[,-2]

# Add column names
filtered_df1 <- emapper_final1[, colnames(emapper_final1) %in% ko_columns1]
filtered_df2 <- emapper_final1[, colnames(emapper_final1) %in% ko_columns2]
filtered_df4 <- emapper_final1[, colnames(emapper_final1) %in% ko_columns4]
filtered_df5 <- emapper_final1[, colnames(emapper_final1) %in% ko_columns5]
filtered_df6 <- emapper_final1[, colnames(emapper_final1) %in% ko_columns6]
filtered_df7 <- emapper_final1[, colnames(emapper_final1) %in% ko_columns7]
filtered_df8 <- emapper_final1[, colnames(emapper_final1) %in% ko_columns8]
filtered_df9 <- emapper_final1[, colnames(emapper_final1) %in% ko_columns9]
filtered_df10 <- emapper_final1[, colnames(emapper_final1) %in% ko_columns10]
filtered_df11 <- emapper_final1[, colnames(emapper_final1) %in% ko_columns11]
filtered_df12 <- emapper_final1[, colnames(emapper_final1) %in% ko_columns12]
filtered_df13 <- emapper_final1[, colnames(emapper_final1) %in% ko_columns13]
filtered_df14 <- emapper_final1[, colnames(emapper_final1) %in% ko_columns14]
filtered_df15 <- emapper_final1[, colnames(emapper_final1) %in% ko_columns15]

# Calculate how many KOs are present
presence_matrix1 <- as.data.frame(lapply(filtered_df1[, c(-1)], function(x) x == 1))
presence_matrix1$has_ko <- rowSums(presence_matrix1) > 0
presence_matrix2 <- as.data.frame(lapply(filtered_df2[, c(-1)], function(x) x == 1))
presence_matrix2$has_ko <- rowSums(presence_matrix2) > 0
presence_matrix4 <- as.data.frame(lapply(filtered_df4[, c(-1)], function(x) x == 1))
presence_matrix4$has_ko <- rowSums(presence_matrix4) > 0
presence_matrix5 <- as.data.frame(lapply(filtered_df5[, c(-1)], function(x) x == 1))
presence_matrix5$has_ko <- rowSums(presence_matrix5) > 0
presence_matrix6 <- as.data.frame(lapply(filtered_df6[, c(-1)], function(x) x == 1))
presence_matrix6$has_ko <- rowSums(presence_matrix6) > 0
presence_matrix7 <- as.data.frame(lapply(filtered_df7[, c(-1)], function(x) x == 1))
presence_matrix7$has_ko <- rowSums(presence_matrix7) > 0
presence_matrix8 <- as.data.frame(lapply(filtered_df8[, c(-1)], function(x) x == 1))
presence_matrix8$has_ko <- rowSums(presence_matrix8) > 0
presence_matrix9 <- as.data.frame(lapply(filtered_df9[, c(-1)], function(x) x == 1))
presence_matrix9$has_ko <- rowSums(presence_matrix9) > 0
presence_matrix10 <- as.data.frame(lapply(filtered_df10[, c(-1)], function(x) x == 1))
presence_matrix10$has_ko <- rowSums(presence_matrix10) > 0
presence_matrix11 <- as.data.frame(lapply(filtered_df11[, c(-1)], function(x) x == 1))
presence_matrix11$has_ko <- rowSums(presence_matrix11) > 0
presence_matrix12 <- as.data.frame(lapply(filtered_df12[, c(-1)], function(x) x == 1))
presence_matrix12$has_ko <- rowSums(presence_matrix12) > 0
presence_matrix13 <- as.data.frame(lapply(filtered_df13[, c(-1)], function(x) x == 1))
presence_matrix13$has_ko <- rowSums(presence_matrix13) > 0
presence_matrix14 <- as.data.frame(lapply(filtered_df14[, c(-1)], function(x) x == 1))
presence_matrix14$has_ko <- rowSums(presence_matrix14) > 0
presence_matrix15 <- as.data.frame(lapply(filtered_df15[, c(-1)], function(x) x == 1))
presence_matrix15$has_ko <- rowSums(presence_matrix15) > 0

# Add the order column back
filtered_df1$has_ko <- presence_matrix1$has_ko
filtered_df2$has_ko <- presence_matrix2$has_ko
filtered_df4$has_ko <- presence_matrix4$has_ko
filtered_df5$has_ko <- presence_matrix5$has_ko
filtered_df6$has_ko <- presence_matrix6$has_ko
filtered_df7$has_ko <- presence_matrix7$has_ko
filtered_df8$has_ko <- presence_matrix8$has_ko
filtered_df9$has_ko <- presence_matrix9$has_ko
filtered_df10$has_ko <- presence_matrix10$has_ko
filtered_df11$has_ko <- presence_matrix11$has_ko
filtered_df12$has_ko <- presence_matrix12$has_ko
filtered_df13$has_ko <- presence_matrix13$has_ko
filtered_df14$has_ko <- presence_matrix14$has_ko
filtered_df15$has_ko <- presence_matrix15$has_ko


# Group by order and summarize
# Filter filtered_df1 to keep rows where strain matches Name in strain_order
filtered_df1 <- filtered_df1[filtered_df1$strain %in% strain_order$Name, ]
filtered_df1 <- merge(filtered_df1, strain_order, by.x = "strain", by.y = "Name")
summary_df1 <- filtered_df1 %>%
  group_by(Description) %>%
  summarize(num_unique_strains = length(unique(strain[has_ko])), .groups = 'drop')

# Step 1: Calculate the sum of the gene counts (1s) per strain
df_summed <- filtered_df1 %>%
  mutate(gene_count = rowSums(select(., starts_with("K"))))  # Sum the KO columns (K02568, K02567, etc.)

# Step 2: Group by the bacterial order and calculate the average number of genes
average_genes_per_order <- df_summed %>%
  group_by(Description) %>%
  summarise(
    total_genes = sum(gene_count),   # Total number of genes per order
    num_strains = n(),               # Number of strains per order
    avg_genes_per_strain = total_genes / num_strains  # Average number of genes per strain
  )

# Repeat for the other groups
filtered_df2 <- filtered_df2[filtered_df2$strain %in% strain_order$Name, ]
filtered_df2 <- merge(filtered_df2, strain_order, by.x = "strain", by.y = "Name")
summary_df2 <- filtered_df2 %>%
  group_by(Description) %>%
  summarize(num_unique_strains = length(unique(strain[has_ko])), .groups = 'drop')
filtered_df4 <- filtered_df4[filtered_df4$strain %in% strain_order$Name, ]
filtered_df4 <- merge(filtered_df4, strain_order, by.x = "strain", by.y = "Name")
summary_df4 <- filtered_df4 %>%
  group_by(Description) %>%
  summarize(num_unique_strains = length(unique(strain[has_ko])), .groups = 'drop')
filtered_df5 <- filtered_df5[filtered_df5$strain %in% strain_order$Name, ]
filtered_df5 <- merge(filtered_df5, strain_order, by.x = "strain", by.y = "Name")
summary_df5 <- filtered_df5 %>%
  group_by(Description) %>%
  summarize(num_unique_strains = length(unique(strain[has_ko])), .groups = 'drop')
filtered_df6 <- filtered_df6[filtered_df6$strain %in% strain_order$Name, ]
filtered_df6 <- merge(filtered_df6, strain_order, by.x = "strain", by.y = "Name")
summary_df6 <- filtered_df6 %>%
  group_by(Description) %>%
  summarize(num_unique_strains = length(unique(strain[has_ko])), .groups = 'drop')
filtered_df7 <- filtered_df7[filtered_df7$strain %in% strain_order$Name, ]
filtered_df7 <- merge(filtered_df7, strain_order, by.x = "strain", by.y = "Name")
summary_df7 <- filtered_df7 %>%
  group_by(Description) %>%
  summarize(num_unique_strains = length(unique(strain[has_ko])), .groups = 'drop')
filtered_df8 <- filtered_df8[filtered_df8$strain %in% strain_order$Name, ]
filtered_df8 <- merge(filtered_df8, strain_order, by.x = "strain", by.y = "Name")
summary_df8 <- filtered_df8 %>%
  group_by(Description) %>%
  summarize(num_unique_strains = length(unique(strain[has_ko])), .groups = 'drop')
filtered_df9 <- filtered_df9[filtered_df9$strain %in% strain_order$Name, ]
filtered_df9 <- merge(filtered_df9, strain_order, by.x = "strain", by.y = "Name")
summary_df9 <- filtered_df9 %>%
  group_by(Description) %>%
  summarize(num_unique_strains = length(unique(strain[has_ko])), .groups = 'drop')
filtered_df10 <- filtered_df10[filtered_df10$strain %in% strain_order$Name, ]
filtered_df10 <- merge(filtered_df10, strain_order, by.x = "strain", by.y = "Name")
summary_df10 <- filtered_df10 %>%
  group_by(Description) %>%
  summarize(num_unique_strains = length(unique(strain[has_ko])), .groups = 'drop')
filtered_df11 <- filtered_df11[filtered_df11$strain %in% strain_order$Name, ]
filtered_df11 <- merge(filtered_df11, strain_order, by.x = "strain", by.y = "Name")
summary_df11 <- filtered_df11 %>%
  group_by(Description) %>%
  summarize(num_unique_strains = length(unique(strain[has_ko])), .groups = 'drop')
filtered_df12 <- filtered_df12[filtered_df12$strain %in% strain_order$Name, ]
filtered_df12 <- merge(filtered_df12, strain_order, by.x = "strain", by.y = "Name")
summary_df12 <- filtered_df12 %>%
  group_by(Description) %>%
  summarize(num_unique_strains = length(unique(strain[has_ko])), .groups = 'drop')
filtered_df13 <- filtered_df13[filtered_df13$strain %in% strain_order$Name, ]
filtered_df13 <- merge(filtered_df13, strain_order, by.x = "strain", by.y = "Name")
summary_df13 <- filtered_df13 %>%
  group_by(Description) %>%
  summarize(num_unique_strains = length(unique(strain[has_ko])), .groups = 'drop')
filtered_df14 <- filtered_df14[filtered_df14$strain %in% strain_order$Name, ]
filtered_df14 <- merge(filtered_df14, strain_order, by.x = "strain", by.y = "Name")
summary_df14 <- filtered_df14 %>%
  group_by(Description) %>%
  summarize(num_unique_strains = length(unique(strain[has_ko])), .groups = 'drop')
filtered_df15 <- filtered_df15[filtered_df15$strain %in% strain_order$Name, ]
filtered_df15 <- merge(filtered_df15, strain_order, by.x = "strain", by.y = "Name")
summary_df15 <- filtered_df15 %>%
  group_by(Description) %>%
  summarize(num_unique_strains = length(unique(strain[has_ko])), .groups = 'drop')

# Merge both dataframes per group
merged_df1 <- dplyr::left_join(summary_df1, order_strain_counts, by = c("Description" = "o"))
merged_df1 <- merged_df1 %>%
  dplyr::mutate(proportion = num_unique_strains / n_strains)
merged_df2 <- dplyr::left_join(summary_df2, order_strain_counts, by = c("Description" = "o"))
merged_df2 <- merged_df2 %>%
  dplyr::mutate(proportion = num_unique_strains / n_strains)
merged_df4 <- dplyr::left_join(summary_df4, order_strain_counts, by = c("Description" = "o"))
merged_df4 <- merged_df4 %>%
  dplyr::mutate(proportion = num_unique_strains / n_strains)
merged_df5 <- dplyr::left_join(summary_df5, order_strain_counts, by = c("Description" = "o"))
merged_df5 <- merged_df5 %>%
  dplyr::mutate(proportion = num_unique_strains / n_strains)
merged_df6 <- dplyr::left_join(summary_df6, order_strain_counts, by = c("Description" = "o"))
merged_df6 <- merged_df6 %>%
  dplyr::mutate(proportion = num_unique_strains / n_strains)
merged_df7 <- dplyr::left_join(summary_df7, order_strain_counts, by = c("Description" = "o"))
merged_df7 <- merged_df7 %>%
  dplyr::mutate(proportion = num_unique_strains / n_strains)
merged_df8 <- dplyr::left_join(summary_df8, order_strain_counts, by = c("Description" = "o"))
merged_df8 <- merged_df8 %>% 
  dplyr::mutate(proportion = num_unique_strains / n_strains)
merged_df9 <- dplyr::left_join(summary_df9, order_strain_counts, by = c("Description" = "o"))
merged_df9 <- merged_df9 %>% 
  dplyr::mutate(proportion = num_unique_strains / n_strains)
merged_df10 <- dplyr::left_join(summary_df10, order_strain_counts, by = c("Description" = "o"))
merged_df10 <- merged_df10 %>% 
  dplyr::mutate(proportion = num_unique_strains / n_strains)
merged_df11 <- dplyr::left_join(summary_df11, order_strain_counts, by = c("Description" = "o"))
merged_df11 <- merged_df11 %>% 
  dplyr::mutate(proportion = num_unique_strains / n_strains)
merged_df12 <- dplyr::left_join(summary_df12, order_strain_counts, by = c("Description" = "o"))
merged_df12 <- merged_df12 %>% 
  dplyr::mutate(proportion = num_unique_strains / n_strains)
merged_df13 <- dplyr::left_join(summary_df13, order_strain_counts, by = c("Description" = "o"))
merged_df13 <- merged_df13 %>% 
  dplyr::mutate(proportion = num_unique_strains / n_strains)
merged_df14 <- dplyr::left_join(summary_df14, order_strain_counts, by = c("Description" = "o"))
merged_df14 <- merged_df14 %>% 
  dplyr::mutate(proportion = num_unique_strains / n_strains)
merged_df15 <- dplyr::left_join(summary_df15, order_strain_counts, by = c("Description" = "o"))
merged_df15 <- merged_df15 %>% 
  dplyr::mutate(proportion = num_unique_strains / n_strains)

# Rename
selected_rows1 <- merged_df1
selected_rows2 <- merged_df2
selected_rows4 <- merged_df4
selected_rows5 <- merged_df5
selected_rows6 <- merged_df6
selected_rows7 <- merged_df7
selected_rows8 <- merged_df8
selected_rows9 <- merged_df9
selected_rows10 <- merged_df10
selected_rows11 <- merged_df11
selected_rows12 <- merged_df12
selected_rows13 <- merged_df13
selected_rows14 <- merged_df14
selected_rows15 <- merged_df15

# Plot the data
ggplot(merged_df2, aes(x = Description, y = proportion)) +
  geom_bar(stat = "identity", fill = '#2A788EFF') +
  labs(title = "Proportion of Strains with KO's of nitrite to ammonia degradation",
       x = "Bacterial Order",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0,1)

# Add pathway name
selected_rows1 <- merged_df1 %>%
  mutate(source = "Nitrate to Nitrite (10)")
selected_rows2 <- merged_df2 %>%
  mutate(source = "Nitrite to Ammonia (29)")
selected_rows4 <- merged_df4 %>%
  mutate(source = "Glutamate to Glutamine (16)")
selected_rows5 <- merged_df5 %>%
  mutate(source = "Urea deaminating (8)")
selected_rows6 <- merged_df6 %>%
  mutate(source = "Glutamate to Ammonia (6)")
selected_rows7 <- merged_df7 %>%
  mutate(source = "Alanine deaminating (5)")
selected_rows8 <- merged_df8 %>% 
  mutate(source = "Proline to Glutamate (2)")
selected_rows9 <- merged_df9 %>% 
  mutate(source = "Proline Deaminating (13)")
selected_rows10 <- merged_df10 %>% 
  mutate(source = "Alanine to Glutamate (2)")
selected_rows11 <- merged_df11 %>% 
  mutate(source = "Glycine, Alanine, Glutamate (3)")
selected_rows12 <- merged_df12 %>% 
  mutate(source = "Glycine Deaminating (11)")
selected_rows13 <- merged_df13 %>% 
  mutate(source = "Glycine to Serine (2)")
selected_rows14 <- merged_df14 %>% 
  mutate(source = "Alanine to Serine (2)")
selected_rows15 <- merged_df15 %>% 
  mutate(source = "Serine Deaminating (5)")


# Combine in groups
combined_df1 <- bind_rows(selected_rows1, selected_rows2, selected_rows4, selected_rows5,
                         selected_rows6)
combined_df2 <- bind_rows(selected_rows7, selected_rows8, selected_rows9, selected_rows10)
combined_df21 <- bind_rows(selected_rows11, selected_rows12, selected_rows13, selected_rows14, selected_rows15)
inroganic_plotko <- bind_rows(selected_rows1, selected_rows2, selected_rows5)
all_kos_list <- bind_rows(selected_rows1, selected_rows2, selected_rows4, selected_rows5,
                          selected_rows6, selected_rows7, selected_rows8, selected_rows9, selected_rows10,
                          selected_rows11, selected_rows12, selected_rows13, selected_rows14, selected_rows15)


# Plot Proportion of Strains with KOs for Inorganic Nitrogen Conversion
ggplot(inroganic_plotko, aes(x = Description, y = proportion, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Strains with KOs for Inorganic Nitrogen Conversion",
       x = "Bacterial Order",
       y = "Proportion", 
       fill = 'Conversion') +
  scale_fill_viridis_d(option = 'cividis')+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 1)

# Create the plot: Proportion of Strains with KOs for Nitrogen Conversion
ggplot(combined_df2, aes(x = Description, y = proportion, fill = source)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Proportion of Strains with KOs for Nitrogen Conversion", 
       x = "Bacterial Order", 
       y = "Proportion", 
       fill = 'Conversion') + 
  scale_fill_viridis_d(option = 'cividis')+
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylim(0, 1)

# Determine colours
colors <- c(brewer.pal(9, "Set1"), colors <- brewer.pal(5, "Set2"))

# Create the plot
ggplot(all_kos_list, aes(x = proportion, y = Description, fill = source)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Fraction of Strains with KOs for Nitrogen Conversion", 
       x = "Fraction", 
       y = "Order", 
       fill = 'Conversion') + 
  scale_fill_manual(values = colors)+
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(reverse = TRUE))

# Create long df
combined_test_long <- combined_t_test_data %>%   
  select(Name, Treatment, growth_OD, max_derivative, Description, everything()) %>%  # Keep the growth column and KOs   
  gather(key = "KO", value = "present", -c(Name, Treatment, growth_OD, max_derivative, max_OD, Description))  # Reshape to long format  

# Keep only the relevant columns and unique rows for each Name and KO
filtered_combined_test_long <- combined_test_long %>%
  select(Name, KO, Description, present) %>%  # Select only the desired columns
  distinct(Name, KO, .keep_all = TRUE)  # Keep unique combinations of Name and KO

# remove third row
filtered_combined_test_long <- filtered_combined_test_long[-3,]

# Select specific conditions
combined_t_test_data_nitrate <- filtered_combined_test_long[filtered_combined_test_long$KO %in% nitrate_nitrite, ]
combined_t_test_data_nitrite <- filtered_combined_test_long[filtered_combined_test_long$KO %in% nitrite_ammonia, ]
combined_t_test_data_urea <- filtered_combined_test_long[filtered_combined_test_long$KO %in% urea_deaminating, ]

# Summarize the data
ko_sum_per_description_nitrate <- combined_t_test_data_nitrate %>%
  group_by(Description) %>%
  summarise(total_KOs = sum(present, na.rm = TRUE))  # Sum the KOs present

# Summarize the data
ko_sum_per_description_nitrite <- combined_t_test_data_nitrite %>%
  group_by(Description) %>%
  summarise(total_KOs = sum(present, na.rm = TRUE))  # Sum the KOs present

# Summarize the data
ko_sum_per_description_urea <- combined_t_test_data_urea %>%
  group_by(Description) %>%
  summarise(total_KOs = sum(present, na.rm = TRUE))  # Sum the KOs present

# Add a column to indicate the nitrogen source for each dataset
ko_sum_per_description_nitrate <- ko_sum_per_description_nitrate %>%
  mutate(Source = "Nitrate")
ko_sum_per_description_nitrite <- ko_sum_per_description_nitrite %>%
  mutate(Source = "Nitrite")
ko_sum_per_description_urea <- ko_sum_per_description_urea %>%
  mutate(Source = "Urea")

# Combine the three datasets
combined_ko_sum <- bind_rows(
  ko_sum_per_description_nitrate,
  ko_sum_per_description_nitrite,
  ko_sum_per_description_urea
)

# Plot the combined data
ggplot(combined_ko_sum, aes(x = factor(Description, levels = desired_order), y = total_KOs, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +  # Create a bar plot with grouped bars
  scale_fill_viridis_d(option = "E") +  # Apply Viridis discrete color scale
  labs(
    title = "Total KOs Selected from Inorganic Conversion Pathways",
    x = " ",
    y = "Total KOs Present",
    fill = "Nitrogen Pathway"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# List of filtered and presence_matrix data
filtered_dfs <- list(filtered_df1, filtered_df2, filtered_df4, filtered_df5, 
                     filtered_df6, filtered_df7, filtered_df8, filtered_df9, 
                     filtered_df10, filtered_df11, filtered_df12, filtered_df13, 
                     filtered_df14, filtered_df15)

# Add count of KOs behind name
sources <- c("Nitrate to Nitrite (10)", "Nitrite to Ammonia (29)", "Glutamate to Glutamine (16)", 
             "Urea deaminating (8)", "Glutamate to Ammonia (6)", "Alanine deaminating (5)", 
             "Proline to Glutamate (2)", "Proline Deaminating (13)", "Alanine to Glutamate (2)", 
             "Glycine, Alanine, Glutamate (3)", "Glycine Deaminating (11)", "Glycine to Serine (2)", 
             "Alanine to Serine (2)", "Serine Deaminating (5)")

# Add the treatment column to each dataframe in filtered_dfs
filtered_dfs <- lapply(seq_along(filtered_dfs), function(i) {
  filtered_df <- filtered_dfs[[i]]
  filtered_df$treatment <- sources[i]  # Add the treatment name
  return(filtered_df)
})

# Step 1: Add has_ko and calculate averages
processed_dfs <- lapply(seq_along(filtered_dfs), function(i) {
  filtered_df <- filtered_dfs[[i]]
  
  # Count the number of columns starting with "K"
  num_k_columns <- ncol(select(filtered_df, starts_with("K")))
  
  # Calculate average number of genes per strain for each order
  filtered_df %>%
    mutate(gene_count = rowSums(select(., starts_with("K")))) %>%
    group_by(Description) %>%
    summarise(
      total_genes = sum(gene_count),
      num_strains = n(),
      kcol = num_k_columns,
      avg_genes_per_strain = total_genes / num_strains,
      avg_genes_per_k = avg_genes_per_strain / num_k_columns # Adjust by dividing by K columns
    ) %>%
    mutate(source = sources[i]) # Add source label
})

# Step 2: Combine all datasets
combined_df <- bind_rows(processed_dfs)

# Step 3: Plot the data
ggplot(combined_df, aes(x = Description, y = avg_genes_per_k, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Number of Genes per Strain by Bacterial Order",
    x = "Bacterial Order",
    y = "Average Genes per Strain",
    fill = "Source"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position='none') +
  scale_fill_manual(values = colors) +
  guides(fill = guide_legend(reverse = TRUE))

ggplot(combined_df, aes(x = avg_genes_per_k, y = Description, fill = source)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(
    title = "Average Number of Genes per Strain",
    x = "Average Number of Genes per Strain",
    y = "Bacterial Order",
    fill = "Source"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),  # Adjust label angles for y-axis
    legend.position = 'none'
  ) + 
  scale_fill_manual(values = colors) + 
  guides(fill = guide_legend(reverse = TRUE))


#Step 1: Calculate number of distinct strains per Description
num_strains_per_desc <- filtered_df1 %>%
  group_by(Description) %>%
  summarise(num_strains = n_distinct(strain), .groups = "drop")  # Calculate distinct strains per Description

# Step 2: Summarize KO occurrences per Description
ko_occurrence_per_strain1 <- filtered_df1 %>%
  group_by(Description) %>%
  summarise(across(starts_with("K"), sum), .groups = "drop") %>%  # Sum KOs per Description
  pivot_longer(cols = starts_with("K"),         # Reshape the data to long format
               names_to = "KO", 
               values_to = "Occurrence") %>%   # Gather KO columns into two: KO and Occurrence
  left_join(num_strains_per_desc, by = "Description") %>%  # Join the number of strains per Description
  mutate(Occurrence_per_strain = Occurrence / num_strains)  # Calculate Occurrence per strain

# Plot the distribution of KO occurrences per strain across Descriptions
ggplot(ko_occurrence_per_strain1, aes(x = KO, y = Occurrence_per_strain, fill = Description)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use 'dodge' for side-by-side bars for each Description
  labs(title = "KO occurrence per strain nitrate to nitrite", 
       x = "KO", 
       y = "Average occurrence per strain", 
       fill = "Order") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        legend.position = "right") +
  scale_fill_manual(values = color)  # Optional: use a color palette for better visualization


#Step 1: Calculate number of distinct strains per Description
num_strains_per_desc2 <- filtered_df2 %>%
  group_by(Description) %>%
  summarise(num_strains = n_distinct(strain), .groups = "drop")  # Calculate distinct strains per Description

# Step 2: Summarize KO occurrences per Description
ko_occurrence_per_strain2 <- filtered_df2 %>%
  group_by(Description) %>%
  summarise(across(starts_with("K"), sum), .groups = "drop") %>%  # Sum KOs per Description
  pivot_longer(cols = starts_with("K"),         # Reshape the data to long format
               names_to = "KO", 
               values_to = "Occurrence") %>%   # Gather KO columns into two: KO and Occurrence
  left_join(num_strains_per_desc2, by = "Description") %>%  # Join the number of strains per Description
  mutate(Occurrence_per_strain = Occurrence / num_strains)  # Calculate Occurrence per strain

# Plot the distribution of KO occurrences per strain across Descriptions
ggplot(ko_occurrence_per_strain2, aes(x = KO, y = Occurrence_per_strain, fill = Description)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use 'dodge' for side-by-side bars for each Description
  labs(title = "KO occurrence per strain nitrite ammonia", 
       x = "KO", 
       y = "Average occurrence per strain", 
       fill = "Order") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        legend.position = "right") +
  scale_fill_manual(values = color)  # Optional: use a color palette for better visualization

#Step 1: Calculate number of distinct strains per Description
num_strains_per_desc4 <- filtered_df4 %>%
  group_by(Description) %>%
  summarise(num_strains = n_distinct(strain), .groups = "drop")  # Calculate distinct strains per Description

# Step 2: Summarize KO occurrences per Description
ko_occurrence_per_strain4 <- filtered_df4 %>%
  group_by(Description) %>%
  summarise(across(starts_with("K"), sum), .groups = "drop") %>%  # Sum KOs per Description
  pivot_longer(cols = starts_with("K"),         # Reshape the data to long format
               names_to = "KO", 
               values_to = "Occurrence") %>%   # Gather KO columns into two: KO and Occurrence
  left_join(num_strains_per_desc4, by = "Description") %>%  # Join the number of strains per Description
  mutate(Occurrence_per_strain = Occurrence / num_strains)  # Calculate Occurrence per strain

# Plot the distribution of KO occurrences per strain across Descriptions
ggplot(ko_occurrence_per_strain4, aes(x = KO, y = Occurrence_per_strain, fill = Description)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use 'dodge' for side-by-side bars for each Description
  labs(title = "KO occurrence per strain glutamine to glutamate", 
       x = "KO", 
       y = "Average occurrence per strain", 
       fill = "Order") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        legend.position = "right") +
  scale_fill_manual(values = color)  # Optional: use a color palette for better visualization

#Step 1: Calculate number of distinct strains per Description
num_strains_per_desc5 <- filtered_df5 %>%
  group_by(Description) %>%
  summarise(num_strains = n_distinct(strain), .groups = "drop")  # Calculate distinct strains per Description

# Step 2: Summarize KO occurrences per Description
ko_occurrence_per_strain5 <- filtered_df5 %>%
  group_by(Description) %>%
  summarise(across(starts_with("K"), sum), .groups = "drop") %>%  # Sum KOs per Description
  pivot_longer(cols = starts_with("K"),         # Reshape the data to long format
               names_to = "KO", 
               values_to = "Occurrence") %>%   # Gather KO columns into two: KO and Occurrence
  left_join(num_strains_per_desc5, by = "Description") %>%  # Join the number of strains per Description
  mutate(Occurrence_per_strain = Occurrence / num_strains)  # Calculate Occurrence per strain

# Plot the distribution of KO occurrences per strain across Descriptions
ggplot(ko_occurrence_per_strain5, aes(x = KO, y = Occurrence_per_strain, fill = Description)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use 'dodge' for side-by-side bars for each Description
  labs(title = "KO occurrence per strain urea", 
       x = "KO", 
       y = "Average occurrence per strain", 
       fill = "Order") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        legend.position = "right") +
  scale_fill_manual(values = color)  # Optional: use a color palette for better visualization

#Step 1: Calculate number of distinct strains per Description
num_strains_per_desc6 <- filtered_df6 %>%
  group_by(Description) %>%
  summarise(num_strains = n_distinct(strain), .groups = "drop")  # Calculate distinct strains per Description

# Step 2: Summarize KO occurrences per Description
ko_occurrence_per_strain6 <- filtered_df6 %>%
  group_by(Description) %>%
  summarise(across(starts_with("K"), sum), .groups = "drop") %>%  # Sum KOs per Description
  pivot_longer(cols = starts_with("K"),         # Reshape the data to long format
               names_to = "KO", 
               values_to = "Occurrence") %>%   # Gather KO columns into two: KO and Occurrence
  left_join(num_strains_per_desc6, by = "Description") %>%  # Join the number of strains per Description
  mutate(Occurrence_per_strain = Occurrence / num_strains)  # Calculate Occurrence per strain

# Plot the distribution of KO occurrences per strain across Descriptions
ggplot(ko_occurrence_per_strain6, aes(x = KO, y = Occurrence_per_strain, fill = Description)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use 'dodge' for side-by-side bars for each Description
  labs(title = "KO occurrence per strain glutamate ammonia", 
       x = "KO", 
       y = "Average occurrence per strain", 
       fill = "Order") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        legend.position = "right") +
  scale_fill_manual(values = color)  # Optional: use a color palette for better visualization

#Step 1: Calculate number of distinct strains per Description
num_strains_per_desc7 <- filtered_df7 %>%
  group_by(Description) %>%
  summarise(num_strains = n_distinct(strain), .groups = "drop")  # Calculate distinct strains per Description

# Step 2: Summarize KO occurrences per Description
ko_occurrence_per_strain7 <- filtered_df7 %>%
  group_by(Description) %>%
  summarise(across(starts_with("K"), sum), .groups = "drop") %>%  # Sum KOs per Description
  pivot_longer(cols = starts_with("K"),         # Reshape the data to long format
               names_to = "KO", 
               values_to = "Occurrence") %>%   # Gather KO columns into two: KO and Occurrence
  left_join(num_strains_per_desc7, by = "Description") %>%  # Join the number of strains per Description
  mutate(Occurrence_per_strain = Occurrence / num_strains)  # Calculate Occurrence per strain

# Plot the distribution of KO occurrences per strain across Descriptions
ggplot(ko_occurrence_per_strain7, aes(x = KO, y = Occurrence_per_strain, fill = Description)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use 'dodge' for side-by-side bars for each Description
  labs(title = "KO occurrence per strain  alanine", 
       x = "KO", 
       y = "Average occurrence per strain", 
       fill = "Order") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        legend.position = "right") +
  scale_fill_manual(values = color)  # Optional: use a color palette for better visualization

#Step 1: Calculate number of distinct strains per Description
num_strains_per_desc8 <- filtered_df8 %>%
  group_by(Description) %>%
  summarise(num_strains = n_distinct(strain), .groups = "drop")  # Calculate distinct strains per Description

# Step 2: Summarize KO occurrences per Description
ko_occurrence_per_strain8 <- filtered_df8 %>%
  group_by(Description) %>%
  summarise(across(starts_with("K"), sum), .groups = "drop") %>%  # Sum KOs per Description
  pivot_longer(cols = starts_with("K"),         # Reshape the data to long format
               names_to = "KO", 
               values_to = "Occurrence") %>%   # Gather KO columns into two: KO and Occurrence
  left_join(num_strains_per_desc8, by = "Description") %>%  # Join the number of strains per Description
  mutate(Occurrence_per_strain = Occurrence / num_strains)  # Calculate Occurrence per strain

# Plot the distribution of KO occurrences per strain across Descriptions
ggplot(ko_occurrence_per_strain8, aes(x = KO, y = Occurrence_per_strain, fill = Description)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use 'dodge' for side-by-side bars for each Description
  labs(title = "KO occurrence per strain proline", 
       x = "KO", 
       y = "Average occurrence per strain", 
       fill = "Order") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        legend.position = "right") +
  scale_fill_manual(values = color)  # Optional: use a color palette for better visualization

#Step 1: Calculate number of distinct strains per Description
num_strains_per_desc9 <- filtered_df9 %>%
  group_by(Description) %>%
  summarise(num_strains = n_distinct(strain), .groups = "drop")  # Calculate distinct strains per Description

# Step 2: Summarize KO occurrences per Description
ko_occurrence_per_strain9 <- filtered_df9 %>%
  group_by(Description) %>%
  summarise(across(starts_with("K"), sum), .groups = "drop") %>%  # Sum KOs per Description
  pivot_longer(cols = starts_with("K"),         # Reshape the data to long format
               names_to = "KO", 
               values_to = "Occurrence") %>%   # Gather KO columns into two: KO and Occurrence
  left_join(num_strains_per_desc9, by = "Description") %>%  # Join the number of strains per Description
  mutate(Occurrence_per_strain = Occurrence / num_strains)  # Calculate Occurrence per strain

# Plot the distribution of KO occurrences per strain across Descriptions
ggplot(ko_occurrence_per_strain9, aes(x = KO, y = Occurrence_per_strain, fill = Description)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use 'dodge' for side-by-side bars for each Description
  labs(title = "KO occurrence per strain proline deaminating", 
       x = "KO", 
       y = "Average occurrence per strain", 
       fill = "Order") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        legend.position = "right") +
  scale_fill_manual(values = color)  # Optional: use a color palette for better visualization

#Step 1: Calculate number of distinct strains per Description
num_strains_per_desc10 <- filtered_df10 %>%
  group_by(Description) %>%
  summarise(num_strains = n_distinct(strain), .groups = "drop")  # Calculate distinct strains per Description

# Step 2: Summarize KO occurrences per Description
ko_occurrence_per_strain10 <- filtered_df10 %>%
  group_by(Description) %>%
  summarise(across(starts_with("K"), sum), .groups = "drop") %>%  # Sum KOs per Description
  pivot_longer(cols = starts_with("K"),         # Reshape the data to long format
               names_to = "KO", 
               values_to = "Occurrence") %>%   # Gather KO columns into two: KO and Occurrence
  left_join(num_strains_per_desc10, by = "Description") %>%  # Join the number of strains per Description
  mutate(Occurrence_per_strain = Occurrence / num_strains)  # Calculate Occurrence per strain

# Plot the distribution of KO occurrences per strain across Descriptions
ggplot(ko_occurrence_per_strain10, aes(x = KO, y = Occurrence_per_strain, fill = Description)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use 'dodge' for side-by-side bars for each Description
  labs(title = "KO occurrence per strain alanine glutamate", 
       x = "KO", 
       y = "Average occurrence per strain", 
       fill = "Order") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        legend.position = "right") +
  scale_fill_manual(values = color)  # Optional: use a color palette for better visualization

#Step 1: Calculate number of distinct strains per Description
num_strains_per_desc11 <- filtered_df11 %>%
  group_by(Description) %>%
  summarise(num_strains = n_distinct(strain), .groups = "drop")  # Calculate distinct strains per Description

# Step 2: Summarize KO occurrences per Description
ko_occurrence_per_strain11 <- filtered_df11 %>%
  group_by(Description) %>%
  summarise(across(starts_with("K"), sum), .groups = "drop") %>%  # Sum KOs per Description
  pivot_longer(cols = starts_with("K"),         # Reshape the data to long format
               names_to = "KO", 
               values_to = "Occurrence") %>%   # Gather KO columns into two: KO and Occurrence
  left_join(num_strains_per_desc11, by = "Description") %>%  # Join the number of strains per Description
  mutate(Occurrence_per_strain = Occurrence / num_strains)  # Calculate Occurrence per strain

# Plot the distribution of KO occurrences per strain across Descriptions
ggplot(ko_occurrence_per_strain11, aes(x = KO, y = Occurrence_per_strain, fill = Description)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use 'dodge' for side-by-side bars for each Description
  labs(title = "KO occurrence per strain glycine_alanine_glutamate", 
       x = "KO", 
       y = "Average occurrence per strain", 
       fill = "Order") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        legend.position = "right") +
  scale_fill_manual(values = color)  # Optional: use a color palette for better visualization

#Step 1: Calculate number of distinct strains per Description
num_strains_per_desc12 <- filtered_df12 %>%
  group_by(Description) %>%
  summarise(num_strains = n_distinct(strain), .groups = "drop")  # Calculate distinct strains per Description

# Step 2: Summarize KO occurrences per Description
ko_occurrence_per_strain12 <- filtered_df12 %>%
  group_by(Description) %>%
  summarise(across(starts_with("K"), sum), .groups = "drop") %>%  # Sum KOs per Description
  pivot_longer(cols = starts_with("K"),         # Reshape the data to long format
               names_to = "KO", 
               values_to = "Occurrence") %>%   # Gather KO columns into two: KO and Occurrence
  left_join(num_strains_per_desc12, by = "Description") %>%  # Join the number of strains per Description
  mutate(Occurrence_per_strain = Occurrence / num_strains)  # Calculate Occurrence per strain

# Plot the distribution of KO occurrences per strain across Descriptions
ggplot(ko_occurrence_per_strain12, aes(x = KO, y = Occurrence_per_strain, fill = Description)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use 'dodge' for side-by-side bars for each Description
  labs(title = "KO occurrence per strain glycine_deaminating", 
       x = "KO", 
       y = "Average occurrence per strain", 
       fill = "Order") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        legend.position = "right") +
  scale_fill_manual(values = color)  # Optional: use a color palette for better visualization


#Step 1: Calculate number of distinct strains per Description
num_strains_per_desc13 <- filtered_df13 %>%
  group_by(Description) %>%
  summarise(num_strains = n_distinct(strain), .groups = "drop")  # Calculate distinct strains per Description

# Step 2: Summarize KO occurrences per Description
ko_occurrence_per_strain13 <- filtered_df13 %>%
  group_by(Description) %>%
  summarise(across(starts_with("K"), sum), .groups = "drop") %>%  # Sum KOs per Description
  pivot_longer(cols = starts_with("K"),         # Reshape the data to long format
               names_to = "KO", 
               values_to = "Occurrence") %>%   # Gather KO columns into two: KO and Occurrence
  left_join(num_strains_per_desc13, by = "Description") %>%  # Join the number of strains per Description
  mutate(Occurrence_per_strain = Occurrence / num_strains)  # Calculate Occurrence per strain

# Plot the distribution of KO occurrences per strain across Descriptions
ggplot(ko_occurrence_per_strain13, aes(x = KO, y = Occurrence_per_strain, fill = Description)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use 'dodge' for side-by-side bars for each Description
  labs(title = "KO occurrence per strain glycine_serine", 
       x = "KO", 
       y = "Average occurrence per strain", 
       fill = "Order") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        legend.position = "right") +
  scale_fill_manual(values = color)  # Optional: use a color palette for better visualization

#Step 1: Calculate number of distinct strains per Description
num_strains_per_desc14 <- filtered_df14 %>%
  group_by(Description) %>%
  summarise(num_strains = n_distinct(strain), .groups = "drop")  # Calculate distinct strains per Description

# Step 2: Summarize KO occurrences per Description
ko_occurrence_per_strain14 <- filtered_df14 %>%
  group_by(Description) %>%
  summarise(across(starts_with("K"), sum), .groups = "drop") %>%  # Sum KOs per Description
  pivot_longer(cols = starts_with("K"),         # Reshape the data to long format
               names_to = "KO", 
               values_to = "Occurrence") %>%   # Gather KO columns into two: KO and Occurrence
  left_join(num_strains_per_desc14, by = "Description") %>%  # Join the number of strains per Description
  mutate(Occurrence_per_strain = Occurrence / num_strains)  # Calculate Occurrence per strain

# Plot the distribution of KO occurrences per strain across Descriptions
ggplot(ko_occurrence_per_strain14, aes(x = KO, y = Occurrence_per_strain, fill = Description)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use 'dodge' for side-by-side bars for each Description
  labs(title = "KO occurrence per strain alanine_serine", 
       x = "KO", 
       y = "Average occurrence per strain", 
       fill = "Order") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        legend.position = "right") +
  scale_fill_manual(values = color)  # Optional: use a color palette for better visualization

#Step 1: Calculate number of distinct strains per Description
num_strains_per_desc15 <- filtered_df15 %>%
  group_by(Description) %>%
  summarise(num_strains = n_distinct(strain), .groups = "drop")  # Calculate distinct strains per Description

# Step 2: Summarize KO occurrences per Description
ko_occurrence_per_strain15 <- filtered_df15 %>%
  group_by(Description) %>%
  summarise(across(starts_with("K"), sum), .groups = "drop") %>%  # Sum KOs per Description
  pivot_longer(cols = starts_with("K"),         # Reshape the data to long format
               names_to = "KO", 
               values_to = "Occurrence") %>%   # Gather KO columns into two: KO and Occurrence
  left_join(num_strains_per_desc15, by = "Description") %>%  # Join the number of strains per Description
  mutate(Occurrence_per_strain = Occurrence / num_strains)  # Calculate Occurrence per strain

# Plot the distribution of KO occurrences per strain across Descriptions
ggplot(ko_occurrence_per_strain15, aes(x = KO, y = Occurrence_per_strain, fill = Description)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use 'dodge' for side-by-side bars for each Description
  labs(title = "KO occurrence per strain serine_deaminating", 
       x = "KO", 
       y = "Average occurrence per strain", 
       fill = "Order") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        legend.position = "right") +
  scale_fill_manual(values = color)  # Optional: use a color palette for better visualization

# Calculate percentage of strain pathway KOs
filtered_df1_binary <- ifelse(filtered_df1[,c(2:8)] >= 1, 1, 0)
filtered_df1_binary <- cbind(filtered_df1_binary, filtered_df1[,c(1,length(filtered_df1))])
row_sumsdf1 <- rowSums(filtered_df1_binary[,-c(8,9)])
row_totalsdf1 <- ncol(filtered_df1_binary[,-c(8,9)])
fractionsdf1 <- row_sumsdf1 / row_totalsdf1 * 100

filtered_df2_binary <- as.data.frame(ifelse(filtered_df2[,c(2:17)] >= 1, 1, 0))
row_sumsdf2 <- rowSums(filtered_df2_binary)
row_totalsdf2 <- ncol(filtered_df2_binary)
fractionsdf2 <- row_sumsdf2 / row_totalsdf2 * 100

filtered_df4_binary <- as.data.frame(ifelse(filtered_df4[,c(2:10)] >= 1, 1, 0))
row_sumsdf4 <- rowSums(filtered_df4_binary)
row_totalsdf4 <- ncol(filtered_df4_binary)
fractionsdf4 <- row_sumsdf4 / row_totalsdf4 * 100

filtered_df5_binary <- as.data.frame(ifelse(filtered_df5[,c(2:8)] >= 1, 1, 0))
row_sumsdf5 <- rowSums(filtered_df5_binary)
row_totalsdf5 <- ncol(filtered_df5_binary)
fractionsdf5 <- row_sumsdf5 / row_totalsdf5 * 100

filtered_df6_binary <- as.data.frame(ifelse(filtered_df6[,c(2:7)] >= 1, 1, 0))
row_sumsdf6 <- rowSums(filtered_df6_binary)
row_totalsdf6 <- ncol(filtered_df6_binary)
fractionsdf6 <- row_sumsdf6 / row_totalsdf6 * 100

filtered_df7_binary <- as.data.frame(ifelse(filtered_df7[,c(2:3)] >= 1, 1, 0))
row_sumsdf7 <- rowSums(filtered_df7_binary)
row_totalsdf7 <- ncol(filtered_df7_binary)
fractionsdf7 <- row_sumsdf7 / row_totalsdf7 * 100

filtered_df8_binary <- as.data.frame(ifelse(filtered_df8[,c(2:3)] >= 1, 1, 0))
row_sumsdf8 <- rowSums(filtered_df8_binary)
row_totalsdf8 <- ncol(filtered_df8_binary)
fractionsdf8 <- row_sumsdf8 / row_totalsdf8 * 100

filtered_df9_binary <- as.data.frame(ifelse(filtered_df9[,c(2:7)] >= 1, 1, 0))
row_sumsdf9 <- rowSums(filtered_df9_binary)
row_totalsdf9 <- ncol(filtered_df9_binary)
fractionsdf9 <- row_sumsdf9 / row_totalsdf9 * 100

filtered_df10_binary <- as.data.frame(ifelse(filtered_df10[,c(2)] >= 1, 1, 0))
row_sumsdf10 <- rowSums(filtered_df10_binary)
row_totalsdf10 <- ncol(filtered_df10_binary)
fractionsdf10 <- row_sumsdf10 / row_totalsdf10 * 100

filtered_df11_binary <- as.data.frame(ifelse(filtered_df11[,c(2)] >= 1, 1, 0))
row_sumsdf11 <- rowSums(filtered_df11_binary)
row_totalsdf11 <- ncol(filtered_df11_binary)
fractionsdf11 <- row_sumsdf11 / row_totalsdf11 * 100

filtered_df12_binary <- as.data.frame(ifelse(filtered_df12[,c(2:8)] >= 1, 1, 0))
row_sumsdf12 <- rowSums(filtered_df12_binary)
row_totalsdf12 <- ncol(filtered_df12_binary)
fractionsdf12 <- row_sumsdf12 / row_totalsdf12 * 100

filtered_df13_binary <- as.data.frame(ifelse(filtered_df13[,c(2,3)] >= 1, 1, 0))
row_sumsdf13 <- rowSums(filtered_df13_binary)
row_totalsdf13 <- ncol(filtered_df13_binary)
fractionsdf13 <- row_sumsdf13 / row_totalsdf13 * 100

filtered_df14_binary <- as.data.frame(ifelse(filtered_df14[,c(2)] >= 1, 1, 0))
row_sumsdf14 <- rowSums(filtered_df14_binary)
row_totalsdf14 <- ncol(filtered_df14_binary)
fractionsdf14 <- row_sumsdf14 / row_totalsdf14 * 100

filtered_df15_binary <- as.data.frame(ifelse(filtered_df15[,c(2:5)] >= 1, 1, 0))
row_sumsdf15 <- rowSums(filtered_df15_binary)
row_totalsdf15 <- ncol(filtered_df15_binary)
fractionsdf15 <- row_sumsdf15 / row_totalsdf15 * 100


# Add the fractions as a new column to the binary matrix
filtered_df_binary <- cbind(filtered_df1[,c(1,length(filtered_df1))], fractionsdf1, fractionsdf2,
                             fractionsdf4, fractionsdf5, fractionsdf6, fractionsdf7,
                            fractionsdf8, fractionsdf9, fractionsdf10, fractionsdf11, fractionsdf12,
                            fractionsdf13, fractionsdf14, fractionsdf15)
strains_genome <- unique(filtered_df_binary$strain)


# urea df 5
urea_data_all <- distinct_data[distinct_data$Treatment=='Urea + C', ]
urea_data_all <- ammonium_data_all[ammonium_data_all$Name %in% strains_genome, ]
colnames(urea_data_all)[which(colnames(urea_data_all) == "Name")] <- "strain" # Rename 'Name' to 'strain' in urea_data_all
merged_data <- merge(urea_data_all, filtered_df_binary, by = "strain")

# Create plot
ggplot(merged_data, aes(x = max_OD, y = fractionsdf5, color = Description)) +
  geom_point(size = 3) +  # Plot points
  scale_color_manual(values=color)+
  labs(
    title = "Fractions of Pathway KOs vs Max OD",
    x = "Max OD",
    y = "Fraction of Pathway KOs"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Summary of the linear model
lm_model <- lm(max_OD ~ fractionsdf5, data = merged_data)
summary(lm_model)

# Perform regression for each Description and store results
split_data <- split(merged_data, merged_data$Description.x)
results <- lapply(split_data, function(df) {
  lm_model <- lm(max_OD ~ fractionsdf15, data = df)
  summary(lm_model)
})

# alanine df 10
alanine_data_all <- distinct_data[distinct_data$Treatment=='Alanine + C', ]
alanine_data_all <- alanine_data_all[alanine_data_all$Name %in% strains_genome, ]
colnames(alanine_data_all)[which(colnames(alanine_data_all) == "Name")] <- "strain" # Rename 'Name' to 'strain' in urea_data_all
merged_data <- merge(alanine_data_all, filtered_df_binary, by = "strain")

# Create plot
ggplot(merged_data, aes(x = max_OD, y = fractionsdf14, color = Description.x)) +
  geom_point(size = 3) +  # Plot points
  scale_color_manual(values=color)+
  labs(
    title = "Fractions of Pathway KOs vs Max OD",
    x = "Max OD",
    y = "Fraction of Pathway KOs"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Summary of the linear model
lm_model <- lm(max_OD ~ fractionsdf14, data = merged_data)
summary(lm_model)

# Perform regression for each Description and store results
split_data <- split(merged_data, merged_data$Description.x)
results <- lapply(split_data, function(df) {
  lm_model <- lm(max_OD ~ fractionsdf15, data = df)
  summary(lm_model)
})

# nitrate df 1
nitrate_data_all <- distinct_data[distinct_data$Treatment=='Nitrate + C', ]
nitrate_data_all <- nitrate_data_all[nitrate_data_all$Name %in% strains_genome, ]
colnames(nitrate_data_all)[which(colnames(nitrate_data_all) == "Name")] <- "strain" # Rename 'Name' to 'strain' in urea_data_all
merged_data <- merge(nitrate_data_all, filtered_df_binary, by = "strain")

# Plot
ggplot(merged_data, aes(x = max_OD, y = fractionsdf1, color = Description.x)) +
  geom_point(size = 3) +  # Plot points
  scale_color_manual(values=color)+
  labs(
    title = "Fractions of Pathway KOs vs Max OD",
    x = "Max OD",
    y = "Fraction of Pathway KOs"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Summary of the linear model
lm_model <- lm(max_OD ~ fractionsdf1, data = merged_data)
summary(lm_model)

# Perform regression for each Description and store results
split_data <- split(merged_data, merged_data$Description.x)
results <- lapply(split_data, function(df) {
  lm_model <- lm(max_OD ~ fractionsdf15, data = df)
  summary(lm_model)
})

# nitrite df 2
nitrite_data_all <- distinct_data[distinct_data$Treatment=='Nitrite + C', ]
nitrite_data_all <- nitrite_data_all[nitrite_data_all$Name %in% strains_genome, ]
colnames(nitrite_data_all)[which(colnames(nitrite_data_all) == "Name")] <- "strain" # Rename 'Name' to 'strain' in urea_data_all
merged_data <- merge(nitrite_data_all, filtered_df_binary, by = "strain")

# Create plot
ggplot(merged_data, aes(x = max_OD, y = fractionsdf2, color = Description.x)) +
  geom_point(size = 3) +  # Plot points
  scale_color_manual(values=color)+
  labs(
    title = "Fractions of Pathway KOs vs Max OD",
    x = "Max OD",
    y = "Fraction of Pathway KOs"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Linear model
lm_model <- lm(max_OD ~ fractionsdf2, data = merged_data)
summary(lm_model)

# Perform regression for each Description and store results
split_data <- split(merged_data, merged_data$Description.x)
results <- lapply(split_data, function(df) {
  lm_model <- lm(max_OD ~ fractionsdf15, data = df)
  summary(lm_model)
})

# glutamate df 4
glutamate_data_all <- distinct_data[distinct_data$Treatment=='Glutamate + C', ]
glutamate_data_all <- glutamate_data_all[glutamate_data_all$Name %in% strains_genome, ]
colnames(glutamate_data_all)[which(colnames(glutamate_data_all) == "Name")] <- "strain" # Rename 'Name' to 'strain' in urea_data_all
merged_data <- merge(glutamate_data_all, filtered_df_binary, by = "strain")

# Create plot
ggplot(merged_data, aes(x = max_OD, y = fractionsdf4, color = Description.x)) +
  geom_point(size = 3) +  # Plot points
  scale_color_manual(values=color)+
  labs(
    title = "Fractions of Pathway KOs vs Max OD",
    x = "Max OD",
    y = "Fraction of Pathway KOs"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Summary of the linear model
lm_model <- lm(max_OD ~ fractionsdf4, data = merged_data)
summary(lm_model)

# Perform regression for each Description and store results
split_data <- split(merged_data, merged_data$Description.x)
results <- lapply(split_data, function(df) {
  lm_model <- lm(max_OD ~ fractionsdf15, data = df)
  summary(lm_model)
})

# glutamate df 4
glutamate_data_all <- distinct_data[distinct_data$Treatment=='Glutamate + C', ]
glutamate_data_all <- glutamate_data_all[glutamate_data_all$Name %in% strains_genome, ]
colnames(glutamate_data_all)[which(colnames(glutamate_data_all) == "Name")] <- "strain" # Rename 'Name' to 'strain' in urea_data_all
merged_data <- merge(glutamate_data_all, filtered_df_binary, by = "strain")

# Create plot
ggplot(merged_data, aes(x = max_OD, y = fractionsdf6, color = Description.x)) +
  geom_point(size = 3) +  # Plot points
  scale_color_manual(values=color)+
  labs(
    title = "Fractions of Pathway KOs vs Max OD",
    x = "Max OD",
    y = "Fraction of Pathway KOs"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Summary of the linear model
lm_model <- lm(max_OD ~ fractionsdf6, data = merged_data)
summary(lm_model)

# Perform regression for each Description and store results
split_data <- split(merged_data, merged_data$Description.x)
results <- lapply(split_data, function(df) {
  lm_model <- lm(max_OD ~ fractionsdf15, data = df)
  summary(lm_model)
})

# alanine df 7, 10
alanine_data_all <- distinct_data[distinct_data$Treatment=='Alanine + C', ]
alanine_data_all <- alanine_data_all[alanine_data_all$Name %in% strains_genome, ]
colnames(alanine_data_all)[which(colnames(alanine_data_all) == "Name")] <- "strain" # Rename 'Name' to 'strain' in urea_data_all
merged_data <- merge(alanine_data_all, filtered_df_binary, by = "strain")

# Create plot
ggplot(merged_data, aes(x = max_OD, y = fractionsdf10, color = Description.x)) +
  geom_point(size = 3) +  # Plot points
  scale_color_manual(values=color)+
  labs(
    title = "Fractions of Pathway KOs vs Max OD",
    x = "Max OD",
    y = "Fraction of Pathway KOs"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Summary of the linear model
lm_model <- lm(max_OD ~ fractionsdf10, data = merged_data)
summary(lm_model)

# Perform regression for each Description and store results
split_data <- split(merged_data, merged_data$Description.x)
results <- lapply(split_data, function(df) {
  lm_model <- lm(max_OD ~ fractionsdf15, data = df)
  summary(lm_model)
})

# proline df8
proline_data_all <- distinct_data[distinct_data$Treatment=='Proline + C', ]
proline_data_all <- proline_data_all[proline_data_all$Name %in% strains_genome, ]
colnames(proline_data_all)[which(colnames(proline_data_all) == "Name")] <- "strain" # Rename 'Name' to 'strain' in urea_data_all
merged_data <- merge(proline_data_all, filtered_df_binary, by = "strain")

# Create plot
ggplot(merged_data, aes(x = max_OD, y = fractionsdf9, color = Description.x)) +
  geom_point(size = 3) +  # Plot points
  scale_color_manual(values=color)+
  labs(
    title = "Fractions of Pathway KOs vs Max OD",
    x = "Max OD",
    y = "Fraction of Pathway KOs"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Summary of the linear model
lm_model <- lm(max_OD ~ fractionsdf9, data = merged_data)
summary(lm_model)

# Perform regression for each Description and store results
split_data <- split(merged_data, merged_data$Description.x)
results <- lapply(split_data, function(df) {
  lm_model <- lm(max_OD ~ fractionsdf15, data = df)
  summary(lm_model)
})

# glycine df11, df12, df13
glycine_data_all <- distinct_data[distinct_data$Treatment=='Glycine + C', ]
glycine_data_all <- glycine_data_all[glycine_data_all$Name %in% strains_genome, ]
colnames(glycine_data_all)[which(colnames(glycine_data_all) == "Name")] <- "strain" # Rename 'Name' to 'strain' in urea_data_all
merged_data <- merge(glycine_data_all, filtered_df_binary, by = "strain")

# Create plot
ggplot(merged_data, aes(x = max_OD, y = fractionsdf12, color = Description.x)) +
  geom_point(size = 3) +  # Plot points
  scale_color_manual(values=color)+
  labs(
    title = "Fractions of Pathway KOs vs Max OD",
    x = "Max OD",
    y = "Fraction of Pathway KOs"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Summary of the linear model
lm_model <- lm(max_OD ~ fractionsdf11, data = merged_data)
summary(lm_model)

# Perform regression for each Description and store results
split_data <- split(merged_data, merged_data$Description.x)
results <- lapply(split_data, function(df) {
  lm_model <- lm(max_OD ~ fractionsdf15, data = df)
  summary(lm_model)
})

# serine df15
serine_data_all <- distinct_data[distinct_data$Treatment=='Serine + C', ]
serine_data_all <- serine_data_all[serine_data_all$Name %in% strains_genome, ]
colnames(serine_data_all)[which(colnames(serine_data_all) == "Name")] <- "strain" # Rename 'Name' to 'strain' in urea_data_all
merged_data <- merge(serine_data_all, filtered_df_binary, by = "strain")

# Create plot
ggplot(merged_data, aes(x = max_OD, y = fractionsdf15, color = Description.x)) +
  geom_point(size = 3) +  # Plot points
  scale_color_manual(values=color)+
  labs(
    title = "Fractions of Pathway KOs vs Max OD",
    x = "Max OD",
    y = "Fraction of Pathway KOs"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Summary of the linear model
lm_model <- lm(max_OD ~ fractionsdf15, data = merged_data)
summary(lm_model)

# Perform regression for each Description and store results
split_data <- split(merged_data, merged_data$Description.x)
results <- lapply(split_data, function(df) {
  lm_model <- lm(max_OD ~ fractionsdf15, data = df)
  summary(lm_model)
})
