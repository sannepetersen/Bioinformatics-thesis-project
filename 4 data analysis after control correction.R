# Data analysis after no-nitrogen control correction

# Obtain distinct data for yield and GR
distinct_data <- all_data_info %>%
  select(Name, Treatment, max_OD, max_derivative, growth, Description) %>%
  distinct()

# Summarize distinct data
summary_data <- distinct_data %>%
  group_by(Description) %>%
  summarise(mean_maxOD = mean(max_OD, na.rm = TRUE),
            median_maxOD = median(max_OD, na.rm = TRUE),
            mean_GR = mean(max_derivative, na.rm = TRUE))

# Plot yields after control correction for each order
ggplot(distinct_data, aes(x = factor(Description, levels = desired_order), y = max_OD, fill = Description)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, color = "black") +  
  geom_jitter(size = 1, width = 0.1, alpha = 0.6) +  
  labs(
    title = "Yield per order",
    x = "Order",
    y = "Yield",
    fill = 'Order'
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0.06, linetype = "dashed", color = "black") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
  scale_fill_manual(values = color)

# Calculate percentage of growth treatment
growth_percentages_after <- distinct_data %>%
  group_by(Treatment) %>%
  summarise(
    growth_count = sum(growth),  
    total_count = n(),                
    growth_percentage = (growth_count / total_count) * 100  
  ) %>%
  ungroup()

# Change order to desceding of percentages of growth
growth_percentages_after <- growth_percentages_after %>%
  arrange(Treatment, desc(growth_percentage))

# Filter out "no N control + C" before plotting
growth_percentages_after_filtered <- growth_percentages_after %>%
  filter(Treatment != "no N control + C")

# Create the plot
ggplot(growth_percentages_after_filtered, aes(x = reorder(Treatment, growth_percentage), y = growth_percentage, fill = growth_percentage)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = growth_percentage)) +  
  labs(
    title = "Percentage of Growth per Treatment",
    x = "Treatment",
    y = "Growth Percentage (%)",
    fill = "Growth Percentage"
  ) +
  theme_minimal() +
  ylim(0,100) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Only take timepoints without timepoint 7
new_df_49hours <- all_data_info %>%
  filter(across(starts_with("timepoints"), ~ . <= 100, .names = "filtered"))

# Summarize data up to timepoint 6
new_df_49hours <- new_df_49hours %>%
  group_by(Name, Treatment, Description) %>%
  summarise(
    max_OD49 = max(Value_minus_no_N, na.rm = TRUE)
  )

# Reorder the 'Name' factor according to the order of the strains grouped
# by their taxonomic distance
all_derivatives_bigheatmap49 <- new_df_49hours %>%
  mutate(Name = factor(Name, levels = allstrains_order))

# Hierarchical clustering to reorder Treatments based on mean max_OD
all_derivatives_bigheatmap49 <- all_derivatives_bigheatmap49 %>%
  mutate(Treatment = factor(Treatment, 
                            levels = all_derivatives_bigheatmap49 %>%
                              group_by(Treatment) %>%
                              summarize(mean_max_OD = mean(max_OD49)) %>%
                              arrange(mean_max_OD) %>%
                              pull(Treatment)))

# Create the heatmap of yield for the first 6 timepoints across all conditions
ggplot(all_derivatives_bigheatmap49, aes(x = factor(Name, levels = allstrains_order), y = Treatment, fill = max_OD49)) +
  geom_tile() +
  scale_fill_viridis_c(option = 'cividis') +
  theme_minimal() +
  labs(
    title = "Heatmap of Yield",
    x = "Strain",
    y = "Condition",
    fill = "Yield"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
    axis.text.y = element_text(size = 5)
  )

# Normalize max_OD by yield of treatment and calculate sorting order based on mean
normalized_data <- all_derivatives_bigheatmap49 %>%
  group_by(Treatment) %>% 
  mutate(
    normalized_OD = max_OD49 / max(max_OD49, na.rm = TRUE), 
    mean_OD = mean(max_OD49, na.rm = TRUE) 
  ) %>% 
  ungroup() %>% 
  mutate(Treatment = reorder(Treatment, mean_OD))

# Create the heatmap with normalized data
ggplot(normalized_data, aes(x = factor(Name, levels = allstrains_order), y = Treatment, fill = normalized_OD)) +
  geom_tile() +
  scale_fill_viridis_c(option = 'cividis') +
  theme_minimal() +
  labs(
    title = "Normalized heatmap of yield after 49 hours",
    x = "Strain",
    y = "Condition",
    fill = "Normalized Yield"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 2),
    axis.text.y = element_text(size = 5)
  )

# Obtain unique data for GR
unique_non_plusC <- non_plusC %>%
  distinct(Name, max_derivative, max_OD, .keep_all = TRUE) %>% 
  group_by(Name) %>%
  mutate(max_derivative = max(max_derivative, na.rm = TRUE)) 

# Determine growth when yield >0.06
unique_non_plusC <- unique_non_plusC %>%
  mutate(growth = if_else(max_OD > 0.06, 1, 0))

# Plot the distribution of the growth rates for no-nitrogen control across orders
ggplot(unique_non_plusC, aes(x = factor(Description, levels = desired_order), y = max_derivative)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  geom_jitter(width = 0.2, height = 0, color = "black", alpha = 0.5) +
  labs(
    title = "Distribution of max growth rate for no-N control",
    x = "",
    y = "Max GR"
  ) +
  theme_minimal()

# Plot the distribution of the yields for no-nitrogen control across orders
ggplot(unique_non_plusC, aes(x = factor(Description, levels = desired_order), y = max_OD)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  geom_jitter(width = 0.2, height = 0, color = "black", alpha = 0.5) +
  labs(
    title = "Distribution of yield for no-N control",
    x = "",
    y = "Yield"
  ) +
  theme_minimal()

# Calculate the percentages of growth for the no-nitrogen control across orders
growth_percentage_no_n <- unique_non_plusC %>%
  group_by(Description) %>%  
  summarise(
    total_count = n(), 
    growth_count = sum(growth == 1, na.rm = TRUE), 
    growth_percentage = (growth_count / total_count) * 100 
  ) %>%
  arrange(desc(growth_percentage)) 

# Filter the data for the two specific treatments
filtered_non <- distinct_data %>%
  filter(Treatment %in% c("no N control + C"))

# Create the boxplot with jitter points for the yield of no-N control
ggplot(filtered_non, aes(x = Description, y = max_OD)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, alpha = 0.6) +  
  geom_jitter(aes(color = Description), width = 0.2, size = 2, alpha = 0.7) +  
  labs(
    title = "Yield inorganic nitrogen sources",
    x = " ",
    y = "Yield"
  ) +
  theme_minimal() +
  scale_color_manual(values = color_orders)+
  theme(
    plot.title = element_text(hjust = 0.5),  
    axis.text = element_text(size = 12, angle = 90),    
    axis.title = element_text(size = 14),   
    legend.position = 'none'
  )

# Filter the data for the inorganic nitrogen sources
filtered_urea_non <- distinct_data %>%
  filter(Treatment %in% c("Urea + C", "no N control + C", 'Ammonium + C', 
                          'Nitrate + C', 'Nitrite + C'))

# Create the boxplot with jitter points for yield
ggplot(filtered_urea_non, aes(x = Treatment, y = max_OD)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, alpha = 0.6) + 
  geom_jitter(aes(color = Treatment), width = 0.2, size = 2, alpha = 0.7) + 
  labs(
    title = "Yield inorganic nitrogen sources",
    x = "Treatment",
    y = "Yield"
  ) +
  theme_minimal() +
  scale_color_manual(values = inorganic_colors)+
  theme(
    plot.title = element_text(hjust = 0.5),   
    axis.text = element_text(size = 12, angle = 90),     
    axis.title = element_text(size = 14)   
  )

# Count the number of treatments
number_treatments <- length(unique(distinct_data$Treatment))

# Calculate on how many conditions each strain grows
growth_per_name <- distinct_data %>%
  filter(growth == 1) %>% 
  group_by(Name) %>%
  summarize(
    growth_count = n_distinct(Treatment), 
    Description = first(Description) 
  )

# Plot yield vs Growth Rate
ggplot(distinct_data, aes(x = max_derivative, y = max_OD)) +
  geom_point(color = "#08519C", size = 3, alpha = 0.7) +  # Scatter plot with blue points
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +  # Add a linear trend line
  labs(
    title = "Yield vs Growth Rate All Data",
    x = "Growth Rate (h)",
    y = "Yield (OD600)"
  ) +
  theme_minimal() 

# Fit linear regression per strain and extract slopes
regression_results <- distinct_data %>%
  group_by(Name) %>%
  summarize(
    slope = coef(lm(max_OD ~ max_derivative, data = cur_data()))[2],
    p_value = summary(lm(max_OD ~ max_derivative, data = cur_data()))$coefficients[2, 4]
  )

# Identify significant slopes
significant_strains <- regression_results %>% filter(p_value < 0.05)

# Calculate overall slope across all data
overall_model <- lm(max_OD ~ max_derivative, data = distinct_data)
overall_slope <- coef(overall_model)[2]

# Plot slopes with overall slope as a vertical line
ggplot(regression_results, aes(x = slope)) +
  geom_histogram(binwidth = 0.2, fill = "#08519C", color = "black", alpha = 0.5) +
  geom_vline(xintercept = overall_slope, color = "blue", linetype = "dashed", size = 1.2) +
  labs(
    title = "Distribution of Regression Slopes per Strain",
    x = "Slope of yield vs max GR",
    y = "Frequency"
  ) +
  theme_minimal() 

# Create the plot growth count per strain
ggplot(growth_per_name, aes(x = growth_count, y = reorder(Name, growth_count), fill = Description)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = color) + 
  labs(
    title = "Growth Count per Strain",
    x = "Growth Count (Number of Conditions)",
    y = "Strain", 
    fill = 'Order'
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 3), 
    plot.title = element_text(hjust = 0.5),  
    aspect.ratio = 2  
  )

# Combine growth per strain data with GR and yield data.
combined_data <- distinct_data %>%
  left_join(growth_per_name, by = "Name") 

# Summarize the average yield and GR per number of treatments that grow
summary_data <- combined_data %>%
  group_by(growth_count) %>%
  summarize(
    avg_yield = mean(max_OD, na.rm = TRUE),
    avg_rate = mean(max_derivative, na.rm = TRUE),
    sd_yield = sd(max_OD, na.rm = TRUE),
    sd_rate = sd(max_derivative, na.rm = TRUE),
    n = n() 
  )

# Plot results
ggplot(summary_data, aes(x = growth_count)) +
  geom_point(aes(y = avg_yield), color = "blue", shape = 16, size = 3) +
  geom_point(aes(y = avg_rate), color = "red", shape = 15, size = 3) +
  geom_errorbar(aes(ymin = avg_yield - sd_yield, ymax = avg_yield + sd_yield), width = 0.2, color = "blue") +
  geom_errorbar(aes(ymin = avg_rate - sd_rate, ymax = avg_rate + sd_rate), width = 0.2, color = "red") +
  geom_smooth(aes(y = avg_yield), method = "lm", color = "blue", se = FALSE) +
  geom_smooth(aes(y = avg_rate), method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Growth Rate and Yield vs. Number of Growing Treatments",
    x = "Number of Treatments Supporting Growth",
    y = "Mean ± SD",
    subtitle = "Blue dots: Yield, Red squares: Growth Rate"
  ) +
  theme_minimal()

# Obtain distinct derivatives
distinct_max_derivative_inorganic <- combined_derivatives_inorganic %>%
  group_by(Name, Treatment, Description) %>%
  summarise(
    max_derivative = max(max_derivative, na.rm = TRUE), 
    .groups = "drop"  
  )

# Obtain distinct yields
distinct_max_OD <- combined_derivatives_inorganic %>%
  group_by(Name, Treatment, Description) %>%
  summarise(
    max_OD = max(max_OD, na.rm = TRUE), 
    .groups = "drop" 
  )

# Plot growth rates inorganic nitrogen sources + C
ggplot(distinct_max_derivative_inorganic, aes(x = Treatment, y = max_derivative, color = Description)) +
  geom_boxplot(alpha = 0.5) +
  labs(
    title = "Max Growth Rates Inorganic N-sources",
    x = "Nitrogen Source",
    y = "Max Per Capita Growth Rate",
    color = "Order"
  ) +
  scale_color_manual(values = color) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# Plot growth rates inorganic nitrogen sources + C
ggplot(distinct_max_OD, aes(x = Treatment, y = max_OD, color = Description)) +
  geom_boxplot(alpha = 0.5) +
  labs(
    title = "Yield Inorganic N-sources",
    x = "Nitrogen Source",
    y = "Yield",
    color = "Order"
  ) +
  scale_color_manual(values = color) +  # Apply the custom colors to the 'Description' column
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# Plot with jitter for max growth rates
ggplot(distinct_max_derivative_inorganic, aes(x = Treatment, y = max_derivative)) +  
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.6) + 
  labs(
    title = "Max growth rates inorganic N-sources",
    x = "N-source",
    y = "Max GR",
    color = "Order"
  ) +  
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    plot.title = element_text(hjust = 0.5) 
  )

# Plot boxplots of GR per order organic nitrogen sources
ggplot(combined_derivatives_organic, aes(x = Treatment, y = max_derivative, color = Description)) +
  geom_boxplot(alpha = 0.5) + 
  labs(
    title = "Max growth rates organic N-sources",
    x = "N-source",
    y = "Max GR",
    color = "Order"
  ) +
  scale_color_manual(values = color) +  
  facet_wrap(~Description, scales = "free_y", ncol = 3) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    plot.title = element_text(hjust = 0.5, size = 12),
    strip.text = element_text(size = 10),  
    legend.position = "none"
  )

# Plot boxplots of GR per order inorganic nitrogen sources
ggplot(combined_derivatives_inorganic, aes(x = Treatment, y = max_derivative, color = Description)) +
  geom_boxplot(alpha = 0.5) + 
  labs(
    title = "Max growth rates inorganic N-sources",
    x = "N-source",
    y = "Max GR",
    color = "Order"
  ) +
  scale_color_manual(values = color) +  
  facet_wrap(~Description, scales = "free_y", ncol = 3) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    plot.title = element_text(hjust = 0.5, size = 12),
    strip.text = element_text(size = 10),  
    legend.position = "none"
  )

# Combine data inorganic and organic nitrogen sources + C
all_derivatives_plusC <- bind_rows(combined_derivatives_inorganic, combined_derivatives_organic)

# Plot boxplots of GR per order organic and inorganic nitrogen sources
ggplot(all_derivatives_plusC, aes(x = Treatment, y = max_derivative, color = Description)) +
  geom_boxplot(alpha = 0.5) + 
  labs(
    title = "Max growth rates N-sources",
    x = "N-source",
    y = "Max GR",
    color = "Order"
  ) +
  scale_color_manual(values = color) +  
  facet_wrap(~Description, scales = "free_y", ncol = 3) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    axis.text.y = element_text(size = 7),
    plot.title = element_text(hjust = 0.5, size = 12),
    strip.text = element_text(size = 10),  
    legend.position = "none"
  )

# Max OD vs GR for all orders
ggplot(distinct_data, aes(x = max_OD, y = max_derivative)) + 
  geom_point(aes(color = max_derivative), size = 1) + 
  scale_color_viridis() +  
  labs(
    title = "max_OD vs max_derivative (Colored by Condition)",
    x = "Yield",  
    y = "Max GR" 
  ) + 
  theme_minimal() +
  facet_wrap(~ Description) + 
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "black") +  # Linear fit line
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10), 
    legend.position = 'none'
  )

# Growth plots all data obtain data
combined_derivatives_organic <- combined_derivatives_organic %>%
  mutate(growth = if_else(max_OD > 0.06, 1, 0))
  distinct_max_derivative_organic <- combined_derivatives_organic %>%
    group_by(Name, Treatment, Description, growth) %>%
    summarise(
      max_OD = max(max_OD, na.rm = TRUE), 
      .groups = "drop"  
    )
  
  # Step 1: Calculate the percentage of growth per Description and Treatment
  growth_percentage <- distinct_max_derivative_organic %>%
    group_by(Description, Treatment) %>%
    summarise(
      total_count = n(),  # Total number of rows per Description and Treatment
      growth_count = sum(growth == 1, na.rm = TRUE),  # Count of growth (growth == 1)
      growth_percentage = (growth_count / total_count) * 100,  # Calculate percentage
      .groups = "drop"
    )
  
  # Determine colors
  colors <- c('Threonine + C' = "#E41A1C", 'Serine + C'= "#377EB8", 'Proline + C'= "#4DAF4A",
              'Glycine + C' = "#984EA3", 'Glutamine + C'= "#FF7F00", 'Glutamate + C'= "#FFFF33",
              'Glucosamine + C' = "#A65628", 'GlcNAc + C'= "#F781BF", 'Aspartate + C'= "#999999",
              'Asparagine + C'= '#66C2A5', 'Arginine + C' = "#FC8D62", 'Alanine + C'= "#8DA0CB")
  
  # Step 2: Plot the growth percentage per Description and Treatment
  ggplot(growth_percentage, aes(x = growth_percentage, y = Description, fill = Treatment)) +
    geom_bar(stat = "identity", position = "dodge") +  # Dodge to separate bars for each Treatment
    theme_minimal() +
    scale_fill_manual(values = colors)+
    labs(
      title = "Percentage of Growth per Description and Treatment",
      x = "Growth Percentage (%)",
      y = " "
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(reverse = TRUE))
  
  # Obtain data for minC
  combined_derivatives_organic_minC <- combined_derivatives_organic_minC %>%
    mutate(growth = if_else(max_OD > 0.06, 1, 0))
  
  distinct_max_derivative_organic_minC <- combined_derivatives_organic_minC %>%
    group_by(Name, Treatment, Description, growth) %>%
    summarise(
      max_OD = max(max_OD, na.rm = TRUE), 
      .groups = "drop"  
    )
  
  # Calculate the percentage of growth per Description and Treatment
  growth_percentage_minC <- distinct_max_derivative_organic_minC %>%
    group_by(Description, Treatment) %>%
    summarise(
      total_count = n(),  # Total number of rows per Description and Treatment
      growth_count = sum(growth == 1, na.rm = TRUE),  # Count of growth (growth == 1)
      growth_percentage = (growth_count / total_count) * 100,  # Calculate percentage
      .groups = "drop"
    )
  
  # Determine colors
  colors <- c('Threonine - C' = "#E41A1C", 'Serine - C'= "#377EB8", 'Proline - C'= "#4DAF4A",
              'Glycine - C' = "#984EA3", 'Glutamine - C'= "#FF7F00", 'Glutamate - C'= "#FFFF33",
              'Glucosamine - C' = "#A65628", 'GlcNAc - C'= "#F781BF", 'Aspartate - C'= "#999999",
              'Asparagine - C'= '#66C2A5', 'Arginine - C' = "#FC8D62", 'Alanine - C'= "#8DA0CB")
  
  # Plot the growth percentage per Description and Treatment
  ggplot(growth_percentage_minC, aes(x = growth_percentage, y = Description, fill = Treatment)) +
    geom_bar(stat = "identity", position = "dodge") +  # Dodge to separate bars for each Treatment
    theme_minimal() +
    scale_fill_manual(values = colors)+
    labs(
      title = "Percentage of Growth per Description and Treatment",
      x = "Growth Percentage (%)",
      y = " "
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(reverse = TRUE))
  
  # Obtain data for plus N
  combined_derivatives_organic_plusN <- combined_derivatives_organic_plusN %>%
    mutate(growth = if_else(max_OD > 0.06, 1, 0))
  distinct_max_derivative_organic_plusN <- combined_derivatives_organic_plusN %>%
    group_by(Name, Treatment, Description, growth) %>%
    summarise(
      max_OD = max(max_OD, na.rm = TRUE), 
      .groups = "drop"  
    )
  
  # Calculate the percentage of growth per Description and Treatment
  growth_percentage_plusN <- distinct_max_derivative_organic_plusN %>%
    group_by(Description, Treatment) %>%
    summarise(
      total_count = n(),  # Total number of rows per Description and Treatment
      growth_count = sum(growth == 1, na.rm = TRUE),  # Count of growth (growth == 1)
      growth_percentage = (growth_count / total_count) * 100,  # Calculate percentage
      .groups = "drop"
    )
  
  # Obtain colors
  colors <- c('Threonine + N' = "#E41A1C", 'Serine + N'= "#377EB8", 'Proline + N'= "#4DAF4A",
              'Glycine + N' = "#984EA3", 'Glutamine + N'= "#FF7F00", 'Glutamate + N'= "#FFFF33",
              'Glucosamine + N' = "#A65628", 'GlcNAc + N'= "#F781BF", 'Aspartate + N'= "#999999",
              'Asparagine + N'= '#66C2A5', 'Arginine + N' = "#FC8D62", 'Alanine + N'= "#8DA0CB")
  
  # Plot the growth percentage per Description and Treatment
  ggplot(growth_percentage_plusN, aes(x = growth_percentage, y = Description, fill = Treatment)) +
    geom_bar(stat = "identity", position = "dodge") +  # Dodge to separate bars for each Treatment
    theme_minimal() +
    scale_fill_manual(values = colors)+
    labs(
      title = "Percentage of Growth per Description and Treatment",
      x = "Growth Percentage (%)",
      y = " "
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(reverse = TRUE))
  
  # Combine organic and inorganic data
  combined_derivatives_inorganic <- combined_derivatives_inorganic %>%
    mutate(growth = if_else(max_OD > 0.06, 1, 0))
  distinct_max_derivative_inorganic <- combined_derivatives_inorganic %>%
    group_by(Name, Treatment, Description, growth) %>%
    summarise(
      max_OD = max(max_OD, na.rm = TRUE), 
      .groups = "drop"  
    )
  
  # Calculate the percentage of growth per Description and Treatment
  growth_percentage_inorganic <- distinct_max_derivative_inorganic %>%
    group_by(Description, Treatment) %>%
    summarise(
      total_count = n(),  # Total number of rows per Description and Treatment
      growth_count = sum(growth == 1, na.rm = TRUE),  # Count of growth (growth == 1)
      growth_percentage = (growth_count / total_count) * 100,  # Calculate percentage
      .groups = "drop"
    )
  
  colors <- c('Threonine + N' = "#E41A1C", 'Serine + N'= "#377EB8", 'Proline + N'= "#4DAF4A",
              'Glycine + N' = "#984EA3", 'Glutamine + N'= "#FF7F00", 'Glutamate + N'= "#FFFF33",
              'Glucosamine + N' = "#A65628", 'GlcNAc + N'= "#F781BF", 'Aspartate + N'= "#999999",
              'Asparagine + N'= '#66C2A5', 'Arginine + N' = "#FC8D62", 'Alanine + N'= "#8DA0CB")
  
  new_colors_inorganic <- c("Ammonium + C" = "#00CED1",
                  "Nitrite + C" = "#FFD700",
                  "Nitrate + C" = "#DC143C",
                  "no N control + C" = "#4B0082",
                  "Urea + C" = "#228B22")
  
  # Step 2: Plot the growth percentage per Description and Treatment
  ggplot(growth_percentage_inorganic, aes(x = growth_percentage, y = Description, fill = Treatment)) +
    geom_bar(stat = "identity", position = "dodge") +  # Dodge to separate bars for each Treatment
    theme_minimal() +
    scale_fill_manual(values = new_colors_inorganic)+
    labs(
      title = "Percentage of Growth per Description and Treatment",
      x = "Growth Percentage (%)",
      y = " "
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(reverse = TRUE))
  
  
  # Step 2: Plot the growth percentage per Description and Treatment
  ggplot(growth_percentage_inorganic, aes(x = growth_percentage, y = Description, fill = Treatment)) +
    geom_bar(stat = "identity", position = "dodge") +  # Dodge to separate bars for each Treatment
    theme_minimal() +
    scale_fill_manual(values = new_colors_inorganic)+
    labs(
      title = "Percentage of Growth per Description and Treatment",
      x = "Growth Percentage (%)",
      y = " "
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(reverse = TRUE))
  
  # Combine colors and data
  colors_plusC <- c(new_colors_inorganic, colors)
  plusC_organic_inorganic_percentage <- rbind(growth_percentage_inorganic, growth_percentage)
  plusC_organic_inorganic_percentage <- plusC_organic_inorganic_percentage[plusC_organic_inorganic_percentage$Treatment != 'no N control + C', ]
  
  # Plot the growth percentage per Description and Treatment
  ggplot(plusC_organic_inorganic_percentage, aes(x = growth_percentage, y = Description, fill = Treatment)) +
    geom_bar(stat = "identity", position = "dodge") +  # Dodge to separate bars for each Treatment
    theme_minimal() +
    scale_fill_manual(values = colors_plusC)+
    labs(
      title = "Percentage of Growth per Description and Treatment",
      x = "Growth Percentage (%)",
      y = " "
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(reverse = TRUE))
  
  # Save the plot with a wider dimension
  ggsave("growth_percentage_plot.png", width = 10, height = 12)
  
