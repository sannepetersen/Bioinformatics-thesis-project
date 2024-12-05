# Data analysis before no-nitrogen subtraction

# Obtain distinct dfs for max GRs inorganic nitrogen sources
distinct_max_derivative_inorganic <- combined_derivatives_inorganic %>%
  group_by(Name, Treatment, Description) %>%
  summarise(
    max_derivative = max(max_derivative, na.rm = TRUE), 
    .groups = "drop"  
  )

# Obtain distinct dfs for yield inorganic nitrogen sources
distinct_max_OD_inorganic <- combined_derivatives_inorganic %>%
  group_by(Name, Treatment, Description) %>%
  summarise(
    max_OD = max(max_OD, na.rm = TRUE),  
    .groups = "drop"  
  )

# Obtain distinct dfs for yield and GR all conditions
all_data_info <- all_data_info %>%
  group_by(Name, Treatment) %>% 
  mutate(maxOD_before = max(Highest_Value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(growth_before = ifelse(maxOD_before > 0.1, 1, 0)) 

# Select columns you want to keep
distinct_values_alldata <- all_data_info %>%
  select(Name, Treatment, maxOD_before, growth_before, Description) %>%
  distinct()

# Summarize the data before no-nitrogen control correction
summary_data_before <- distinct_values_alldata %>%
  group_by(Description) %>%
  summarise(mean_maxOD_before = mean(maxOD_before, na.rm = TRUE),
            median_maxOD_before = median(maxOD_before, na.rm = TRUE))

# Calculate percentage of growth treatment per treatment
growth_percentages <- distinct_values_alldata %>%
  group_by(Treatment) %>%
  summarise(
    growth_count = sum(growth_before),  
    total_count = n(),                
    growth_percentage = (growth_count / total_count) * 100  #
  ) %>%
  ungroup()

# Calculate percentage of growth treatment per order
growth_percentages_order <- distinct_values_alldata %>%
  group_by(Description) %>%
  summarise(
    growth_count = sum(growth_before),  
    total_count = n(),                 
    growth_percentage = (growth_count / total_count) * 100  
  ) %>%
  ungroup()

# Put them in order of decreasing
growth_percentages_order <- growth_percentages_order %>%
  arrange(Description, desc(growth_percentage))
growth_percentages <- growth_percentages %>%
  arrange(Treatment, desc(growth_percentage))

# Plot growth rates inorganic nitrogen sources + C
ggplot(distinct_max_derivative_inorganic, aes(x = Treatment, y = max_derivative, fill = Description)) +
  geom_boxplot() +
  labs(
    title = "Max growth rates inorganic N-sources",
    x = "Nitrogen Source",
    y = "Max Per Capita Growth Rate",
    fill = "Order"
  ) +
  scale_fill_manual(values = color_orders) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# Plot growth rates inorganic nitrogen sources + C
ggplot(distinct_max_OD_inorganic, aes(x = Treatment, y = max_OD, fill = Description)) +
  geom_boxplot() +
  labs(
    title = "Yields inorganic N-sources",
    x = "Nitrogen Source",
    y = "Yield",
    fill = "Order"
  ) +
  scale_fill_manual(values = color_orders) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# Plot the yields of all data across all conditions
ggplot(distinct_values_alldata, aes(x = factor(Description, levels = desired_order), y = maxOD_before, fill = Description)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, color = "black") +  
  geom_jitter(size = 1, width = 0.1, alpha = 0.6) +  
  labs(
    title = "Yield per order",
    x = "Order",
    y = "Yield",
    fill = 'Order'
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "black") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
  scale_fill_manual(values = color)

# Plot the growth percentages per order
ggplot(growth_percentages_order, aes(x = Description, y = growth_percentage, fill = Description)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = growth_percentage)) +
  labs(
    title = "Growth Percentage by Bacterial Order",
    x = "Bacterial Order",
    y = "Growth Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) 

# Plot the growth percentages per treatment
ggplot(growth_percentages, aes(x = reorder(Treatment, growth_percentage), y = growth_percentage, fill = growth_percentage)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = growth_percentage)) + 
  labs(
    title = "Percentage of growth per treatment",
    x = "Treatment",
    y = "Growth Percentage (%)",
    fill = "Growth Percentage"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=6),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Combine all control datasets into a single data frame with Treatment labels
combined_control <- bind_rows(
  controlA2R1 %>% mutate(Treatment = conditionA2),
  controlB1 %>% mutate(Treatment = conditionB1),
  controlB2R1 %>% mutate(Treatment = conditionB2),
  controlC1R1 %>% mutate(Treatment = conditionC1),
  controlC2R1 %>% mutate(Treatment = conditionC2),
  controlD2R1 %>% mutate(Treatment = conditionD2),
  controlE1 %>% mutate(Treatment = conditionE1),
  controlE2R1 %>% mutate(Treatment = conditionE2),
  control %>% mutate(Treatment = conditionF1),
  controlF2R1 %>% mutate(Treatment = conditionF2),
  controlG1 %>% mutate(Treatment = conditionG1),
  controlG2R1 %>% mutate(Treatment = conditionG2),
  controlH1 %>% mutate(Treatment = conditionH1),
  controlH2R1 %>% mutate(Treatment = conditionH2),
  controlI1 %>% mutate(Treatment = conditionI1),
  controlI2R1 %>% mutate(Treatment = conditionI2),
  controlJ1 %>% mutate(Treatment = conditionJ1),
  controlJ2R1 %>% mutate(Treatment = conditionJ2),
  controlK1 %>% mutate(Treatment = conditionK1),
  controlK2R1 %>% mutate(Treatment = conditionK2),
  controlL1 %>% mutate(Treatment = conditionL1),
  controlL2R1 %>% mutate(Treatment = conditionL2),
  controlM1R1 %>% mutate(Treatment = conditionM1),
  controlM2R1 %>% mutate(Treatment = conditionM2),
  controlN1 %>% mutate(Treatment = conditionN1),
  controlN2R1 %>% mutate(Treatment = conditionN2),
  controlO1 %>% mutate(Treatment = conditionO1),
  controlO2R1 %>% mutate(Treatment = conditionO2),
  controlP1 %>% mutate(Treatment = conditionP1),
  controlP2R1 %>% mutate(Treatment = conditionP2),
  controlQ1 %>% mutate(Treatment = conditionQ1),
  controlQ2R1 %>% mutate(Treatment = conditionQ2),
  controlR1 %>% mutate(Treatment = conditionR1),
  controlR2R1 %>% mutate(Treatment = conditionR2),
  controlS1 %>% mutate(Treatment = conditionS1),
  controlS2S2 %>% mutate(Treatment = conditionS2),
  controlT1 %>% mutate(Treatment = conditionT1),
  controlT2T1 %>% mutate(Treatment = conditionT2)
)

# Calculate the maximum value for each condition of control
max_values_control <- combined_control %>%
  group_by(Treatment) %>%
  summarize(MaxValue = max(Value, na.rm = TRUE))

ggplot(max_values_control, aes(x = "", y = MaxValue)) +  # Set x to "" for a single boxplot
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Single boxplot
  geom_jitter(width = 0.2, color = "black", alpha = 0.5) +  # Jittered points
  labs(
    title = "Distribution of yield across controls",
    x = NULL,  # No x-axis label
    y = "Yield"
  ) +
  theme_minimal()

