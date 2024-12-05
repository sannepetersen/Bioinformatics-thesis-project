# Correlation between conditions

# Calculate the correlation between max_derivative values for each Treatment
correlation_result <- merged_data %>%
  select(Treatment, max_derivative, Name) %>%
  distinct()

# Pivot the data to wide format where each Treatment is a column
wide_data <- correlation_result %>%
  pivot_wider(names_from = Treatment, values_from = max_derivative)

# Calculate the correlation
correlation_result <- cor(wide_data %>% select(-Name), use = "pairwise.complete.obs")

# Reshape the correlation matrix into long format
correlation_long <- melt(correlation_result, varnames = c("Treatment1", "Treatment2"))

# Find the order of treatments based on the average correlation values
treatment_order <- correlation_long %>%
  group_by(Treatment1) %>%
  summarise(avg_corr = mean(value, na.rm = TRUE)) %>%
  arrange(desc(avg_corr)) %>%
  pull(Treatment1)

# Reorder both x and y axes based on the sorted treatment order
correlation_long$Treatment1 <- factor(correlation_long$Treatment1, levels = treatment_order)
correlation_long$Treatment2 <- factor(correlation_long$Treatment2, levels = treatment_order)

# Create the heatmap
ggplot(correlation_long, aes(x = Treatment1, y = Treatment2, fill = value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "white", high = "#08519C", mid = "white", midpoint = 0) +  
  labs(title = "Correlation matrix of growth rates", 
       x = "Condition", 
       y = "Condition", 
       fill = "Correlation") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate the correlation between yield values for each condition
correlation_result <- distinct_data %>%
  select(Treatment, max_OD, Name) %>%
  distinct()

# Pivot the data to wide format where each Treatment is a column
wide_data <- correlation_result %>%
  pivot_wider(names_from = Treatment, values_from = max_OD)

# Calculate hte correlation
correlation_result <- cor(wide_data %>% select(-Name), use = "pairwise.complete.obs")

# Reshape the correlation matrix into long format
correlation_long <- melt(correlation_result, varnames = c("Treatment1", "Treatment2"))

# Find the order of treatments based on the average correlation values
treatment_order <- correlation_long %>%
  group_by(Treatment1) %>%
  summarise(avg_corr = mean(value, na.rm = TRUE)) %>%
  arrange(desc(avg_corr)) %>%
  pull(Treatment1)

#Reorder both x and y axes based on the sorted treatment order
correlation_long$Treatment1 <- factor(correlation_long$Treatment1, levels = treatment_order)
correlation_long$Treatment2 <- factor(correlation_long$Treatment2, levels = treatment_order)

# Create the heatmap
ggplot(correlation_long, aes(x = Treatment1, y = Treatment2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "white", high = "#08519C", mid = "white", midpoint = 0) + 
  labs(title = "Correlation matrix of growth rates", 
       x = "Condition", 
       y = "Condition", 
       fill = "Correlation") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=6), 
        axis.text.y = element_text(size=6))
