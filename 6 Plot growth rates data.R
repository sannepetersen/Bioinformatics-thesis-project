# Plot growth rates data

# Combine organic and inorganic data
organic_inorganic_data <- bind_rows(organic_selected, inorganic_selected)

# Determine growth when yield >0.06
organic_inorganic_data_grow <- organic_inorganic_data[organic_inorganic_data$max_OD >0.06, ]

# Step 1: Calculate the mean for each treatment
mean_values <- organic_inorganic_data_grow %>%
  group_by(Treatment) %>%
  summarise(mean_value = mean(max_derivative, na.rm = TRUE)) %>%
  arrange(mean_value)  

# Sort the condition factor based on the calculated mean values
organic_inorganic_data_grow$Treatment <- factor(organic_inorganic_data_grow$Treatment, 
                                                levels = mean_values$Treatment)

# Combine mean values with all individual data points
organic_inorganic_data_grow <- merge(organic_inorganic_data_grow, mean_values, by.x = 'Treatment',
                                     by.y = 'Treatment')

# Plot growth rates inorganic data 
ggplot(inorganic_selected, aes(x = Treatment, y = max_derivative)) +
  geom_boxplot() + 
  labs(
    title = "Growth Rates for All Treatments",
    x = " ",
    y = "GR",
    fill = "Treatment"
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    plot.title = element_text(hjust = 0.5) 
  )

# Create the boxplot, fill based on the mean values, and sorted treatments
ggplot(organic_inorganic_data_grow, aes(x = Treatment, y = max_derivative, fill = mean_value)) +
  geom_boxplot() + 
  labs(
    title = "Growth rates distribution",
    x = "Condition",  
    y = "GR", 
    fill = "Mean GR"  
  ) +
  scale_fill_viridis_c(option = "cividis") +  
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    plot.title = element_text(hjust = 0.5)  
  )

# Create the bee swarm plot GR with specific for no-N control
ggplot(distinct_data, aes(x = 1, y = max_derivative, color = Treatment)) + 
  geom_beeswarm(size = 0.3, alpha = 0.7) + 
  geom_hline(yintercept = 0.04, linetype = "dashed", color = "red") + 
  labs(
    title = "Max Growth Rate Distribution",
    x = NULL,  
    y = "Max GR",
    color = "Condition"
  ) +
  scale_color_manual(
    values = c("no N control + C" = "blue", "Other" = "black") 
  ) + 
  theme_minimal() +  # Clean theme
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank()   
  )  
