# Plot no-N control

# Filter for the first 40 unique names
no_N_random_names <- unique(all_derivatives_timepoints$Name)[1:40]

# Obtain no-N control data
no_N_random_plot <- all_derivatives_timepoints[all_derivatives_timepoints$Treatment == 'no N control + C', ]
no_N_random_plot <- no_N_random_plot %>%
  filter(Name %in% no_N_random_names) %>%
  mutate(group = 'no N control + C') %>%
  mutate(timepoints = timepoints_F)

# Create the plot
ggplot(no_N_random_plot, aes(x = timepoints, y = Highest_Value, color = group)) +   
  geom_point(show.legend = FALSE, color = "#08519C") +  # Scatter plot without legend
  geom_line(aes(group = interaction(group, Name)), color = "#08519C", show.legend = FALSE) +  # Lines in blue without legend
  labs(x = "Time (h)", y = "OD600", color = 'Amino acids') +  
  ggtitle("Growth curves of no-N") +   
  theme_minimal()
