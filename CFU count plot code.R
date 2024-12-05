# CFU count plot

# Create the data
dataCFU <- data.frame(
  time = c(0, 1, 2, 3.58, 4.58),  # Time as numeric hours
  CFU_counts = c(148, 95, 299, 283, 782)
)

# Transform the CFU counts
dataCFU$CFU_counts_per_litre <- dataCFU$CFU_counts * 10^5

# Define your color palette
color_palette <- viridis(6)[3]
color_palette1 <- viridis(6)[4]

# Plot the data with points and lines
ggplot(dataCFU, aes(x = time, y = CFU_counts_per_litre)) +
  geom_point(color = 'black', size = 3) +      # Add points
  geom_line(color = 'black') +                # Add lines connecting the points
  labs(
    x = "Time (hours)",
    y = "CFU Counts per Liter",
    title = "CFU Counts Over Time 1A01 after nitrogen depletion"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 5, by = 1))  # Adjust x-axis to show whole hours

# Create the bar plot
ggplot(dataCFU, aes(x = as.factor(time), y = CFU_counts_per_litre)) +
  geom_bar(stat = "identity", fill = color_palette1) +  # Use `fill` to set the color of the bars
  labs(
    x = "Time (hours)",
    y = "CFU Counts per Liter",
    title = "CFU Counts Over Time 1A01 after dialysis"
  ) +
  theme_minimal() +
  scale_x_discrete(breaks = seq(0, 4, by = 1))  # Adjust x-axis to show whole hours

