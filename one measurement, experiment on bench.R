# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Define your data manually or load from a CSV
datamanydilutions <- data.frame(
  Strain = c("1A01", "3B05", "4H09", "3F01", "4A10", "4D01", "5G01", "12B01"),
  '0' = c(0.466, 0.515, 0.262, 0.334, 0.169, 0.186, 0.237, 0.56),
  '44' = c(0.015, 0.015, 0.096, 0.007, 0.018, -0.002, 0.009, 0.014),
  '165' = c(-0.013, -0.015, 0.021, -0.017, -0.014, -0.016, 0.007, -0.012)
)

# Reshape the data into long format for ggplot
data_long <- datamanydilutions %>%
  pivot_longer(cols = -Strain, names_to = "Time", values_to = "OD") 
data_long$Time <- as.numeric(gsub("X", "", data_long$Time))

# Pick colors
extended_colors <- viridis(option = 'cividis', 8)

# Plot the data
ggplot(data_long, aes(x = Time, y = OD, group = Strain, color = Strain)) +
  geom_line(size = 1) +           # Create a line for each strain
  scale_color_manual(values = extended_colors)+
  geom_point(size = 3) +          # Add points for each time point
  theme_minimal() +               # Use a minimal theme
  labs(title = "OD600 After 3 Dilutions in no-N control",
       x = "Time (h)",
       y = "OD600",
       color = "Strain") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position='none')  # Tilt x-axis labels for better readability
