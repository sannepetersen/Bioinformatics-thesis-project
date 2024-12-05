# Big experiment data
library(readxl)
library(dplyr)
library(hms)
library(tidyr) 

# Set working directory 
setwd('/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/big experiment')

# Download data
raw_data <- read.csv('raw_data.csv')
data_broad_growth_screen <- read.csv('all_data_info.csv')
control_data <- read.csv('control_data.csv')

# Determine treatment for which you want to check the controls
control_selected <- control_data[control_data$Treatment == 'Alanine + C', ]

# Plot the controls
ggplot(control_selected, aes(x = timepoint, y = Value, group = 1)) + 
  geom_point(alpha = 0.6, show.legend = FALSE) + 
  geom_line(color = "blue", size = 1) +  
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red") +  
  labs(
    title = sprintf("Distribution of Control"),
    x = "Timepoints",
    y = "Maximum Value"
  ) +
  theme_minimal() + 
  ylim(0, 1) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Determine treatment for which you want to make the following plots
data_selected <- data_broad_growth_screen[data_broad_growth_screen$Treatment == 'Alanine + C', ]

# Create a boxplot to visualize the maximum values per strain and description
ggplot(data_selected, aes(x = Description, y = Highest_Value)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0.2, alpha = 0.6, show.legend = FALSE) + 
  labs(title = sprintf("Distribution of Maximum Values"),
       x = "Description",
       y = "Maximum Value") +
  theme_minimal() +  # Use a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Determine treatment for which you want to make the following plots
data_selected_raw <- raw_data[raw_data$Treatment == 'Alanine + C', ]

# Plot selected replicates against each other
ggplot(data_selected_raw, aes(x = Value, y = y)) +
  geom_point(alpha = 0.6) +  # Simple scatter plot with points
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +  # y = x line
  labs(
    title = sprintf("Values between replicates"),
    x = "R1",
    y = "R2"
  ) +
  theme_minimal()  # Minimal theme for a clean look

# Determine treatment groups for which you want to make the following plots
inorganic_group <- c('Ammonium + C', 'Nitrite + C', 'Nitrate + C', 'Urea + C',
                     'no N control + C')
organic_group <- c('Alanine + C', 'Glycine + C', 'Serine + C', 'Threonine + C',
                     'Glutamine + C')

# Obtain data from selected group
data_selected_growth <- data_broad_growth_screen[data_broad_growth_screen$Treatment %in% inorganic_group, ]

# Or select a single condition
data_selected_growth <- data_broad_growth_screen[data_broad_growth_screen$Treatment == 'Ammonium + C', ]

# Calculate total and 'growth = 1' counts per order, then calculate percentage
growth_summary <- data_selected_growth %>%
  group_by(Description) %>%
  summarise(
    total_count = n(),  
    growth_count = sum(growth_before == 1, na.rm = TRUE)  
  ) %>%
  mutate(growth_percentage = (growth_count / total_count) * 100) 

# Enhanced barplot with color and labels
ggplot(growth_summary, aes(x = reorder(Description, growth_percentage), y = growth_percentage)) +
  geom_bar(stat = "identity", aes(fill = growth_percentage), width = 0.8) +  
  labs(title = sprintf("Percentage of Growth on Ammonium"),
       x = "Order",
       y = "Growth Percentage (%)") +
  geom_text(aes(label = sprintf("%.1f%%", growth_percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5, color = "white") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "none"  
  )

########## now after no-N control correction
# Obtain data from selected group
data_selected_growth <- data_broad_growth_screen[data_broad_growth_screen$Treatment %in% inorganic_group, ]

# Or select a single condition
data_selected_growth <- data_broad_growth_screen[data_broad_growth_screen$Treatment == 'Ammonium + C', ]

# Calculate total and 'growth = 1' counts per order, then calculate percentage
growth_summary <- data_selected_growth %>%
  group_by(Description) %>%
  summarise(
    total_count = n(),  
    growth_count = sum(growth == 1, na.rm = TRUE)  
  ) %>%
  mutate(growth_percentage = (growth_count / total_count) * 100) 

# Enhanced barplot with color and labels
ggplot(growth_summary, aes(x = reorder(Description, growth_percentage), y = growth_percentage)) +
  geom_bar(stat = "identity", aes(fill = growth_percentage), width = 0.8) +  
  labs(title = sprintf("Percentage of Growth on Ammonium"),
       x = "Order",
       y = "Growth Percentage (%)") +
  geom_text(aes(label = sprintf("%.1f%%", growth_percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5, color = "white") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    legend.position = "none" 
  )

