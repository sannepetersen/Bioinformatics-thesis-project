 # HEPES growth rate analysis

# Load packages if needed
library(ggplot2)
library(gridExtra)

# Growth rates
growth_rates_HEPES <- c(
  0.138,
  0.318,
  0.0868,
  0.143,
  0.14,
  0.089,
  0.085,
  0.226,
  0.134,
  0.0327,
  0.0518,
  0.0421)

# Strains
strains_HEPES <- c(
  "4F10",
  "4D01",
  "4H09",
  "A2R16",
  "1A01",
  '3D05',
  "4F10",
  "4D01",
  "4H09",
  "A2R16",
  "1A01",
  '3D05'
)

# Conditions
conditions_HEPES <- c(
  "MBL - N - hepes",
  "MBL - N - hepes",
  "MBL - N - hepes",
  "MBL - N - hepes",
  "MBL - N - hepes",
  "MBL - N - hepes",
  "MBL - N",
  "MBL - N",
  "MBL - N",
  "MBL - N",
  "MBL - N",
  "MBL - N"
)

# Create DataFrame
df_HEPES <- data.frame(
  Strain = strains_HEPES,
  GrowthRate = growth_rates_HEPES,
  Condition = conditions_HEPES
)

# Prepare data for plotting
df_summary_HEPES <- df_HEPES %>%
  group_by(Strain, Condition) %>%
  summarise(AverageGrowthRate = mean(GrowthRate)) %>%
  ungroup()

# Plot growth rates HEPES experiment
ggplot(df_summary_HEPES, aes(x = Condition, y = AverageGrowthRate)) +
  geom_boxplot(aes(fill = Condition), outlier.shape = NA, alpha = 0.8) +  # Boxplot
  geom_jitter(aes(color = AverageGrowthRate), position = position_jitter(width = 0.2), size = 2, alpha = 0.7) +  # Jitter points with gradient
  scale_color_gradient(low = "#00204D", high = "#FFEA46") +  # Gradient for points
  scale_fill_manual(values = c("MBL - N - hepes" = "#FFEA46", "MBL - N" = "#00204D")) +  # Custom colors for boxplots
  labs(
    title = "Growth Rates with and without HEPES",
    x = "Condition",
    y = "GR",
    fill = "Condition",
    color = "Growth Rate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(), legend.position = 'none'
  )

# Yields
max_OD_HEPES <- c(
  0.572,
  0.534,
  0.411,
  0.556,
  0.199,
  0.322,
  
  0.503,
  0.362,
  0.33,
  0.247,
  0.159,
  0.229)

# Create DataFrame
df_HEPES_max_OD <- data.frame(
  Strain = strains_HEPES,
  GrowthRate = max_OD_HEPES,
  Condition = conditions_HEPES
)

# Prepare data for plotting
df_summary_HEPES_max_OD <- df_HEPES_max_OD %>%
  group_by(Strain, Condition) %>%
  summarise(AverageGrowthRate = mean(GrowthRate)) %>%
  ungroup()

# Plot yield data HEPES experiment
ggplot(df_summary_HEPES_max_OD, aes(x = Condition, y = AverageGrowthRate)) +
  geom_boxplot(aes(fill = Condition), outlier.shape = NA, alpha = 0.8) +  # Boxplot
  geom_jitter(aes(color = AverageGrowthRate), position = position_jitter(width = 0.2), size = 2, alpha = 0.7) +  # Jitter points with gradient
  scale_color_gradient(low = "#00204D", high = "#FFEA46") +  # Gradient for points
  scale_fill_manual(values = c("MBL - N - hepes" = "#FFEA46", "MBL - N" = "#00204D")) +  # Custom colors for boxplots
  labs(
    title = "Growth Rates with and without HEPES",
    x = "Condition",
    y = "GR",
    fill = "Condition",
    color = "Growth Rate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(), legend.position = 'none'
  )

# Create a data frame with the correct pairing
df_max_OD <- data.frame(
  max_OD_1 = max_OD_HEPES[1:6],    # First half of max_OD_HEPES
  max_OD_2 = max_OD_HEPES[7:12]    # Second half of max_OD_HEPES
)

# Perform t-test
t_test_result <- t.test(df_max_OD$max_OD_1, df_max_OD$max_OD_2, paired = TRUE)

# Plot yields of with and without hepes groups
ggplot(df_max_OD, aes(x = max_OD_1, y = max_OD_2)) +
  geom_point(color = "blue", size = 4) +   # Scatter plot of max_OD_1 vs max_OD_2
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # 45-degree line
  labs(
    title = "Max OD (HEPES) Comparison",
    x = "Max OD (First Half)",
    y = "Max OD (Second Half)"
  ) +
  theme_minimal()

# Your first plot
plot1 <- ggplot(df_summary_HEPES_max_OD, aes(x = Condition, y = AverageGrowthRate)) +   
  geom_boxplot(aes(fill = Condition), outlier.shape = NA, alpha = 0.8) +  # Boxplot   
  geom_jitter(aes(color = AverageGrowthRate), position = position_jitter(width = 0.2), size = 2, alpha = 0.7) +  # Jitter points with gradient   
  scale_color_gradient(low = "#00204D", high = "#FFEA46") +  # Gradient for points   
  scale_fill_manual(values = c("MBL - N - hepes" = "#FFEA46", "MBL - N" = "#00204D")) +  # Custom colors for boxplots   
  labs(
    title = "Growth Rates with and without HEPES", 
    x = " ", 
    y = "Yield", 
    fill = "Condition", 
    color = "Growth Rate"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.minor = element_blank(), 
    legend.position = 'none'
  )

# Your second plot
plot2 <- ggplot(df_summary_HEPES, aes(x = Condition, y = AverageGrowthRate)) +   
  geom_boxplot(aes(fill = Condition), outlier.shape = NA, alpha = 0.8) +  # Boxplot   
  geom_jitter(aes(color = AverageGrowthRate), position = position_jitter(width = 0.2), size = 2, alpha = 0.7) +  # Jitter points with gradient   
  scale_color_gradient(low = "#00204D", high = "#FFEA46") +  # Gradient for points   
  scale_fill_manual(values = c("MBL - N - hepes" = "#FFEA46", "MBL - N" = "#00204D")) +  # Custom colors for boxplots   
  labs(
    title = " ", 
    x = " ", 
    y = "GR", 
    fill = "Condition", 
    color = "Growth Rate"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.minor = element_blank(), 
    legend.position = 'none'
  )

# Combine the two plots using gridExtra
grid.arrange(plot1, plot2, ncol = 2)

