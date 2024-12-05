# 96-well plate per row
# Code to load plate reader data from csv file.  
library(patchwork)

# Create a data frame with the growth rates for each strain
dataGR_12A <- data.frame(
  Strain = c("3B05", "4F10", "4D01", "12B01"),
  GrowthRate_10x = c(0.0964, 0.419, 0.155, 0.161),
  GrowthRate_50x = c(0.079, 0.329, 0.0675, 0.194),
  GrowthRate_250x = c(0.0901, 0.244, 0.0894, 0.121),
  GrowthRate_1250x = c(0.0602, 0.283, 0.0484, 0.167),
  GrowthRate_6250x = c(0.0414, 0.0508, 0.0507, 0.0495)
)

# Create a data frame with the growth rates for each strain
datalag_12A <- data.frame(
  Strain = c("3B05", "4F10", "4D01", "12B01"),
  Lag_10x = c(10.6, 6.58, 1.34, 5.44),
  Lag_50x = c(28.7, 20.3, 21.2, 25.6),
  Lag_250x = c(42.1, 42.5, 41.9, 42.7),
  Lag_1250x = c(44.5, 44, 44.1, 36.7),
  Lag_6250x = c(45.4, 45.9, 50.9, 28)
)

# Create a data frame with the growth rates for each strain
maxod_12A <- data.frame(
  Strain = c("3B05", "4F10", "4D01", "12B01"),
  OD_10x = c(0.495, 1.01, 0.516, 0.934),
  OD_50x = c(0.486, 1.0603225, 0.5779912, 0.9439809),
  OD_250x = c(0.5019023, 1.0467852, 0.5729817, 0.9685885),
  OD_1250x = c(0.4446994, 1.5217655, 0.3144817, 0.928),
  OD_6250x = c(0.212, 0.255292, 0.1514103, 0.3652877)
)

# Create long df
maxod_long <- maxod_12A %>%
  pivot_longer(cols = starts_with("OD"), names_to = "Condition", values_to = "OD_value")

# ANOVA test
anova_result <- aov(OD_value ~ Condition, data = maxod_long)
summary(anova_result)

# Reshape the data to long format for ggplot
data_long_12A <- dataGR_12A %>%
  pivot_longer(cols = -Strain, names_to = "Dilution", values_to = "GrowthRate") %>%
  mutate(Dilution = factor(Dilution, 
                           levels = c("GrowthRate_10x", "GrowthRate_50x", "GrowthRate_250x", "GrowthRate_1250x", "GrowthRate_6250x"), 
                           labels = c("10x", "50x", "250x", "1250x", "6250x"),
                           ordered = TRUE))  # Set factor levels for x-axis as ordered

# Create a gradient color palette for dilutions
gradient_colors <- colorRampPalette(c("blue", "#FDE725FF"))(length(unique(data_long_12A$Dilution)))

# Plot growth rates 
ggplot(data_long_12A, aes(x = Strain, y = GrowthRate, fill = Dilution)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use dodge position to separate bars
  scale_y_continuous(name = "Growth Rate") +
  scale_fill_manual(values = gradient_colors) +  # Apply custom colors
  scale_x_discrete(name = "Strain") +
  labs(title = "Growth rates at various  initial dilutions", 
       fill = 'Dilution factor') +
  theme_minimal() +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Angle x-axis labels for readability
  )

# Reshape the lag data to long format
datalag_long_12A <- datalag_12A %>%
  pivot_longer(cols = -Strain, names_to = "Dilution", values_to = "LagPhase") %>%
  mutate(Dilution = factor(Dilution,
                           levels = c("Lag_10x", "Lag_50x", "Lag_250x", "Lag_1250x", "Lag_6250x"),
                           labels = c("10x", "50x", "250x", "1250x", "6250x"),
                           ordered = TRUE))


# Create a gradient color palette for dilutions
gradient_colors <- colorRampPalette(c("blue", "#FDE725FF"))(length(unique(datalag_long_12A$Dilution)))

# Plot lag times
ggplot(datalag_long_12A, aes(x = Strain, y = LagPhase, fill = Dilution)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use dodge position to separate bars
  scale_y_continuous(name = "Lag time (h)") +
  scale_fill_manual(values = gradient_colors) +  # Apply custom colors
  scale_x_discrete(name = "Strain") +
  labs(title = "Lag times at various initial dilutions", 
       fill = 'Dilution factor') +
  theme_minimal() +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Angle x-axis labels for readability
  )

# Reshape the lag data to long format
dataOD_long_12A <- maxod_12A %>%
  pivot_longer(cols = -Strain, names_to = "Dilution", values_to = "OD") %>%
  mutate(Dilution = factor(Dilution,
                           levels = c("OD_10x", "OD_50x", "OD_250x", "OD_1250x", "OD_6250x"),
                           labels = c("10x", "50x", "250x", "1250x", "6250x"),
                           ordered = TRUE))

# Create a gradient color palette for dilutions
gradient_colors <- colorRampPalette(c("blue", "#FDE725FF"))(length(unique(dataOD_long_12A$Dilution)))

# Plot yields
ggplot(dataOD_long_12A, aes(x = Strain, y = OD, fill = Dilution)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use dodge position to separate bars
  scale_y_continuous(name = "Yield") +
  scale_fill_manual(values = gradient_colors) +  # Apply custom colors
  scale_x_discrete(name = "Strain") +
  labs(title = "Yields at various initial dilutions", 
       fill = 'Dilution factor') +
  theme_minimal() +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Angle x-axis labels for readability
  )

# Reshape the growth rate data to long format
data_long_12A <- dataGR_12A %>%
  pivot_longer(cols = -Strain, names_to = "Dilution", values_to = "Value") %>%
  mutate(Dilution = factor(Dilution,
                           levels = c("GrowthRate_10x", "GrowthRate_50x", "GrowthRate_250x", "GrowthRate_1250x", "GrowthRate_6250x"),
                           labels = c("10x", "50x", "250x", "1250x", "6250x"),
                           ordered = TRUE)) %>%
  mutate(Metric = "Growth Rate")

# Create the heatmap
ggplot(data_long_12A, aes(x = Dilution, y = Strain, fill = Value)) +      
  geom_tile() +  # Create heatmap with geom_tile
  scale_fill_viridis_c(option = "cividis") +  # Apply the Cividis color palette (continuous color scale)
  scale_x_discrete(name = "Dilution") +  # Label for x-axis
  scale_y_discrete(name = "Strain") +  # Label for y-axis
  labs(title = "Growth Rates at Various Dilutions", fill = 'Growth Rate') +      
  theme_minimal() +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Angle x-axis labels for readability
    plot.title = element_text(size = 14),  # Adjust the title text size
    strip.text = element_text(size = 12),  # Adjust the facet label text size
    panel.grid = element_blank()  # Remove grid lines for better clarity in heatmap
  )

# Reshape the lag phase data to long format
datalag_long_12A <- datalag_12A%>%
  pivot_longer(cols = -Strain, names_to = "Dilution", values_to = "Value") %>%
  mutate(Dilution = factor(Dilution,
                           levels = c("Lag_10x", "Lag_50x", "Lag_250x", "Lag_1250x", "Lag_6250x"),
                           labels = c("10x", "50x", "250x", "1250x", "6250x"),
                           ordered = TRUE)) %>%
  mutate(Metric = "Lag Phase (h)")

# Create the heatmap
ggplot(datalag_long_12A, aes(x = Dilution, y = Strain, fill = Value)) +      
  geom_tile() +  # Create heatmap with geom_tile
  scale_fill_viridis_c(option = "cividis") +  # Apply the Cividis color palette (continuous color scale)
  scale_x_discrete(name = "Dilution") +  # Label for x-axis
  scale_y_discrete(name = "Strain") +  # Label for y-axis
  labs(title = "Growth Rates at Various Dilutions", fill = 'Lag Phase (h)') +      
  theme_minimal() +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Angle x-axis labels for readability
    plot.title = element_text(size = 14),  # Adjust the title text size
    strip.text = element_text(size = 12),  # Adjust the facet label text size
    panel.grid = element_blank()  # Remove grid lines for better clarity in heatmap
  )

# Reshape the OD/Yield data to long format
dataOD_long_12A <- maxod_12A %>%
  pivot_longer(cols = -Strain, names_to = "Dilution", values_to = "Value") %>%
  mutate(Dilution = factor(Dilution,
                           levels = c("OD_10x", "OD_50x", "OD_250x", "OD_1250x", "OD_6250x"),
                           labels = c("10x", "50x", "250x", "1250x", "6250x"),
                           ordered = TRUE)) %>%
  mutate(Metric = "Yield")

# Create the heatmap
ggplot(dataOD_long_12A, aes(x = Dilution, y = Strain, fill = Value)) +      
  geom_tile() +  # Create heatmap with geom_tile
  scale_fill_viridis_c(option = "cividis") +  # Apply the Cividis color palette (continuous color scale)
  scale_x_discrete(name = "Dilution") +  # Label for x-axis
  scale_y_discrete(name = "Strain") +  # Label for y-axis
  labs(title = "Growth Rates at Various Dilutions", fill = 'Yield') +      
  theme_minimal() +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Angle x-axis labels for readability
    plot.title = element_text(size = 14),  # Adjust the title text size
    strip.text = element_text(size = 12),  # Adjust the facet label text size
    panel.grid = element_blank()  # Remove grid lines for better clarity in heatmap
  )

# Combine all datasets into one long dataframe
combined_data_12A <- bind_rows(data_long_12A, datalag_long_12A, dataOD_long_12A)

# Create a gradient color palette for dilutions
gradient_colors <- colorRampPalette(c("blue", "#FDE725FF"))(length(unique(combined_data_12A$Dilution)))

# Pick colors
extended_colors <- c(brewer.pal(5, "Set1"))

# Plot GR, lag times and yields
ggplot(combined_data_12A, aes(x = Strain, y = Value, fill = Dilution)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use dodge position to separate bars
  facet_grid(rows = vars(Metric), scales = "free_y") +  # Facet by metric, allowing different scales for y-axis
  scale_y_continuous(name = "Value") +
  scale_fill_viridis_d(option = "cividis") +  # Apply the Cividis color palette (option "C" for Cividis)
  scale_x_discrete(name = "Strain") +
  labs(title = "Growth Rates, Lag Times, and Yields at Various Dilutions", fill = 'Dilution factor') +
  theme_minimal() +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Angle x-axis labels for readability
    plot.title = element_text(size = 10)  # Adjust the title text size (default is usually 14)
  )

# Growth Rate heatmap
heatmap_GR <- ggplot(data_long_12A, aes(x = Dilution, y = Strain, fill = Value)) +      
  geom_tile() +  # Create heatmap with geom_tile
  scale_fill_viridis_c(option = "cividis") +  # Apply the Cividis color palette (continuous color scale)
  scale_x_discrete(name = "Dilution") +  # Label for x-axis
  scale_y_discrete(name = "Strain") +  # Label for y-axis
  labs(title = "Growth Rates", fill = 'Growth Rate') +      
  theme_minimal() +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Angle x-axis labels for readability
    plot.title = element_text(size = 14),  # Adjust the title text size
    strip.text = element_text(size = 12),  # Adjust the facet label text size
    panel.grid = element_blank()  # Remove grid lines for better clarity in heatmap
  )

# Lag Phase heatmap
heatmap_Lag <- ggplot(datalag_long_12A, aes(x = Dilution, y = Strain, fill = Value)) +      
  geom_tile() +  # Create heatmap with geom_tile
  scale_fill_viridis_c(option = "cividis") +  # Apply the Cividis color palette (continuous color scale)
  scale_x_discrete(name = "Dilution") +  # Label for x-axis
  scale_y_discrete(name = "Strain") +  # Label for y-axis
  labs(title = "Lag Phase", fill = 'Lag Phase (h)') +      
  theme_minimal() +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Angle x-axis labels for readability
    plot.title = element_text(size = 14),  # Adjust the title text size
    strip.text = element_text(size = 12),  # Adjust the facet label text size
    panel.grid = element_blank()  # Remove grid lines for better clarity in heatmap
  )

# Yield (OD) heatmap
heatmap_Yield <- ggplot(dataOD_long_12A, aes(x = Dilution, y = Strain, fill = Value)) +      
  geom_tile() +  # Create heatmap with geom_tile
  scale_fill_viridis_c(option = "cividis") +  # Apply the Cividis color palette (continuous color scale)
  scale_x_discrete(name = "Dilution") +  # Label for x-axis
  scale_y_discrete(name = "Strain") +  # Label for y-axis
  labs(title = "Yield", fill = 'Yield') +      
  theme_minimal() +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Angle x-axis labels for readability
    plot.title = element_text(size = 14),  # Adjust the title text size
    strip.text = element_text(size = 12),  # Adjust the facet label text size
    panel.grid = element_blank()  # Remove grid lines for better clarity in heatmap
  )

# Combine the plots using patchwork
combined_heatmap <- heatmap_GR / heatmap_Lag / heatmap_Yield

# Print the combined heatmap
print(combined_heatmap)
ggsave("combined_heatmap.png", plot = combined_heatmap, width = 5, height = 13, dpi = 300)

