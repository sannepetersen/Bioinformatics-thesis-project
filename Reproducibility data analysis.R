# Reproduction experiment analysis

# Create the dataset
data_reproducibility12 <- data.frame(
  Nitrogen_Source = c(rep('ammonium', 11), rep('nitrite', 11), rep('nitrate', 11), rep('urea', 11), 
                      rep('no N', 11), rep('proline', 11), rep('serine', 11), rep('glutamine', 11)),
  dataset1 = c(0.5107437, 0.440387, 0.5830024, 0.4829497, 0.1392308, 0.3028536, 0.2856288, 0.1013056,
               0.398028, 0.6347762, 0.4398547, #ammonium
               0.21423922, 0.13042073, 0.44278504, 0.21038203, 0.13300801, 0.26724501, 0.12427721, 
               0.36910875, 0.41606479, 0.39724031, 0.30993799, #nitrite
               0.37644571, 0.05089864, 0.41001702, 0.21043669, 0.0763577, 0.29620506, 0.08600094, 
               0.31204182, 0.35724594, 0.43344057, 0.36793972, #nitrate
               0.52646281, 0.0858156, 0.60231259, 0.27844892, 0.06562543, 0.34345447, 0.05973743, 
               0.32228191, 0.06764209, 0.62058848, 0.2294078, #urea
               0.0786794, 0.05397303, 0.06403493, 0.19510841, 0.03840473, 0.22212648, 0.04845876,
               0.11826492, 0.06348014, 0.05728129, 0.06251492, #no N
               0.11167472, 0.21713297, 0.39506762, 0.23153438, 0.05678172, 0.27469363, 0.24737961, 
               0.08218196, 0.2768097, 0.69377948, 0.17017753, #proline
               0.47269078, 0.3505235, 0.093779955, 0.16989532, 0.07665913, 0.21383937, 0.43879216, 
               0.0566578, 0.22239753, 0.30599621, 0.47895141, #serine
               0.67443645, 0.42066662, 0.59319351, 0.2386743, 0.16423722, 0.44280846, 0.42623703, 
               0.25276263, 0.45456553, 0.38830527, 0.35427124 #glutamine
               ),
  dataset2 = c(0.47534641, 0.42260302, 0.54558481, 0.42653832, 0.16513553, 
               0.301885, 0.44196194, 0.04519329, 0.38762837, 0.42928083, 0.61135631,  #ammonium
               0.14920274, 0.18110929, 0.45274707, 0.40137181, 0.11855928, 
               0.20537413, 0.14615093, 0.07732248, 0.32633005, 0.33062021, 0.31508065,  #nitrite
               0.3665863, 0.1363512, 0.4064613, 0.3600158, 0.1007532, 0.2532123, 
               0.034754772, 0.066687871, 0.4135788, 0.4178758, 0.3951556, #nitrate
               0.47186613, 0.10834893, 0.49736334, 0.41827812, 0.17036697, 
               0.31106353, 0.16385643, 0.110002022, 0.06269978, 0.44142524, 0.25471216,  #urea
               0.15999418, 0.08808351, 0.10064208, 0.08849964, 0.05377594, 
               0.14805282, 0.07156027, 0.07658336, 0.0631487, 0.32604745, 0.06666837, #no nitrogen
               0.1337633, 0.27260577, 0.31344476, 0.45576439, 0.0634418, 
               0.2475101, 0.29691014, 0.09531885, 0.31962566, 0.5228692, 0.12218376,  #proline
               0.4039963, 0.4208345, 0.1496777, 0.1645598, 0.1113685, 
               0.17982, 0.431883, 0.1952597, 0.2606074, 0.9289968, 0.4576405,  #serine
               0.71853907, 0.47620518, 0.5439861, 0.44406854, 0.22038093, 
               0.40720635, 0.42839657, 0.083811682, 0.43389617, 0.41463786, 0.41544831) #glutamine
)

# Create a function to calculate R²
calculate_r_squared <- function(df) {
  model <- lm(dataset2 ~ dataset1, data = df)
  summary(model)$r.squared
}

# Define custom order for nitrogen sources
custom_order <- c("ammonium", "nitrite", "nitrate", "urea", "proline", "serine", "glutamine", "no N")

# Convert Nitrogen_Source to a factor and specify the level order
data_reproducibility12$Nitrogen_Source <- factor(data_reproducibility12$Nitrogen_Source, levels = custom_order)

# Group by Nitrogen_Source and calculate R² for each group
r_squared_per_condition <- data_reproducibility12 %>%
  group_by(Nitrogen_Source) %>%
  summarise(R_squared = calculate_r_squared(cur_data()))

# Plot the R² values per nitrogen source
ggplot(r_squared_per_condition, aes(x = Nitrogen_Source, y = R_squared)) +
  geom_bar(stat = "identity", fill = "#414487FF") +  # Custom color for the bars
  labs(title = "R² Values between Growth Rates",
       x = "Nitrogen Source",
       y = "R²") +
  theme_minimal() +
  ylim(0,1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Growth rate distribution dataset 1
ggplot(data_reproducibility12, aes(x = Nitrogen_Source, y = dataset1)) +
  geom_boxplot() +
  labs(title = "Growth Rate Distribution per Nitrogen Source",
       x = "Nitrogen Source",
       y = "Growth Rate") +
  theme_minimal()

# Growth rate distribution dataset 2
ggplot(data_reproducibility12, aes(x = Nitrogen_Source, y = dataset2)) +
  geom_boxplot() +
  labs(title = "Growth Rate Distribution per Nitrogen Source",
       x = "Nitrogen Source",
       y = "Growth Rate") +
  theme_minimal()

# Reshape the data into long format
data_long <- data_reproducibility12 %>%
  pivot_longer(cols = c(dataset1, dataset2),
               names_to = "Dataset",
               values_to = "GrowthRate")

custom_colors <- c("dataset1" = "#FDE725FF",  # Blue for dataset1
                   "dataset2" = "#414487FF")  # Orange for dataset2

# Plot the combined boxplot with custom colors
ggplot(data_long, aes(x = Nitrogen_Source, y = GrowthRate, fill = Dataset)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 0, outlier.stroke = 0.5) +  # Use hollow points for outliers
  labs(title = "Growth Rate Distribution",
       x = "Nitrogen Source",
       y = "Growth Rate",
       fill = "Dataset") +
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  theme_minimal() +  # Set a larger base font size
  geom_jitter(aes(color = Dataset), width = 0.2, alpha = 0.5) +  # Add jittered points for individual data
  scale_color_manual(values = custom_colors)  # Use same colors for jitter points

# Perform correlation based on R1 and R2
correlation_results <- data_reproducibility12 %>%
  group_by(Nitrogen_Source) %>%
  summarise(correlation = cor(dataset1, dataset2))

# Choose colours
colors <- c("ammonium" = "blue", "no N" = "black", "nitrite" = "#2A788EFF", "nitrate" = "orange", "urea" = "#FDE725FF", 'proline' = "#7AD151FF", 'serine' = '#B12A90FF', 'glutamine' = "#6A00A8FF")

# Manually specify the coordinates for labels
manual_labels <- data.frame(
  Nitrogen_Source = c("ammonium", "no N", "nitrite", "nitrate", "urea", "proline", "serine", "glutamine"),
  x = c(0.32, 0.092, 0.1652, 0.13, 0.23, 0.24, 0.158, 0.21), # Replace these with your desired x positions
  y = c(0.23, 0.05, 0.1, 0.11, 0.16, 0.2, 0.202, 0.195) # Replace these with your desired y positions
)

# Plot growth rate lm per condition
ggplot(data_reproducibility12, aes(x = dataset1, y = dataset2, color = Nitrogen_Source)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = colors) + # Apply custom color palette
  labs(title = "LM Growth Rates", 
       color = 'Nitrogen Source', 
       x = 'Growth rate (dataset 1)', 
       y = 'Growth rate (dataset 2)') +
  theme_minimal() +
  geom_text(data = manual_labels, aes(x = x, y = y, label = Nitrogen_Source),
            size = 3,
            vjust = -0.5, hjust = 1.1, color = "black")+ # Add manually positioned labels
theme(legend.position = "none") # Remove the legend

# Create vectors for growth rates, conditions, and strains 
growth_ratesR3 <- c(
  0.422, 0.134, 0.279, 0.531, 0.131, 0.239,
  0.178, 0.21, 0.412, 0.133, 0.328, 0.121,
  0.252, 0.35, 0.107, 0.431, 0.0603, 0.0554,
  0.515, 0.106, 0.0724, 0.0463, 0.0392, 0.0415,
  0.0622, 0.372, 0, 0.196, 0.233, 0.504,
  0.243, 0.058, 0.162, 0.232, 0.106, 0.427,
  0.0855, 0.321, 0.44, 0.225
)

# New growth rates for Replicate 2
growth_ratesR2 <- c(
  0.398, 0.0421, 0.356, 0.404, 0.553, 0.365,
  0.0656, 0.291, 0.304, 0.274, 0.329, 0.149,
  0.372, 0.377, 0.355, 0.39, 0.103, 0.0556,
  0.401, 0.235, 0.0811, 0.0729, 0.0561, 0.289,
  0.0373, 0.425, 0.0932, 0.288, 0.475, 0.111,
  0.15, 0.178, 0.24, 0.358, 0.416, 0.41,
  0.0819, 0.395, 0.385, 0.375
)

# Growth rates for Replicate 1 (as provided earlier)
growth_ratesR1 <- c(
  0.0288, 0.365, 0.582, 0.393, 0.000547, 0.117,
  0.374, 0.346, 0.275, 0, 0.0681,
  0.325, 0.384, 0.331, 0.18, 0.0505,
  0.0632, 0.561, 0.211, 0, 0.0343,
  0.057, 0.0481, 0.0588, 0, 0.0481,
  0.255, 0.619, 0.152, 0, 0.0715,
  0.207, 0.28, 0.433, 0,
  0.149, 0.417, 0.355, 0.327, 0
)
  
# Conditions and strains (same as before)
conditions <- c("ammonium", "ammonium", "ammonium", "ammonium", 'ammonium',
                "nitrite", "nitrite", "nitrite", "nitrite", 'nitrite',
                "nitrate", "nitrate", "nitrate", "nitrate", 'nitrate',
                "urea", "urea", "urea", "urea", 'urea',
                "no N", "no N", "no N", "no N", 'no N',
                "proline", "proline", "proline", "proline", 'proline',
                "serine", "serine", "serine", "serine", 'serine',
                "glutamine", "glutamine", "glutamine", "glutamine", 'glutamine')

strains <- c("3B05", "4A10", "4D01", '5G01', "12B01", 
             "3B05", "4A10", "4D01", '5G01', "12B01", 
             "3B05", "4A10", "4D01", '5G01', "12B01", 
             "3B05", "4A10", "4D01", '5G01', "12B01", 
             "3B05", "4A10", "4D01", '5G01', "12B01", 
             "3B05", "4A10", "4D01", '5G01', "12B01", 
             "3B05", "4A10", "4D01", '5G01', "12B01", 
             "3B05", "4A10", "4D01", '5G01', "12B01")

# Create the data frame for the three replicates together
growth_data <- data.frame(Condition = conditions, Strain = strains, GrowthRate_Rep1 = growth_ratesR1, GrowthRate_Rep2 = growth_ratesR2, GrowthRate_Rep3 = growth_ratesR3)

# Create a scatter plot for Rep1 vs Rep2
plot(growth_data$GrowthRate_Rep1, growth_data$GrowthRate_Rep2, 
     main = "Replicate 1 vs Replicate 2", 
     xlab = "Growth Rate Replicate 1", 
     ylab = "Growth Rate Replicate 2", 
     pch = 19, col = "blue")
# Add a regression line
abline(lm(growth_data$GrowthRate_Rep2 ~ growth_data$GrowthRate_Rep1), col = "red")

# Create a scatter plot for Rep2 vs Rep3
plot(growth_data$GrowthRate_Rep2, growth_data$GrowthRate_Rep3, 
     main = "Replicate 2 vs Replicate 3", 
     xlab = "Growth Rate Replicate 2", 
     ylab = "Growth Rate Replicate 3", 
     pch = 19, col = "green")

# Add a regression line
abline(lm(growth_data$GrowthRate_Rep3 ~ growth_data$GrowthRate_Rep2), col = "red")

# Create a scatter plot for Rep1 vs Rep3
plot(growth_data$GrowthRate_Rep1, growth_data$GrowthRate_Rep3, 
     main = "Replicate 1 vs Replicate 3", 
     xlab = "Growth Rate Replicate 1", 
     ylab = "Growth Rate Replicate 3", 
     pch = 19, col = "purple")

# Add a regression line
abline(lm(growth_data$GrowthRate_Rep3 ~ growth_data$GrowthRate_Rep1), col = "red")

# Calculate R^2 for Rep1 vs Rep2
model_R1_R2 <- lm(growth_data$GrowthRate_Rep2 ~ growth_data$GrowthRate_Rep1)
R2_R1_R2 <- summary(model_R1_R2)$r.squared

# Calculate R^2 for Rep2 vs Rep3
model_R2_R3 <- lm(growth_data$GrowthRate_Rep3 ~ growth_data$GrowthRate_Rep2)
R2_R2_R3 <- summary(model_R2_R3)$r.squared

# Calculate R^2 for Rep1 vs Rep3
model_R1_R3 <- lm(growth_data$GrowthRate_Rep3 ~ growth_data$GrowthRate_Rep1)
R2_R1_R3 <- summary(model_R1_R3)$r.squared

# Plot Rep1 vs Rep2 with R^2
plot(growth_data$GrowthRate_Rep1, growth_data$GrowthRate_Rep2, 
     main = paste("Replicate 1 vs Replicate 2, R² = ", round(R2_R1_R2, 3)), 
     xlab = "Growth Rate Replicate 1", 
     ylab = "Growth Rate Replicate 2", 
     pch = 19, col = "blue")
abline(model_R1_R2, col = "red")

# Plot Rep2 vs Rep3 with R^2
plot(growth_data$GrowthRate_Rep2, growth_data$GrowthRate_Rep3, 
     main = paste("Replicate 2 vs Replicate 3, R² = ", round(R2_R2_R3, 3)), 
     xlab = "Growth Rate Replicate 2", 
     ylab = "Growth Rate Replicate 3", 
     pch = 19, col = "green")
abline(model_R2_R3, col = "red")

# Plot Rep1 vs Rep3 with R^2
plot(growth_data$GrowthRate_Rep1, growth_data$GrowthRate_Rep3, 
     main = paste("Replicate 1 vs Replicate 3, R² = ", round(R2_R1_R3, 3)), 
     xlab = "Growth Rate Replicate 1", 
     ylab = "Growth Rate Replicate 3", 
     pch = 19, col = "purple")
abline(model_R1_R3, col = "red")

# Boxplot for Growth Rate Rep1 by Condition
boxplot(GrowthRate_Rep1 ~ Condition, data = growth_data, 
        main = "Growth Rate Replicate 1 by Condition", 
        xlab = "Condition", 
        ylab = "Growth Rate", 
        col = "lightblue", las = 2)

# Boxplot for Growth Rate Rep2 by Condition
boxplot(GrowthRate_Rep2 ~ Condition, data = growth_data, 
        main = "Growth Rate Replicate 2 by Condition", 
        xlab = "Condition", 
        ylab = "Growth Rate", 
        col = "lightgreen", las = 2)

# Boxplot for Growth Rate Rep3 by Condition
boxplot(GrowthRate_Rep3 ~ Condition, data = growth_data, 
        main = "Growth Rate Replicate 3 by Condition", 
        xlab = "Condition", 
        ylab = "Growth Rate", 
        col = "lightpink", las = 2)

# Calculate the average growth rate per condition
growth_data <- growth_data %>%
  rowwise() %>%
  mutate(AverageGrowthRate = mean(c(GrowthRate_Rep1, GrowthRate_Rep2, GrowthRate_Rep3), na.rm = TRUE))

# Create the boxplot
ggplot(growth_data, aes(x = Condition, y = AverageGrowthRate)) +
  geom_boxplot(color = "black") +
  labs(title = "Average Growth Rate per Condition",
       x = "Condition",
       y = "Average Growth Rate") +
  theme_minimal()

# Create a function to calculate R²
calculate_r_squared <- function(x, y) {
  model <- lm(y ~ x)
  summary(model)$r.squared
}

# Convert Condition to a factor and specify the level order
growth_data$Condition <- factor(growth_data$Condition, levels = custom_order)

# Function to calculate R² values for different replicate pairs by condition
calculate_r_squared_per_condition <- function(df, x, y) {
  df %>%
    group_by(Condition) %>%
    summarise(R_squared = calculate_r_squared(!!sym(x), !!sym(y)), .groups = 'drop')
}

# Function to calculate R² values for different replicate pairs by strain
calculate_r_squared_per_strain <- function(df, x, y) {
  df %>%
    group_by(Strain) %>%
    summarise(R_squared = calculate_r_squared(!!sym(x), !!sym(y)), .groups = 'drop')
}

# Calculate R² for Rep1 vs Rep2, Rep2 vs Rep3, and Rep1 vs Rep3
r_squared_rep1_rep2 <- calculate_r_squared_per_condition(growth_data, "GrowthRate_Rep1", "GrowthRate_Rep2")
r_squared_rep2_rep3 <- calculate_r_squared_per_condition(growth_data, "GrowthRate_Rep2", "GrowthRate_Rep3")
r_squared_rep1_rep3 <- calculate_r_squared_per_condition(growth_data, "GrowthRate_Rep1", "GrowthRate_Rep3")
r_squared_rep1_rep2strain <- calculate_r_squared_per_strain(growth_data, "GrowthRate_Rep1", "GrowthRate_Rep2")
r_squared_rep2_rep3strain <- calculate_r_squared_per_strain(growth_data, "GrowthRate_Rep2", "GrowthRate_Rep3")
r_squared_rep1_rep3strain <- calculate_r_squared_per_strain(growth_data, "GrowthRate_Rep1", "GrowthRate_Rep3")

# Combine the results into one data frame
r_squared_combined <- rbind(
  mutate(r_squared_rep1_rep2, Comparison = "Rep1 vs Rep2"),
  mutate(r_squared_rep2_rep3, Comparison = "Rep2 vs Rep3"),
  mutate(r_squared_rep1_rep3, Comparison = "Rep1 vs Rep3")
)
r_squared_combinedstrain <- rbind(
  mutate(r_squared_rep1_rep2strain, Comparison = "Rep1 vs Rep2"),
  mutate(r_squared_rep2_rep3strain, Comparison = "Rep2 vs Rep3"),
  mutate(r_squared_rep1_rep3strain, Comparison = "Rep1 vs Rep3")
)

# Plot the R² values per nitrogen source for each comparison
ggplot(r_squared_combined, aes(x = Condition, y = R_squared, fill = Comparison)) +
  geom_bar(stat = "identity", position = "dodge") +  # Dodge to show bars side by side
  labs(title = "R² Values between Growth Rates",
       x = "Nitrogen Source",
       y = "R²") +
  theme_minimal() +
  ylim(0, 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#414487FF", "#2A788EFF", "#22A884FF"))

# Plot the R² values per nitrogen source for each comparison
ggplot(r_squared_combinedstrain, aes(x = Strain, y = R_squared, fill = Comparison)) +
  geom_bar(stat = "identity", position = "dodge") +  # Dodge to show bars side by side
  labs(title = "R² Values between Growth Rates",
       x = "Strain",
       y = "R²") +
  theme_minimal() +
  ylim(0, 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#414487FF", "#2A788EFF", "#22A884FF"))

# Create the first plot (R² values per nitrogen source)
plot_nitrogen <- ggplot(r_squared_combined, aes(x = Condition, y = R_squared, fill = Comparison)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  labs(title = "R² Values between Growth Rates per Nitrogen Source", 
       x = "Nitrogen source", 
       y = "R²") + 
  theme_minimal() + 
  ylim(0, 1) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = brewer.pal(3, "Set1"))

# Create the second plot (R² values per strain)
plot_strain <- ggplot(r_squared_combinedstrain, aes(x = Strain, y = R_squared, fill = Comparison)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  labs(title = "R² Values between Growth Rates per Strain", 
       x = "Strain", 
       y = "R²") + 
  theme_minimal() + 
  ylim(0, 1) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = brewer.pal(3, "Set1")) +
  coord_flip()  # Flip the coordinates to make this plot horizontal

# Arrange both plots in one graph
combined_plot <- plot_nitrogen / plot_strain  # Use '/' to stack plots vertically

# Show the combined plot
print(combined_plot)

# Reshape the data into long format to include all three replicates
growth_data_long <- growth_data %>%
  pivot_longer(cols = c(GrowthRate_Rep1, GrowthRate_Rep2, GrowthRate_Rep3), 
               names_to = "Replicate", 
               values_to = "GrowthRate")

# Define custom colors for each replicate
custom_colors <- c("GrowthRate_Rep1" = "#FDE725FF",  # Yellow
                   "GrowthRate_Rep2" = "#7AD151FF",  # Blue
                   "GrowthRate_Rep3" = "blue")  # Green

ggplot(growth_data_long, aes(x = Condition, y = GrowthRate, fill = Replicate)) + 
  geom_boxplot(outlier.shape = 21, outlier.size = 0, outlier.stroke = 0.5, alpha = 0.7) +  # Use hollow points for outliers
  labs(title = "Growth Rate Distribution", 
       x = "Nitrogen Source", 
       y = "Growth Rate", 
       fill = "Replicate") + 
  geom_jitter(aes(color = Replicate), width = 0.2, alpha = 0.5) +  # Add jittered points for individual data
  scale_fill_manual(values = custom_colors, labels = c("Replicate 1", "Replicate 2", "Replicate 3")) +  # Apply custom colors and custom names
  scale_color_manual(values = custom_colors, labels = c("Replicate 1", "Replicate 2", "Replicate 3")) +  # Use same colors for jitter points
  theme_minimal() +  
  guides(fill = "none") +  # Remove the fill legend (for boxplots)
  theme(legend.key.size = unit(0.5, "cm"),  # Reduce size of the legend boxes
        legend.text = element_text(size = 8),  # Reduce text size
        legend.title = element_text(size = 9),  # Reduce title size
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Calculate correlation for each nitrogen source across replicates
correlation_results <- growth_data %>%
  group_by(Condition) %>%
  summarise(correlation_R1_R2 = cor(GrowthRate_Rep1, GrowthRate_Rep2),
            correlation_R1_R3 = cor(GrowthRate_Rep1, GrowthRate_Rep3),
            correlation_R2_R3 = cor(GrowthRate_Rep2, GrowthRate_Rep3))

# Print correlation results
print(correlation_results)

# Example correlation results (replace with your actual results)
correlation_results <- growth_data %>%
  group_by(Condition) %>%
  summarise(
    R1_R2 = cor(GrowthRate_Rep1, GrowthRate_Rep2),
    R1_R3 = cor(GrowthRate_Rep1, GrowthRate_Rep3),
    R2_R3 = cor(GrowthRate_Rep2, GrowthRate_Rep3)
  )

# Reshape the data to long format for plotting
correlation_long <- correlation_results %>%
  pivot_longer(
    cols = starts_with("R"),
    names_to = "Comparison",
    values_to = "Correlation"
  )

# Create a heatmap
ggplot(correlation_long, aes(x = Condition, y = Comparison, fill = Correlation)) +
  geom_tile() +
  scale_fill_viridis_c(option = 'cividis')+
  labs(
    title = "Correlation Heatmap Between Growth Rates of Replicates",
    x = "Condition",
    y = "Replicate Comparison",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Create the density plot
ggplot(correlation_long, aes(x = Correlation, fill = Comparison)) +
  scale_fill_viridis_d()+
  geom_density(alpha = 0.4) +  # Add density curves with some transparency
  labs(
    title = "Density Plot of Correlation Values Growth Rates",
    x = "Correlation",
    y = "Density",
    color = "Comparison",
    fill = "Comparison"
  ) +
  theme_minimal()

# Create vectors for growth rates, conditions, and strains
max_ODR1 <- c(0.8762036, 0.557441, 0.6715807, 0.9040606, 0.9727884, 
              0.76644957, 0.59185958, 0.59528928, 0.56514638, 0.60588093,
              0.7911513, 0.7758601, 0.5086153, 0.5119105, 0.7458521, 
              0.95081498, 0.70680388, 0.35427512, 0.79835544, 0.56725582,
              0.7516186, 0.3456924, 0.3403757, 0.5129204, 0.3911171, 
              1.3076349, 1.2074291, 0.9068044, 1.4559359, 0.9665707,
              0.75954031, 0.53718216, 0.77766918, 0.89972874, 0.98677421,
              0.69960226, 1.03506215, 0.76688674, 0.8890449, 0.94943843)

# New growth rates for Replicate 2
max_ODR2 <- c(1.022208, 0.4384929, 0.6231337, 1.0261436, 1.1206679, 0.7639679, 
              0.5089809, 0.4804763, 0.5714629, 0.7778248, 0.5393771, 0.6412476,
              0.5147014, 0.556444, 0.8156772, 0.8953626, 0.6920674, 0.3269527, 
              0.7360106, 0.652062, 0.4908981, 0.5522888, 0.3214194, 1.0111223, 
              0.4296198, 1.8328377, 1.7973774, 1.0895566, 1.3162638, 0.8868172,
              0.9689336, 0.8542993, 0.9778114, 1.761513, 1.1800423, 1.18225787, 
              1.12169707, 0.82021366, 0.88799689, 0.83725897 )

# Growth rates for Replicate 1 (as provided earlier)
max_ODR3 <- c(1.30935938, 0.76906407, 0.51004253
              , 0.76408273
              , 0.44557538, 0.73236037, 0.66335809
              , 0.46894231
              , 0.6965617
              , 0.62675408, 0.78558369, 0.66610646
              ,0.46545656
              ,0.37939726
              ,0.42433663, 1.17068655, 0.68084921
              ,0.33657721
              ,0.77530626
              ,0.43145411, 0.58652089, 0.27429342
              ,0.31206307
              ,0.28246408
              ,0.3116238, 1.64901091, 1.02085786
              ,1.16179094
              ,1.25937433
              ,1.02065235, 1.34468569, 0.29328278
              ,0.7112816
              ,0.68925867
              ,0.94964423, 1.24914237, 0.61006758
              ,0.63757192
              ,0.89014332
              ,0.79079669)

# Conditions and strains (same as before)
conditions <- c("ammonium", "ammonium", "ammonium", "ammonium", 'ammonium',
                "nitrite", "nitrite", "nitrite", "nitrite", 'nitrite',
                "nitrate", "nitrate", "nitrate", "nitrate", 'nitrate',
                "urea", "urea", "urea", "urea", 'urea',
                "no N", "no N", "no N", "no N", 'no N',
                "proline", "proline", "proline", "proline", 'proline',
                "serine", "serine", "serine", "serine", 'serine',
                "glutamine", "glutamine", "glutamine", "glutamine", 'glutamine')

strains <- c("3B05", "4A10", "4D01", '5G01', "12B01", 
             "3B05", "4A10", "4D01", '5G01', "12B01", 
             "3B05", "4A10", "4D01", '5G01', "12B01", 
             "3B05", "4A10", "4D01", '5G01', "12B01", 
             "3B05", "4A10", "4D01", '5G01', "12B01", 
             "3B05", "4A10", "4D01", '5G01', "12B01", 
             "3B05", "4A10", "4D01", '5G01', "12B01", 
             "3B05", "4A10", "4D01", '5G01', "12B01")

order <- c('Pseudomonadales', 'Rhodobacterales', 'Alteromonadales', 'Pseudomonadales', 'Vibrionales',
           'Pseudomonadales', 'Rhodobacterales', 'Alteromonadales', 'Pseudomonadales', 'Vibrionales',
           'Pseudomonadales', 'Rhodobacterales', 'Alteromonadales', 'Pseudomonadales', 'Vibrionales',
           'Pseudomonadales', 'Rhodobacterales', 'Alteromonadales', 'Pseudomonadales', 'Vibrionales',
           'Pseudomonadales', 'Rhodobacterales', 'Alteromonadales', 'Pseudomonadales', 'Vibrionales',
           'Pseudomonadales', 'Rhodobacterales', 'Alteromonadales', 'Pseudomonadales', 'Vibrionales',
           'Pseudomonadales', 'Rhodobacterales', 'Alteromonadales', 'Pseudomonadales', 'Vibrionales',
           'Pseudomonadales', 'Rhodobacterales', 'Alteromonadales', 'Pseudomonadales', 'Vibrionales')

# Create the data frame
max_ODdata <- data.frame(Condition = conditions, Strain = strains, maxOD_Rep1 = max_ODR1, maxOD_Rep2 = max_ODR2, maxOD_Rep3 = max_ODR3, order = order)

# Create a function to calculate R² for a given strain
calculate_r2 <- function(strain_data) {
  # Fit a linear model between maxOD_Rep1 and maxOD_Rep2
  lm1 <- lm(maxOD_Rep2 ~ maxOD_Rep1, data = strain_data)
  lm2 <- lm(maxOD_Rep3 ~ maxOD_Rep1, data = strain_data)
  lm3 <- lm(maxOD_Rep3 ~ maxOD_Rep2, data = strain_data)
  
  # Extract R² values
  r2_values <- c(summary(lm1)$r.squared, summary(lm2)$r.squared, summary(lm3)$r.squared)
  
  # Return R² values as a named vector
  return(r2_values)
}

# Apply the function to calculate R² for each strain and get a data frame
r2_values_list <- by(max_ODdata, max_ODdata$Strain, calculate_r2)

# Convert the list to a data frame
r2_df <- as.data.frame(do.call(rbind, r2_values_list))

# Set column names
colnames(r2_df) <- c("R2_Rep1_Rep2", "R2_Rep1_Rep3", "R2_Rep2_Rep3")

# Add the strain names as a column
r2_df$Strain <- rownames(r2_df)

# Print the results
print(r2_df)

# Plot yields R1 vs R2
ggplot(max_ODdata, aes(x = maxOD_Rep1, y = maxOD_Rep2, color = order)) + 
  geom_point(size = 3) +  # Plot points
  scale_color_manual(values=color_orders) +  # Use a color scale for order
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add line y = x
  labs(
    title = "Max OD Rep1 vs Max OD Rep2",
    x = "Yield (R1)",
    y = "Yield (R2)",
    color = "Order"  # Legend title for the color scale
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "right",  # Position legend
    legend.title = element_text(size = 10),  # Legend title size
    legend.text = element_text(size = 8)  # Legend text size
  )

# Plot yields R1 vs R3
ggplot(max_ODdata, aes(x = maxOD_Rep1, y = maxOD_Rep3, color = order)) + 
  geom_point(size = 3) +  # Plot points
  scale_color_manual(values=color_orders) +  # Use a color scale for order
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add line y = x
  labs(
    title = "Max OD Rep1 vs Max OD Rep3",
    x = "Yield (R1)",
    y = "Yield (R3)",
    color = "Order"  # Legend title for the color scale
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "right",  # Position legend
    legend.title = element_text(size = 10),  # Legend title size
    legend.text = element_text(size = 8)  # Legend text size
  )

# Plot yields R2 vs R3
ggplot(max_ODdata, aes(x = maxOD_Rep2, y = maxOD_Rep3, color = order)) + 
  geom_point(size = 3) +  # Plot points
  scale_color_manual(values=color_orders) +  # Use a color scale for order
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add line y = 
  labs(
    title = "Max OD Rep2 vs Max OD Rep3",
    x = "Yield (R2)",
    y = "Yield (R3)",
    color = "Order"  # Legend title for the color scale
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "right",  # Position legend
    legend.title = element_text(size = 10),  # Legend title size
    legend.text = element_text(size = 8)  # Legend text size
  )

# Calculate the average yield per condition
max_ODdata1 <- max_ODdata %>%
  rowwise() %>%
  mutate(Averageyield = mean(c(maxOD_Rep1, maxOD_Rep2, maxOD_Rep3), na.rm = TRUE))

# Create the boxplot
ggplot(max_ODdata1, aes(x = Condition, y = Averageyield)) +
  geom_boxplot(color = "black") +
  labs(title = "Average Yield per Condition",
       x = "Condition",
       y = "Average Yield") +
  theme_minimal()

# Convert Condition to a factor and specify the level order
max_ODdata1$Condition <- factor(max_ODdata1$Condition, levels = custom_order)

# Calculate R² for Rep1 vs Rep2, Rep2 vs Rep3, and Rep1 vs Rep3
r_squared_rep1_rep2 <- calculate_r_squared_per_condition(max_ODdata1, "maxOD_Rep1", "maxOD_Rep2")
r_squared_rep2_rep3 <- calculate_r_squared_per_condition(max_ODdata1, "maxOD_Rep2", "maxOD_Rep3")
r_squared_rep1_rep3 <- calculate_r_squared_per_condition(max_ODdata1, "maxOD_Rep1", "maxOD_Rep3")
r_squared_rep1_rep2strain <- calculate_r_squared_per_strain(max_ODdata1, "maxOD_Rep1", "maxOD_Rep2")
r_squared_rep2_rep3strain <- calculate_r_squared_per_strain(max_ODdata1, "maxOD_Rep2", "maxOD_Rep3")
r_squared_rep1_rep3strain <- calculate_r_squared_per_strain(max_ODdata1, "maxOD_Rep1", "maxOD_Rep3")

# Combine the results into one data frame
r_squared_combined <- rbind(
  mutate(r_squared_rep1_rep2, Comparison = "Rep1 vs Rep2"),
  mutate(r_squared_rep2_rep3, Comparison = "Rep2 vs Rep3"),
  mutate(r_squared_rep1_rep3, Comparison = "Rep1 vs Rep3")
)
r_squared_combinedstrain <- rbind(
  mutate(r_squared_rep1_rep2strain, Comparison = "Rep1 vs Rep2"),
  mutate(r_squared_rep2_rep3strain, Comparison = "Rep2 vs Rep3"),
  mutate(r_squared_rep1_rep3strain, Comparison = "Rep1 vs Rep3")
)

# Plot the R² values per nitrogen source for each comparison
ggplot(r_squared_combined, aes(x = Condition, y = R_squared, fill = Comparison)) +
  geom_bar(stat = "identity", position = "dodge") +  # Dodge to show bars side by side
  labs(title = "R² Values between Growth Rates",
       x = "Nitrogen Source",
       y = "R²") +
  theme_minimal() +
  ylim(0, 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#414487FF", "#2A788EFF", "#22A884FF"))

# Plot the R² values per nitrogen source for each comparison
ggplot(r_squared_combinedstrain, aes(x = Strain, y = R_squared, fill = Comparison)) +
  geom_bar(stat = "identity", position = "dodge") +  # Dodge to show bars side by side
  labs(title = "R² Values between Growth Rates",
       x = "Strain",
       y = "R²") +
  theme_minimal() +
  ylim(0, 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#414487FF", "#2A788EFF", "#22A884FF"))

# Create the first plot (R² values per nitrogen source)
plot_nitrogen <- ggplot(r_squared_combined, aes(x = Condition, y = R_squared, fill = Comparison)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  labs(title = "R² Values between Yield per Nitrogen Source", 
       x = "Nitrogen source", 
       y = "R²") + 
  theme_minimal() + 
  ylim(0, 1) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = c("#414487FF", "#2A788EFF", "#22A884FF"))

# Create the second plot (R² values per strain)
plot_strain <- ggplot(r_squared_combinedstrain, aes(x = Strain, y = R_squared, fill = Comparison)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  labs(title = "R² Values between Yield per Strain", 
       x = "Strain", 
       y = "R²") + 
  theme_minimal() + 
  ylim(0, 1) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = c("#414487FF", "#2A788EFF", "#22A884FF")) +
  coord_flip()  # Flip the coordinates to make this plot horizontal

# Arrange both plots in one graph
combined_plot <- plot_nitrogen / plot_strain 

# Show the combined plot
print(combined_plot)

# Reshape the data into long format to include all three replicates
maxOD_data_long <- max_ODdata1 %>%
  pivot_longer(cols = c(maxOD_Rep1, maxOD_Rep2, maxOD_Rep3), 
               names_to = "Replicate", 
               values_to = "Yield")

# Define custom colors for each replicate
custom_colors <- c("maxOD_Rep1" = brewer.pal(3, "Set1")[1], 
                   "maxOD_Rep2" = brewer.pal(3, "Set1")[2], 
                   "maxOD_Rep3" = brewer.pal(3, "Set1")[3]) 

# Plot yield distribution between replicates
ggplot(maxOD_data_long, aes(x = Condition, y = Yield, fill = Replicate)) + 
  geom_boxplot(outlier.shape = 21, outlier.size = 0, outlier.stroke = 0.5, alpha = 0.7) +  # Use hollow points for outliers
  labs(title = "Yield Distribution Between Replicates", 
       x = "Nitrogen Source", 
       y = "Yield", 
       fill = "Replicate") + 
  geom_jitter(aes(color = Replicate), width = 0.2, alpha = 0.5) +  # Add jittered points for individual data
  scale_fill_manual(values = viridis3)+
  scale_color_manual(values= viridis3)+
  theme_minimal() +  
  guides(fill = "none") +  # Remove the fill legend (for boxplots)
  theme(    legend.position = c(0.17, 0.82),  # Position legend inside the graph (x, y coordinates)
            legend.background = element_rect(fill = "white", color = "black", size = 0.5),  # Optional: Add a background
            legend.title = element_text(size = 10),  # Adjust legend title size
            legend.text = element_text(size = 8))  # Adjust legend text size)  # Rotate x-axis labels for readability

# Calculate correlation for each nitrogen source across replicates
correlation_results <- max_ODdata1 %>%
  group_by(Condition) %>%
  summarise(correlation_R1_R2 = cor(maxOD_Rep1, maxOD_Rep2),
            correlation_R1_R3 = cor(maxOD_Rep1, maxOD_Rep3),
            correlation_R2_R3 = cor(maxOD_Rep2, maxOD_Rep3))

# Print correlation results
print(correlation_results)

# Example correlation results (replace with your actual results)
correlation_results <- max_ODdata1 %>%
  group_by(Condition) %>%
  summarise(
    R1_R2 = cor(maxOD_Rep1, maxOD_Rep2),
    R1_R3 = cor(maxOD_Rep1, maxOD_Rep3),
    R2_R3 = cor(maxOD_Rep2, maxOD_Rep3)
  )

# Example correlation results (replace with your actual results)
correlation_results_strain <- max_ODdata1 %>%
  group_by(Strain) %>%
  summarise(
    R1_R2 = cor(maxOD_Rep1, maxOD_Rep2),
    R1_R3 = cor(maxOD_Rep1, maxOD_Rep3),
    R2_R3 = cor(maxOD_Rep2, maxOD_Rep3)
  )

# Reshape the data to long format for plotting
correlation_longstrain <- correlation_results_strain %>%
  pivot_longer(
    cols = starts_with("R"),
    names_to = "Comparison",
    values_to = "Correlation"
  )

# Reshape the data to long format for plotting
correlation_long <- correlation_results %>%
  pivot_longer(
    cols = starts_with("R"),
    names_to = "Comparison",
    values_to = "Correlation"
  )

# Create a heatmap
ggplot(correlation_longstrain, aes(x = Strain, y = Comparison, fill = Correlation)) +
  geom_tile() +
  scale_fill_viridis_c()+
  labs(
    title = "Correlation Heatmap Between Yields of Replicates",
    x = "Strain",
    y = "Replicate Comparison",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Create a heatmap
ggplot(correlation_long, aes(x = Condition, y = Comparison, fill = Correlation)) +
  geom_tile() +
  scale_fill_viridis_c(option = 'cividis')+
  labs(
    title = "Correlation Heatmap Between Yields of Replicates",
    x = "Condition",
    y = "Replicate Comparison",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Create the density plot
ggplot(correlation_long, aes(x = Correlation, fill = Comparison)) +
  scale_fill_viridis_d()+
  geom_density(alpha = 0.4) +  # Add density curves with some transparency
  labs(
    title = "Density Plot of Correlation Values for Yields",
    x = "Correlation",
    y = "Density",
    color = "Comparison",
    fill = "Comparison"
  ) +
  theme(legend.position = 'mid')+
  theme_minimal()

# Create the density plot with legend inside the graph
ggplot(correlation_long, aes(x = Correlation, fill = Comparison)) +
  scale_fill_viridis_d() +
  geom_density(alpha = 0.4) +  # Add density curves with some transparency
  labs(
    title = "Density Plot of Correlation Values for Yields",
    x = "Correlation",
    y = "Density",
    color = "Comparison",
    fill = "Comparison"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.14, 0.8),  # Position legend inside the graph (x, y coordinates)
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),  # Optional: Add a background
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 8)  # Adjust legend text size
  )

# Fit linear model
lm_model <- lm(maxOD_Rep3 ~ maxOD_Rep2, data = max_ODdata)

# Extract p-value and R-squared
model_summary <- summary(lm_model)
p_value <- model_summary$coefficients[2, 4]  # p-value for maxOD_Rep1
r_squared <- model_summary$r.squared  # R-squared value

# Create the plot
ggplot(max_ODdata, aes(x = maxOD_Rep2, y = maxOD_Rep3, color = order)) + 
  geom_point(size = 3) +  # Plot points
  scale_color_manual(values=color) +  # Use a custom color scale for order
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add line y = x
  labs(
    title = "Max OD Rep1 vs Max OD Rep2",
    x = "Yield (R2)",
    y = "Yield (R3)",
    color = "Order"  # Legend title for the color scale
  ) +
  annotate("text", 0.1, y = 1.5, 
           label = paste("p-value:", round(p_value, 4), "\nR-squared:", round(r_squared, 4)),
           size = 3, color = "black", hjust = 0) +  # Add p-value and R-squared to plot
  theme_minimal() + 
  ylim(0,1.8)+
  theme(
    legend.position = c(0.9, 0.2),  # Position legend inside the graph (x, y coordinates)
    legend.background = element_rect(fill = "white", color = "black", size = 0.4),  # Optional: Add a background
    legend.title = element_text(size = 8),  # Adjust legend title size
    legend.text = element_text(size = 6)  # Adjust legend text size
  )


