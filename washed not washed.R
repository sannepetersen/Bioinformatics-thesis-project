# Data analysis washed vs not washed

# Create the data frame with growth rates (GRs)
growth_rates_washed <- data.frame(
  Strain = c("3B05", "3B05", "3B05", "3B05", "3B05",
             "4F10", "4F10", "4F10", "4F10", "4F10",
             "4D01", "4D01", "4D01", "4D01", "4D01",
             "12B01", "12B01", "12B01", "12B01", "12B01",
             "3B05", "3B05", "3B05", "3B05", "3B05",
             "4F10", "4F10", "4F10", "4F10", "4F10",
             "4D01", "4D01", "4D01", "4D01", "4D01",
             "12B01", "12B01", "12B01", "12B01", "12B01"),
  Condition = rep(c("Normal", "Washed"), each = 20, times = 1),
  Condition2 = rep(c('10x', '50x', '250x', '1250x', '1650x'), each=1, times=8),
  GR = c(
    # 3B05 normal
    0.0964, 0.079, 0.0901, 0.0602, 0.0414,
    # 4F10 normal
    0.419, 0.329, 0.244, 0.283, 0.0508,
    # 4D01 normal
    0.155, 0.0675, 0.0894, 0.0484, 0.0507,
    # 12B01 normal
    0.161, 0.194, 0.121, 0.167, 0.0495,
    # 3B05 washed
    0.186, 0.033, 0.0156, 0.0118, 0.022,
    # 4F10 washed
    0.274, 0.0522, 0.0269, 0.0226, 0.0284,
    # 4D01 washed
    0.316, 0.107, 0.0169, 0.00672, 0.00529,
    # 12B01 washed
    0.383, 0.0957, 0.0309, 0.0476, 0.0236
  )
)

# Ensure the 'Condition' and 'GR' are properly formatted
growth_rates_washed$Condition <- factor(growth_rates_washed$Condition, levels = c("Normal", "Washed"))

# Perform a t-test for comparing 'Normal' vs 'Washed' conditions for growth rate (GR)
t_test_result <- t.test(GR ~ Condition, data = growth_rates_washed)

# Make boxplots of the data
ggplot(growth_rates_washed, aes(x = Condition, y = GR, fill = Condition)) + 
  geom_boxplot(alpha = 0.6) + 
  geom_jitter(aes(color = Condition), size = 2, width = 0.2, alpha = 0.7) + 
  scale_fill_manual(values = c("Normal" = 'firebrick', "Washed" = "firebrick")) + 
  scale_color_manual(values = c("Normal" = "red", "Washed" = "red")) + 
  labs(title = "Growth Rate Distribution Normal and Washed Cells",
       x = "Condition",
       y = "Growth Rate (h)") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none')

# Create the data frame with yield
yields_washed <- data.frame(
  Strain = c("3B05", "3B05", "3B05", "3B05", "3B05",
             "4F10", "4F10", "4F10", "4F10", "4F10",
             "4D01", "4D01", "4D01", "4D01", "4D01",
             "12B01", "12B01", "12B01", "12B01", "12B01",
             "3B05", "3B05", "3B05", "3B05", "3B05",
             "4F10", "4F10", "4F10", "4F10", "4F10",
             "4D01", "4D01", "4D01", "4D01", "4D01",
             "12B01", "12B01", "12B01", "12B01", "12B01"),
  Condition = rep(c("Normal", "Washed"), each = 20, times = 1),
  Condition2 = rep(c('10x', '50x', '250x', '1250x', '1650x'), each=1, times=8),
  
  GR = c(
    # 3B05 normal
    0.495, 0.486266, 0.5019023, 0.4446994, 0.2139408,
    # 4F10 normal
    1.0061327, 1.0603225, 1.0467852, 1.5217655, 0.255292, 
    # 4D01 normal
    0.5170312, 0.5779912, 0.5729817, 0.3144817, 0.1514103,
    # 12B01 normal
    0.935259, 0.9439809, 0.9685885, 0.9368649, 0.3652877,
    # 3B05 washed
    0.1817248, 0.1166538, 0.1437657, 0.3670098, 0.5467101, 
    # 4F10 washed
    0.2457906, 0.1892876, 0.293643, 0.9438013, 1.5960731,
    # 4D01 washed
    0.1075571, 0.1150772, 0.1710666, 0.4917702, 0.7425649,
    # 12B01 washed
    0.304085, 0.1968698, 0.2534891, 0.8792957, 1.0339681
  )
)

# Make boxplots of the data
ggplot(yields_washed, aes(x = Condition, y = GR, fill = Condition)) + 
  geom_boxplot(alpha = 0.6) + 
  geom_jitter(aes(color = Condition), size = 2, width = 0.2, alpha = 0.7) + 
  scale_fill_manual(values = c("Normal" = 'firebrick', "Washed" = "firebrick")) + 
  scale_color_manual(values = c("Normal" = "red", "Washed" = "red")) + 
  labs(title = "Yield Distribution Normal and Washed Cells",
       x = "Condition",
       y = "Yields") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none')

# Calculate means and standard deviations
summary_stats <- yields_washed %>%
  group_by(Condition) %>%
  summarize(mean_GR = mean(GR, na.rm = TRUE),
            sd_GR = sd(GR, na.rm = TRUE))

# Make plot of summary
ggplot(summary_stats, aes(x = Condition, y = mean_GR, fill = Condition)) + 
  geom_bar(stat = "identity", alpha = 0.6, color = NA) + 
  geom_errorbar(aes(ymin = mean_GR - sd_GR, ymax = mean_GR + sd_GR), width = 0.2) + 
  scale_fill_manual(values = c("Normal" = "firebrick", "Washed" = "firebrick")) + 
  labs(title = "Mean Yields with Error Bars",
       x = "Condition",
       y = "Mean Yield (± SD)") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Add a Metric column and combine GR and Yields
growth_rates_washed$Metric <- "Growth Rate"
yields_washed$Metric <- "Yield"

# Rename the value column for consistency
colnames(growth_rates_washed)[colnames(growth_rates_washed) == "GR"] <- "Value"
colnames(yields_washed)[colnames(yields_washed) == "GR"] <- "Value"

# Combine the datasets
combined_data <- rbind(growth_rates_washed, yields_washed)

# Plot the combined data with facets
ggplot(combined_data, aes(x = Condition, y = Value, fill = Condition)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(aes(color = Condition), size = 2, width = 0.2, alpha = 0.7) +
  scale_fill_manual(values = c("Normal" = "firebrick", "Washed" = "steelblue")) +
  scale_color_manual(values = c("Normal" = "black", "Washed" = "black")) +
  labs(title = "Growth Rates and Yields for Normal and Washed Cells",
       x = "Condition",
       y = "Value") +
  facet_wrap(~Metric, scales = "free_y") + # Separate facets for GR and Yields
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

# Ensure the 'Condition' and 'GR' are properly formatted
yields_washed$Condition <- factor(yields_washed$Condition, levels = c("Normal", "Washed"))

# Perform a t-test for comparing 'Normal' vs 'Washed' conditions for growth rate (GR)
t_test_result <- t.test(Value ~ Condition, data = yields_washed)

# Print the t-test result
t_test_result

# Reshape the data into wide format
yields_washed_wide <- yields_washed %>%
  pivot_wider(names_from = Condition, values_from = Value)

# Create the plot
ggplot(yields_washed, aes(x = Condition, y = Value, fill = Condition)) +
  geom_jitter(aes(color = Strain), width = 0.2, size = 2, alpha = 0.8) + # Points for individual data
  scale_fill_manual(values = c("Normal" = "lightblue", "Washed" = "pink")) +
  scale_color_viridis_d(option = "turbo") +                             # Distinct colors for strains
  labs(title = "Growth Rate Comparison: Washed vs Normal",
       x = "Condition",
       y = "Growth Rate (GR)",
       fill = "Condition") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 10, face = "bold"),
        plot.title = element_text(hjust = 0.5))

# Step 2: Separate washed and not washed data
washed1 <- yields_washed %>% filter(Condition == "Washed")
not_washed1 <- yields_washed %>% filter(Condition == "Normal")

# Step 3: Merge the datasets to align "washed" and "not washed" by Strain and Condition
comparison1 <- merge(washed1, not_washed1, 
                    by = c("Strain", "Condition2"),
                    suffixes = c("_Washed", "_NotWashed"))

# Step 4: Plot yields: washed vs not washed
ggplot(comparison1, aes(x = Value_Washed, y = Value_NotWashed, colour=Strain)) +
  geom_point(size = 3) +
  scale_color_viridis_d(option = "cividis") + # Distinct colors
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") + # Reference line
  labs(title = "Yield Comparison: Washed vs Not Washed",
       x = "Yield (Washed)",
       y = "Yield (Not Washed)") +
  theme_minimal()

#exp 2
# Create vectors for the data
yields <- c(0.985, 1.12, 1.41, 1.14, 0.696, 1.41, 0.987, 0.916, 1.39, 0.885,
            0.734, 1.39, 0.456, 0.453, 0.471, 0.458, 0.526, 0.53, 0.51, 0.524,
            0.529, 0.509, 0.567, 0.602, 0.425, 0.408, 0.431, 0.431, 0.452, 0.451,
            0.416, 0.39, 0.471, 0.399, 0.456, 0.478, 0.437, 0.401, 0.425, 0.427,
            0.431, 0.42, 0.4, 0.396, 0.459, 0.466, 0.428, 0.428, 0.524, 0.519,
            0.53, 0.535, 0.415, 0.407, 0.229, 0.224, 0.387, 0.38, 0.163, 0.231,
            0.433, 0.267, 0.36, 0.362, 0.14, 0.139, 0.105, 0.104, 0.144, 0.151,
            0.1, 0.144, 0.241, 0.124, 0.167, 0.164, 0, 0, 0, 0.102, 0.17, 0.171,
            0, 0.126)

strains <- c("3B05", "3B05", "4F10", "4F10", "12B01", "12B01",
             "3B05", "3B05", "4F10", "4F10", "12B01", "12B01",
             "3B05", "3B05", "4F10", "4F10", "12B01", "12B01",
             "3B05", "3B05", "4F10", "4F10", "12B01", "12B01",
             "3B05", "3B05", "4F10", "4F10", "12B01", "12B01",
             "3B05", "3B05", "4F10", "4F10", "12B01", "12B01",
             "3B05", "3B05", "4F10", "4F10", "12B01", "12B01",
             "3B05", "3B05", "4F10", "4F10", "12B01", "12B01",
             "3B05", "3B05", "4F10", "4F10", "12B01", "12B01",
             "3B05", "3B05", "4F10", "4F10", "12B01", "12B01",
             "3B05", "3B05", "4F10", "4F10", "12B01", "12B01",
             "3B05", "3B05", "4F10", "4F10", "12B01", "12B01",
             "3B05", "3B05", "4F10", "4F10", "12B01", "12B01",
             "3B05", "3B05", "4F10", "4F10", "12B01", "12B01")

conditions <- c("MB", "MB", "MB", "MB", "MB", "MB",
                "MB", "MB", "MB", "MB", "MB", "MB",
                "10x dilution Glucose + Pyruvate", "10x dilution Glucose + Pyruvate",
                "10x dilution Glucose + Pyruvate", "10x dilution Glucose + Pyruvate",
                "10x dilution Glucose + Pyruvate", "10x dilution Glucose + Pyruvate",
                "10x dilution Glucose + Pyruvate", "10x dilution Glucose + Pyruvate",
                "10x dilution Glucose + Pyruvate", "10x dilution Glucose + Pyruvate",
                "10x dilution Glucose + Pyruvate", "10x dilution Glucose + Pyruvate",
                "50x dilution Glucose + Pyruvate", "50x dilution Glucose + Pyruvate",
                "50x dilution Glucose + Pyruvate", "50x dilution Glucose + Pyruvate",
                "50x dilution Glucose + Pyruvate", "50x dilution Glucose + Pyruvate",
                "50x dilution Glucose + Pyruvate", "50x dilution Glucose + Pyruvate",
                "50x dilution Glucose + Pyruvate", "50x dilution Glucose + Pyruvate",
                "50x dilution Glucose + Pyruvate", "50x dilution Glucose + Pyruvate",
                "250x dilution Glucose + Pyruvate", "250x dilution Glucose + Pyruvate",
                "250x dilution Glucose + Pyruvate", "250x dilution Glucose + Pyruvate",
                "250x dilution Glucose + Pyruvate", "250x dilution Glucose + Pyruvate",
                "250x dilution Glucose + Pyruvate", "250x dilution Glucose + Pyruvate",
                "250x dilution Glucose + Pyruvate", "250x dilution Glucose + Pyruvate",
                "250x dilution Glucose + Pyruvate", "250x dilution Glucose + Pyruvate",
                "10x dilution Glucose + Pyruvate", "10x dilution Glucose + Pyruvate",
                "10x dilution Glucose + Pyruvate", "10x dilution Glucose + Pyruvate",
                "10x dilution Glucose + Pyruvate", "10x dilution Glucose + Pyruvate",
                "10x dilution Glucose + Pyruvate", "10x dilution Glucose + Pyruvate",
                "10x dilution Glucose + Pyruvate", "10x dilution Glucose + Pyruvate",
                "10x dilution Glucose + Pyruvate", "10x dilution Glucose + Pyruvate",
                "50x dilution Glucose + Pyruvate", "50x dilution Glucose + Pyruvate",
                "50x dilution Glucose + Pyruvate", "50x dilution Glucose + Pyruvate",
                "50x dilution Glucose + Pyruvate", "50x dilution Glucose + Pyruvate",
                "50x dilution Glucose + Pyruvate", "50x dilution Glucose + Pyruvate",
                "50x dilution Glucose + Pyruvate", "50x dilution Glucose + Pyruvate",
                "50x dilution Glucose + Pyruvate", "50x dilution Glucose + Pyruvate",
                "250x dilution Glucose + Pyruvate", "250x dilution Glucose + Pyruvate",
                "250x dilution Glucose + Pyruvate", "250x dilution Glucose + Pyruvate",
                "250x dilution Glucose + Pyruvate", "250x dilution Glucose + Pyruvate",
                "250x dilution Glucose + Pyruvate", "250x dilution Glucose + Pyruvate",
                "250x dilution Glucose + Pyruvate", "250x dilution Glucose + Pyruvate",
                "250x dilution Glucose + Pyruvate", "250x dilution Glucose + Pyruvate")

preconditions <- c("normal", "normal", "normal", "normal", "normal", "normal",
                   "washed", "washed", "washed", "washed", "washed", "washed",
                   "normal, 5mM Ammonium", "normal, 5mM Ammonium",
                   "normal, 5mM Ammonium", "normal, 5mM Ammonium",
                   "normal, 5mM Ammonium", "normal, 5mM Ammonium",
                   "washed, 5mM Ammonium", "washed, 5mM Ammonium",
                   "washed, 5mM Ammonium", "washed, 5mM Ammonium",
                   "washed, 5mM Ammonium", "washed, 5mM Ammonium",
                   "normal, 5mM Ammonium", "normal, 5mM Ammonium",
                   "normal, 5mM Ammonium", "normal, 5mM Ammonium",
                   "normal, 5mM Ammonium", "normal, 5mM Ammonium",
                   "washed, 5mM Ammonium", "washed, 5mM Ammonium",
                   "washed, 5mM Ammonium", "washed, 5mM Ammonium",
                   "washed, 5mM Ammonium", "washed, 5mM Ammonium",
                   "normal, 5mM Ammonium", "normal, 5mM Ammonium",
                   "normal, 5mM Ammonium", "normal, 5mM Ammonium",
                   "normal, 5mM Ammonium", "normal, 5mM Ammonium",
                   "washed, 5mM Ammonium", "washed, 5mM Ammonium",
                   "washed, 5mM Ammonium", "washed, 5mM Ammonium",
                   "washed, 5mM Ammonium", "washed, 5mM Ammonium",
                   "normal, 20mM Ammonium", "normal, 20mM Ammonium",
                   "normal, 20mM Ammonium", "normal, 20mM Ammonium",
                   "normal, 20mM Ammonium", "normal, 20mM Ammonium",
                   "washed, 20mM Ammonium", "washed, 20mM Ammonium",
                   "washed, 20mM Ammonium", "washed, 20mM Ammonium",
                   "washed, 20mM Ammonium", "washed, 20mM Ammonium",
                   "normal, 20mM Ammonium", "normal, 20mM Ammonium",
                   "normal, 20mM Ammonium", "normal, 20mM Ammonium",
                   "normal, 20mM Ammonium", "normal, 20mM Ammonium",
                   "washed, 20mM Ammonium", "washed, 20mM Ammonium",
                   "washed, 20mM Ammonium", "washed, 20mM Ammonium",
                   "washed, 20mM Ammonium", "washed, 20mM Ammonium",
                   "normal, 20mM Ammonium", "normal, 20mM Ammonium",
                   "normal, 20mM Ammonium", "normal, 20mM Ammonium",
                   "normal, 20mM Ammonium", "normal, 20mM Ammonium",
                   "washed, 20mM Ammonium", "washed, 20mM Ammonium",
                   "washed, 20mM Ammonium", "washed, 20mM Ammonium",
                   "washed, 20mM Ammonium", "washed, 20mM Ammonium")

# Combine into a data frame
data <- data.frame(Yield = yields, Strain = strains, Condition = conditions, Precondition = preconditions)

# Create a new column to categorize washed vs not washed
data$WashStatus <- ifelse(grepl("washed", data$Precondition), "Washed", "Not Washed")

# Create the plot
ggplot(data, aes(x = WashStatus, y = Yield, fill = WashStatus)) +
  geom_boxplot() +
  labs(title = "Yield Rates: Washed vs Not Washed",
       x = "Condition",
       y = "Yield") +
  theme_minimal() +
  scale_fill_manual(values = c("Washed" = "skyblue", "Not Washed" = "lightgreen"))

# Step 2: Separate washed and not washed data
washed <- data %>% filter(WashStatus == "Washed")
not_washed <- data %>% filter(WashStatus == "Not Washed")

# Step 3: Merge the datasets to align "washed" and "not washed" by Strain and Condition
comparison <- merge(washed, not_washed, 
                    by = c("Strain", "Condition"),
                    suffixes = c("_Washed", "_NotWashed"))

# Step 4: Plot yields: washed vs not washed
ggplot(comparison, aes(x = Yield_NotWashed, y = Yield_Washed, colour=Strain)) +
  geom_point(size = 3) +
  scale_color_viridis_d(option = "cividis") + # Distinct colors
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") + # Reference line
  labs(title = "Yield Comparison: Washed vs Not Washed",
       x = "Yield (Not Washed)",
       y = "Yield (Washed)") +
  theme_minimal()

#exp 1 and 2 together
comparison_select <- comparison[,c(1,3,6)]
colnames(comparison_select) <- c('Strain', 'Value_Washed', 'Value_NotWashed')
comparison1_select <- comparison1[,c(1,4,7)]

final_comparison <- rbind(comparison_select, comparison1_select)

# Step 4: Plot yields: washed vs not washed
ggplot(final_comparison, aes(x = Value_NotWashed, y = Value_Washed, colour=Strain)) +
  geom_point(size = 3) +
  scale_color_viridis_d(option = "cividis") + # Distinct colors
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") + # Reference line
  labs(title = "Yield Comparison: Washed vs Not Washed",
       x = "Yield (Not Washed)",
       y = "Yield (Washed)") +
  theme_minimal()

# Convert final_comparison to a long format
final_comparison_long <- final_comparison %>%
  pivot_longer(
    cols = starts_with("Value"), # Columns to pivot (Yield_Washed and Yield_NotWashed)
    names_to = "Condition",      # Name for the new column with original column names
    values_to = "Yield"          # Name for the new column with values
  )

# Perform a t-test for comparing 'Normal' vs 'Washed' conditions for growth rate (GR)
t_test_result <- t.test(Yield ~ Condition, data = final_comparison_long)

# Print the t-test result
t_test_result
