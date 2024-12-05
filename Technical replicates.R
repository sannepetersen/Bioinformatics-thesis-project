library(forcats)

# Plot curves for 1A01
strain_1A01 <- all_derivatives_timepoints[all_derivatives_timepoints$Name == '1A01', ]
strain_1A01rep <- all_derivatives_timepoints[all_derivatives_timepoints$Name == '1A01_rep', ]

# Check that both data frames have the 'Highest_Value' column
if("Highest_Value" %in% colnames(strain_1A01) & "Highest_Value" %in% colnames(strain_1A01rep)) {
  
  # Extract the `Highest_Value` columns
  x_values <- strain_1A01$Highest_Value
  y_values <- strain_1A01rep$Highest_Value
  
  # Plot the data
  plot(x_values, y_values,
       xlab = "Yield 1A01", 
       ylab = "Yield 1A01_rep",
       main = "1A01 and 1A01_rep before no-N correction",
       pch = 19, col = "blue")
  
  # Optionally add a diagonal line for reference
  abline(0, 1, col = "black", lty = 2)  # Line with slope = 1 and intercept = 0
} else {
  stop("The column 'Highest_Value' does not exist in one or both data frames.")
}

# Calculate the correlation
cor_1A01 <- cor(strain_1A01$Highest_Value, strain_1A01rep$Highest_Value, method = "pearson")
mean_values <- (strain_1A01$Highest_Value + strain_1A01rep$Highest_Value) / 2
diff_values <- strain_1A01$Highest_Value - strain_1A01rep$Highest_Value

# Plot
ggplot() +
  geom_point(aes(x = mean_values, y = diff_values)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Mean of Replicates", y = "Difference between Replicates", title = "Bland-Altman Plot")


# replicates technical obtain data
technicalA2 <- combinedA2_R12[, c(3,9)]
technicalA2$condition <- conditionA2

technicalA1 <- combinedA1_R12[, c(3,9)]
technicalA1$condition <- conditionA1

technicalO2 <- combinedO2_R12[, c(3,9)]
technicalO2$condition <- conditionO2

technicalO1 <- combinedO1_R12[, c(3,9)]
technicalO1$condition <- conditionO1

technicalB2 <- combinedB2_R12[, c(3,9)]
technicalB2$condition <- conditionB2

technicalB1 <- combinedB1_R12[, c(3,9)]
technicalB1$condition <- conditionB1

technicalT2 <- combinedT2_R12[, c(3,9)]
technicalT2$condition <- conditionT2

technicalT1 <- combinedT1_R12[, c(3,9)]
technicalT1$condition <- conditionT1

technicalC2 <- combinedC2_R12[, c(3,9)]
technicalC2$condition <- conditionC2

technicalC1 <- combinedC1_R12[, c(3,9)]
technicalC1$condition <- conditionC1

technicalD1 <- combinedD1_R12[, c(3,9)]
technicalD1$condition <- conditionD1

technicalD2 <- combinedD2_R12[, c(3,9)]
technicalD2$condition <- conditionD2

technicalE1 <- combinedE1_R12[, c(3,9)]
technicalE1$condition <- conditionE1

technicalE2 <- combinedE2_R12[, c(3,9)]
technicalE2$condition <- conditionE2

technicalF1 <- combinedF1_R12[, c(3,9)]
technicalF1$condition <- conditionF1

technicalF2 <- combinedF2_R12[, c(3,9)]
technicalF2$condition <- conditionF2

technicalG1 <- combinedG1_R12[, c(3,9)]
technicalG1$condition <- conditionG1

technicalG2 <- combinedG2_R12[, c(3,9)]
technicalG2$condition <- conditionG2

technicalH1 <- combinedH1_R12[, c(3,9)]
technicalH1$condition <- conditionH1

technicalH2 <- combinedH2_R12[, c(3,9)]
technicalH2$condition <- conditionH2

technicalI1 <- combinedI1_R12[, c(3,9)]
technicalI1$condition <- conditionI1

technicalI2 <- combinedI2_R12[, c(3,9)]
technicalI2$condition <- conditionI2

technicalJ1 <- combinedJ1_R12[, c(3,9)]
technicalJ1$condition <- conditionJ1

technicalJ2 <- combinedJ2_R12[, c(3,9)]
technicalJ2$condition <- conditionJ2

technicalK1 <- combinedK1_R12[, c(3,9)]
technicalK1$condition <- conditionK1

technicalK2 <- combinedK2_R12[, c(3,9)]
technicalK2$condition <- conditionK2

technicalL1 <- combinedL1_R12[, c(3,9)]
technicalL1$condition <- conditionL1

technicalL2 <- combinedL2_R12[, c(3,9)]
technicalL2$condition <- conditionL2

technicalM1 <- combinedM1_R12[, c(3,9)]
technicalM1$condition <- conditionM1

technicalM2 <- combinedM2_R12[, c(3,9)]
technicalM2$condition <- conditionM2

technicalN1 <- combinedN1_R12[, c(3,9)]
technicalN1$condition <- conditionN1

technicalN2 <- combinedN2_R12[, c(3,9)]
technicalN2$condition <- conditionN2

technicalP1 <- combinedP1_R12[, c(3,9)]
technicalP1$condition <- conditionP1

technicalP2 <- combinedP2_R12[, c(3,9)]
technicalP2$condition <- conditionP2

technicalQ1 <- combinedQ1_R12[, c(3,9)]
technicalQ1$condition <- conditionQ1

technicalQ2 <- combinedQ2_R12[, c(3,9)]
technicalQ2$condition <- conditionQ2

technicalR1 <- combinedR1_R12[, c(3,9)]
technicalR1$condition <- conditionR1

technicalR2 <- combinedR2_R12[, c(3,9)]
technicalR2$condition <- conditionR2

technicalS1 <- combinedS1_R12[, c(3,9)]
technicalS1$condition <- conditionS1

technicalS2 <- combinedS2_R12[, c(3,9)]
technicalS2$condition <- conditionS2

# Combine data
technical_replicates <- rbind(technicalO2, technicalO1, technicalA1, technicalA2,
                              technicalT2, technicalT1, technicalB2, technicalB1, 
                              technicalC1, technicalC2, technicalD1, technicalD2, 
                              technicalE1, technicalE2, technicalF1, technicalF2, 
                              technicalG1, technicalG1, technicalH1, technicalH2, 
                              technicalI1, technicalI2, technicalJ2, technicalJ1,
                              technicalK1, technicalK2, technicalL1, technicalL2, 
                              technicalM1, technicalM2, technicalN1, technicalN2, 
                              technicalP1, technicalP2, technicalQ1, technicalQ2, 
                              technicalR1, technicalR2, technicalS1, technicalS2)

# Plot data
ggplot(technical_replicates, aes(x = Value, y = y, color = condition)) +  # Map 'condition' inside aes()
  geom_point(alpha = 0.6) +  # Add points with transparency
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +  # Reference line
  labs(
    title = sprintf("Values between replicates for"),  # Assuming 'condition' is a single string or factor
    x = "R1",
    y = "R2"
  ) +
  theme(legend.position='none')+
  theme_minimal()

# Calculate R^2 for each condition
r2_results <- technical_replicates %>%
  group_by(condition) %>%
  summarise(
    r_squared = summary(lm(y ~ Value, data = cur_data()))$r.squared
  )

# Sort conditions by R-squared values
r2_results <- r2_results %>%
  mutate(condition = fct_reorder(condition, r_squared))

# Plot R^2 values sorted by condition
ggplot(r2_results, aes(x = condition, y = r_squared, fill = r_squared)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "R-squared Values by Condition",
    x = "Condition",
    y = "R-squared"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
