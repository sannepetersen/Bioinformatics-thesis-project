# Max per capita GR calculation
# Only calculate the growth rates if yield >0.06. Add 0.01 to all values to
# handle 0's for the logarithm. 
calculate_max_GR <- function(data, timepoints_col, value_col, condition_col) {  
  data <- data %>%
    arrange(Name, !!sym(timepoints_col)) %>%
    group_by(Name) %>%
    mutate(
      adjusted_value_col = ifelse(max_OD > 0.06, !!sym(value_col) + 0.01, !!sym(value_col)),
      derivative = ifelse(max_OD > 0.06,
                          (log(adjusted_value_col) - log(lag(adjusted_value_col))) / 
                            (!!sym(timepoints_col) - lag(!!sym(timepoints_col))),
                          0), 
      derivative = ifelse(is.infinite(derivative), NA, derivative),
      max_derivative = max(derivative, na.rm = TRUE),
      Treatment = !!sym(condition_col)
    ) %>%
    ungroup()
  return(data)
}

# Calculate the GR for each treatment
ammonium_plusC <- calculate_max_GR(
  data = final_ammonium,
  timepoints_col = "timepoints_A",
  value_col = "Value_minus_no_N",
  condition_col = "conditionA1"
)

nitrite_plusC <- calculate_max_GR(
  data = final_nitrite,
  timepoints_col = "timepoints_A",
  value_col = "Value_minus_no_N",
  condition_col = "conditionA2"
)

nitrate_plusC <- calculate_max_GR(
  data = final_nitrate,
  timepoints_col = "timepoints_B",
  value_col = "Value_minus_no_N",
  condition_col = "conditionB1"
)

urea_plusC <- calculate_max_GR(
  data = final_urea,
  timepoints_col = "timepoints_F",
  value_col = "Value_minus_no_N",
  condition_col = "conditionF2"
)

non_plusC <- calculate_max_GR(
  data = combinedF1_R12,
  timepoints_col = "timepoints_F",
  value_col = "Highest_Value",
  condition_col = "conditionF1"
)

alanine_plusC <- calculate_max_GR(
  data = final_alanine,
  timepoints_col = "timepoints_B",
  value_col = "Value_minus_no_N",
  condition_col = "conditionB2"
)

glycine_plusC <- calculate_max_GR(
  data = final_glycine,
  timepoints_col = "timepoints_C",
  value_col = "Value_minus_no_N",
  condition_col = "conditionC1"
)

serine_plusC <- calculate_max_GR(
  data = final_serine,
  timepoints_col = "timepoints_C",
  value_col = "Value_minus_no_N",
  condition_col = "conditionC2"
)

glutamate_plusC <- calculate_max_GR(
  data = final_glutamate,
  timepoints_col = "timepoints_D",
  value_col = "Value_minus_no_N",
  condition_col = "conditionD1"
)

proline_plusC <- calculate_max_GR(
  data = final_proline,
  timepoints_col = "timepoints_D",
  value_col = "Value_minus_no_N",
  condition_col = "conditionD2"
)

aspartate_plusC <- calculate_max_GR(
  data = final_aspartate,
  timepoints_col = "timepoints_E",
  value_col = "Value_minus_no_N",
  condition_col = "conditionE1"
)

glucosamine_plusC <- calculate_max_GR(
  data = final_glucosamine,
  timepoints_col = "timepoints_E",
  value_col = "Value_minus_no_N",
  condition_col = "conditionE2"
)

threonine_plusC <- calculate_max_GR(
  data = final_threonine,
  timepoints_col = "timepoints_G",
  value_col = "Value_minus_no_N",
  condition_col = "conditionG1"
)

glutamine_plusC <- calculate_max_GR(
  data = final_glutamine,
  timepoints_col = "timepoints_G",
  value_col = "Value_minus_no_N",
  condition_col = "conditionG2"
)

arginine_plusC <- calculate_max_GR(
  data = final_arginine,
  timepoints_col = "timepoints_H",
  value_col = "Value_minus_no_N",
  condition_col = "conditionH1"
)

asparagine_plusC <- calculate_max_GR(
  data = final_asparagine,
  timepoints_col = "timepoints_H",
  value_col = "Value_minus_no_N",
  condition_col = "conditionH2"
)

glcnac_plusC <- calculate_max_GR(
  data = final_glcnac,
  timepoints_col = "timepoints_I",
  value_col = "Value_minus_no_N",
  condition_col = "conditionI1"
)

alanine_minC <- calculate_max_GR(
  data = final_alanine_minC,
  timepoints_col = "timepoints_I",
  value_col = "Value_minus_no_N",
  condition_col = "conditionI2"
)

glycine_minC <- calculate_max_GR(
  data = final_glycine_minC,
  timepoints_col = "timepoints_J",
  value_col = "Value_minus_no_N",
  condition_col = "conditionJ1"
)

serine_minC <- calculate_max_GR(
  data = final_serine_minC,
  timepoints_col = "timepoints_J",
  value_col = "Value_minus_no_N",
  condition_col = "conditionJ2"
)

glutamate_minC <- calculate_max_GR(
  data = final_glutamate_minC,
  timepoints_col = "timepoints_K",
  value_col = "Value_minus_no_N",
  condition_col = "conditionK1"
)

proline_minC <- calculate_max_GR(
  data = final_proline_minC,
  timepoints_col = "timepoints_K",
  value_col = "Value_minus_no_N",
  condition_col = "conditionK2"
)

aspartate_minC <- calculate_max_GR(
  data = final_aspartate_minC,
  timepoints_col = "timepoints_L",
  value_col = "Value_minus_no_N",
  condition_col = "conditionL1"
)

glucosamine_minC <- calculate_max_GR(
  data = final_glucosamine_minC,
  timepoints_col = "timepoints_L",
  value_col = "Value_minus_no_N",
  condition_col = "conditionL2"
)

threonine_minC <- calculate_max_GR(
  data = final_threonine_minC,
  timepoints_col = "timepoints_M",
  value_col = "Value_minus_no_N",
  condition_col = "conditionM1"
)

glutamine_minC <- calculate_max_GR(
  data = final_glutamine_minC,
  timepoints_col = "timepoints_N",
  value_col = "Value_minus_no_N",
  condition_col = "conditionN1"
)

arginine_minC <- calculate_max_GR(
  data = final_arginine_minC,
  timepoints_col = "timepoints_N",
  value_col = "Value_minus_no_N",
  condition_col = "conditionN2"
)

asparagine_minC <- calculate_max_GR(
  data = final_asparagine_minC,
  timepoints_col = "timepoints_O",
  value_col = "Value_minus_no_N",
  condition_col = "conditionO1"
)

glcnac_minC <- calculate_max_GR(
  data = final_glcnac_minC,
  timepoints_col = "timepoints_O",
  value_col = "Value_minus_no_N",
  condition_col = "conditionO2"
)

alanine_plusN <- calculate_max_GR(
  data = final_alanine_plusN,
  timepoints_col = "timepoints_P",
  value_col = "Value_minus_no_N",
  condition_col = "conditionP1"
)

glycine_plusN <- calculate_max_GR(
  data = final_glycine_plusN,
  timepoints_col = "timepoints_P",
  value_col = "Value_minus_no_N",
  condition_col = "conditionP2"
)

serine_plusN <- calculate_max_GR(
  data = final_serine_plusN,
  timepoints_col = "timepoints_Q",
  value_col = "Value_minus_no_N",
  condition_col = "conditionQ1"
)

glutamate_plusN <- calculate_max_GR(
  data = final_glutamate_plusN,
  timepoints_col = "timepoints_Q",
  value_col = "Value_minus_no_N",
  condition_col = "conditionQ2"
)

proline_plusN <- calculate_max_GR(
  data = final_proline_plusN,
  timepoints_col = "timepoints_R",
  value_col = "Value_minus_no_N",
  condition_col = "conditionR1"
)

aspartate_plusN <- calculate_max_GR(
  data = final_aspartate_plusN,
  timepoints_col = "timepoints_R",
  value_col = "Value_minus_no_N",
  condition_col = "conditionR2"
)

asparagine_plusN <- calculate_max_GR(
  data = final_asparagine_plusN,
  timepoints_col = "timepoints_S",
  value_col = "Value_minus_no_N",
  condition_col = "conditionS1"
)

threonine_plusN <- calculate_max_GR(
  data = final_threonine_plusN,
  timepoints_col = "timepoints_S",
  value_col = "Value_minus_no_N",
  condition_col = "conditionS2"
)

glutamine_plusN <- calculate_max_GR(
  data = final_glutamine_plusN,
  timepoints_col = "timepoints_T",
  value_col = "Value_minus_no_N",
  condition_col = "conditionT1"
)

arginine_plusN <- calculate_max_GR(
  data = final_arginine_plusN,
  timepoints_col = "timepoints_T",
  value_col = "Value_minus_no_N",
  condition_col = "conditionT2"
)

# Combine datasets inorganic nitrogen sources + C
combined_derivatives_inorganic <- bind_rows(ammonium_plusC, urea_plusC, nitrite_plusC, 
                                            nitrate_plusC, non_plusC)

# Combine datasets organic amino acids + C
combined_derivatives_organic <- bind_rows(alanine_plusC, glycine_plusC, 
                                          serine_plusC, glutamate_plusC, 
                                          proline_plusC, aspartate_plusC, 
                                          glucosamine_plusC, threonine_plusC, 
                                          glutamine_plusC, arginine_plusC, 
                                          asparagine_plusC, glcnac_plusC)

# Combine both datasets organic amino acids - C
combined_derivatives_organic_minC <- bind_rows(alanine_minC, glycine_minC, 
                                               serine_minC, glutamate_minC, 
                                               proline_minC, aspartate_minC, 
                                               glucosamine_minC, threonine_minC, 
                                               glutamine_minC, arginine_minC, 
                                               asparagine_minC, glcnac_minC)

# Combine both datasets organic amino acids + N
combined_derivatives_organic_plusN <- bind_rows(alanine_plusN, glycine_plusN,
                                                serine_plusN, glutamate_plusN, 
                                                proline_plusN, aspartate_plusN, 
                                                asparagine_plusN, threonine_plusN, 
                                                glutamine_plusN, arginine_plusN)

# Combine the data nitrogen sources + C
all_data_plusC <- bind_rows(combined_derivatives_inorganic, 
                            combined_derivatives_organic)

# Combine all data
all_data_info <- bind_rows(combined_derivatives_inorganic, 
                           combined_derivatives_organic, 
                           combined_derivatives_organic_minC, 
                           combined_derivatives_organic_plusN)

# Merge all the timepoints into one column
all_data_info <- all_data_info %>%
  rowwise() %>%
  mutate(timepoints = list(na.omit(c_across(starts_with("timepoints"))))) %>%
  ungroup()

# Selecting the columns to keep
all_data_info <- all_data_info[,c(2:15, 38)]

# Safe data as csv file
write.csv(all_data_info, "all_data_info.csv", row.names = FALSE)
