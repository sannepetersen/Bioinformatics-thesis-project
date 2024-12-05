# Plot individual strains

# Obtain 1A01 data
strain_1A01 <- all_derivatives_timepoints[all_derivatives_timepoints$Name == '1A01', ]
timepoints_A_vector <- as.vector(timepoints_A[, 1]) 

# Obtain all individual growth data
strain_1A01_ammonium <- strain_1A01[strain_1A01$Treatment == 'Ammonium + C', ]
strain_1A01_ammonium <- strain_1A01_ammonium %>%mutate(timepoints = timepoints_A)
strain_1A01_urea <- strain_1A01[strain_1A01$Treatment == 'Urea + C', ]
strain_1A01_urea <- strain_1A01_urea %>% mutate(timepoints = timepoints_A_vector)
strain_1A01_proline <- strain_1A01[strain_1A01$Treatment == 'Proline + C', ]
strain_1A01_proline <- strain_1A01_proline %>% mutate(timepoints = timepoints_A_vector)
strain_1A01_non <- strain_1A01[strain_1A01$Treatment == 'no N + C', ]
strain_1A01_non <- strain_1A01_non %>% mutate(timepoints = timepoints_A_vector)
strain_1A01_serine <- strain_1A01[strain_1A01$Treatment == 'Serine + C', ]
strain_1A01_serine <- strain_1A01_serine %>% mutate(timepoints = timepoints_A_vector)
strain_1A01_alanine <- strain_1A01[strain_1A01$Treatment == 'Alanine + C', ]
strain_1A01_alanine <- strain_1A01_alanine %>% mutate(timepoints = timepoints_A_vector)
strain_1A01_nitrite <- strain_1A01[strain_1A01$Treatment == 'Nitrite + C', ]
strain_1A01_nitrite <- strain_1A01_nitrite %>% mutate(timepoints = timepoints_A_vector)
strain_1A01_glutamate <- strain_1A01[strain_1A01$Treatment == 'Glutamate + C', ]
strain_1A01_glutamate <- strain_1A01_glutamate %>% mutate(timepoints = timepoints_A_vector[1:6])
strain_1A01_glutamine <- strain_1A01[strain_1A01$Treatment == 'Glutamine + C', ]
strain_1A01_glutamine <- strain_1A01_glutamine %>% mutate(timepoints = timepoints_A_vector[1:6])
strain_1A01_threonine <- strain_1A01[strain_1A01$Treatment == 'Threonine + C', ]
strain_1A01_threonine <- strain_1A01_threonine %>% mutate(timepoints = timepoints_A_vector)
strain_1A01_asparagine <- strain_1A01[strain_1A01$Treatment == 'Asparagine + C', ]
strain_1A01_asparagine <- strain_1A01_asparagine %>% mutate(timepoints = timepoints_A_vector)

# Combine the individual conditions
data_1A01 <- rbind(strain_1A01_urea, strain_1A01_ammonium, strain_1A01_proline,
                   strain_1A01_non, strain_1A01_serine, strain_1A01_alanine, 
                   strain_1A01_nitrite, strain_1A01_glutamate, strain_1A01_glutamine,
                   strain_1A01_threonine, strain_1A01_asparagine)

# Pick colours
colors3 <- brewer.pal(min(nrow(data_1A01), 12), "Set3") 

# Create the plot
ggplot(data_1A01, aes(x = timepoints, y = Highest_Value, color = Treatment)) +
  geom_point() +                    # Scatter plot
  geom_line(aes(group = interaction(Treatment, Name))) +  
  scale_color_manual(values = colors3) +               
  labs(x = "Time (h)", y = "OD600", color = 'N-sources') +
  ggtitle("Growth curves 1A01") +
  theme_minimal()

# Filter the data for Name '1A01'
combinedE2_R12_aminosugars <- glucosamine_plusC %>% 
  filter(Name == '1A01')

# Create the plot
ggplot(combinedE2_R12_aminosugars, aes(x = timepoints_E, y = Highest_Value)) +
  geom_point() +                    # Scatter plot
  geom_line() +                      # Line plot
  labs(x = "Timepoints", y = "Highest Value") + 
  ggtitle("Plot of Highest Value over Timepoints for Name '1A01'") +
  theme_minimal()                    # Clean theme for aesthetics


# Filter for the first 20 unique names
unique_names <- unique(glucosamine_plusC$Name)[1:20]

# Obtain data for these selected strains
combinedE2_R12_subset <- glucosamine_plusC %>%
  filter(Name %in% unique_names) %>%
  mutate(group = 'Glucosamine + C') %>%
  mutate(timepoints = timepoints_E)
combinedE2_R12_subset <- combinedE2_R12_subset[,-1]

# Filter for other 20 unique strains and obtain data
unique_namesI <- unique(glcnac_plusC$Name)[30:50]
combinedI1_R12_subset <- glcnac_plusC %>%
  filter(Name %in% unique_namesI) %>%
  mutate(group = 'GlcNAc + C') %>%
  mutate(timepoints = timepoints_I)
combinedI1_R12_subset <- combinedI1_R12_subset[,-1]

# Combine datasets
glucosamine_glcnac_growthdata <- rbind(combinedI1_R12_subset, combinedE2_R12_subset)

# Determine colours
colors = c('GlcNAc + C' = '#0D0887FF', 'Glucosamine + C' = '#FCA636FF')

# Create the plot
ggplot(glucosamine_glcnac_growthdata, aes(x = timepoints, y = Highest_Value, color = group)) +
  geom_point() +                    # Scatter plot
  geom_line(aes(group = interaction(group, Name))) +  
  scale_color_manual(values = colors) +               
  labs(x = "Time (h)", y = "OD600", color = 'Amino sugar') +
  ggtitle("Plot of Amino sugars") +
  theme_minimal()

# Filter for the first 20 unique names and obtain data
unique_namesE1 <- unique(aspartate_plusC$Name)[1:20]
combinedE1_R12_subset <- aspartate_plusC %>%
  filter(Name %in% unique_names) %>%
  mutate(group = 'Aspartate + C') %>%
  mutate(timepoints = timepoints_E)
combinedE1_R12_subset <- combinedE1_R12_subset[,-1]

# Filter for other strains and obtain data
unique_namesD2 <- unique(proline_plusC$Name)[20:39]
combinedD2_R12_subset <- proline_plusC %>%
  filter(Name %in% unique_namesD2) %>%
  mutate(group = 'Proline + C') %>%
  mutate(timepoints = timepoints_D)
combinedD2_R12_subset <- combinedD2_R12_subset[,-1]

# Combine information of both conditions
aspartate_proline_growthdata <- rbind(combinedE1_R12_subset, combinedD2_R12_subset)

# Determine colours
colors2 = c('Proline + C' = '#0D0887FF', 'Aspartate + C' = '#FCA636FF')

# Create the plot
ggplot(aspartate_proline_growthdata, aes(x = timepoints, y = Highest_Value, color = group)) +
  geom_point() +                    # Scatter plot
  geom_line(aes(group = interaction(group, Name))) +  
  scale_color_manual(values = colors2) +               
  labs(x = "Time (h)", y = "OD600", color = 'Amino acids') +
  ggtitle("Plot of Amino acids") +
  theme_minimal()
