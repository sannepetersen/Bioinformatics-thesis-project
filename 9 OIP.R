# Calculate the average growth rate for sugars (kS) and amino acids (kA) for each Name

# Obtain data without no-N control data
inorganic_selected1 <- inorganic_selected %>%
  filter(Treatment != 'no N control + C')

# Calculate the mean GR for inorganic sources
inorganic_avg <- inorganic_selected1 %>%
  group_by(Name, Description) %>%
  summarise(kI = mean(max_derivative), .groups = 'drop')

# Calculate the mean GR for organic sources
organic_avg <- organic_selected %>%
  group_by(Name, Description) %>%
  summarise(kO = mean(max_derivative), .groups = 'drop')

# Remove rows with NAs or zero values in kI or kO
inorganic_avg <- inorganic_avg %>% filter(!is.na(kI) & kI != 0)
organic_avg <- organic_avg %>% filter(!is.na(kO) & kO != 0)

# Merge data
merged_data_organic_inorganic <- merge(inorganic_avg, organic_avg, by = c("Name", "Description"))

# Recalculate OIP
merged_data_organic_inorganic <- merged_data_organic_inorganic %>%
  mutate(OIP = (kO - kI) / (kI + kO))

# Create the plot
ggplot() + 
  geom_point(data = merged_data_organic_inorganic, aes(x = OIP, y = Description), color = "#08519C", alpha = 0.5) + 
  geom_point(data = oip_summary, aes(x = mean_OIP, y = Description), size = 4, color = "#08519C") + 
  geom_errorbarh(data = oip_summary, aes(xmin = mean_OIP - sd_OIP, xmax = mean_OIP + sd_OIP, y = Description), height = 0.2, color = "#08519C") + 
  labs(
    x = "OIP",
    y = "Order",
    title = "Organic-inorganic Preference",
    color = "Category"
  ) + 
  theme_minimal() + 
  theme(legend.position = "none", aspect.ratio = 2,
        plot.title = element_text(size = 9, hjust = 0.5),  
  )

# Add a column to indicate organic vs inorganic for each dataset
combined_derivatives_inorganic <- inorganic_selected1 %>% 
  mutate(NitrogenSource = "Inorganic") 
combined_derivatives_organic <- organic_selected %>% 
  mutate(NitrogenSource = "Organic")  

# Merge the two datasets by stacking them
merged_data <- bind_rows(combined_derivatives_inorganic, combined_derivatives_organic)

# Prepare the PCA data (distinct by Name and Treatment, select relevant columns, remove NAs)
pca_data <- merged_data %>%
  group_by(Name, NitrogenSource) %>%  
  summarise(
    max_derivative = first(max_derivative),  
    max_OD = first(max_OD), 
    .groups = 'drop'  
  ) %>%
  drop_na()  


# Perform PCA
pca_data1 <- pca_data[,3:4]
pca_result <- prcomp(pca_data1, scale. = TRUE)  
pca_plot_data <- pca_data %>%
  left_join(merged_data %>% select(Name, NitrogenSource) %>% distinct(), by = c("Name", "NitrogenSource"))
pca_plot_data <- data.frame(
  PC1 = pca_result$x[, 1],  # First principal component
  PC2 = pca_result$x[, 2],  # Second principal component
  NitrogenSource = pca_plot_data$NitrogenSource  
)

# Create the plot
ggplot(pca_plot_data, aes(x = PC1, y = PC2, color = NitrogenSource)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("Organic" = "#08519C", "Inorganic" = "#FCA636")) + 
  labs(
    title = "PCA of Organic vs Inorganic N-sources",
    x = "PC1",
    y = "PC2",
    color = "Nitrogen Source"
  ) +
  theme_minimal()

# Plot distribution OIP
ggplot(merged_data_organic_inorganic, aes(x = OIP)) +
  geom_histogram(binwidth = 0.05, fill = "#08519C", color = "black", alpha = 0.5) +
  labs(title = "Distribution of Organic-Inorganic Preference (OIP)",
       x = "OIP",
       y = "Count") +
  theme_minimal()


# Plot the yield as density plots
ggplot(merged_data, aes(x = max_OD, fill = NitrogenSource)) + 
  geom_density(alpha = 0.5) + 
  scale_fill_manual(values = c("Organic" = "#FCA636", "Inorganic" = "#08519C")) + 
  labs(title = "Density plot of yield", 
       x = "Yield", 
       y = "Density", 
       fill = "Nitrogen Source") + 
  theme_minimal() +
  theme(legend.position = "top")

# Plot the GR as density plots for all orders
ggplot(combined_derivatives_inorganic, aes(x = max_derivative, fill = Description)) +
  geom_density(alpha = 0.6) + 
  labs(
    title = "Density plot of GR inorganic n-sources",
    x = "Max GR",
    y = "Density",
    fill = "Order"
  ) +
  scale_fill_manual(values=color) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14)
  )

# Ridgeline plot of GR for all orders
ggplot(combined_derivatives_inorganic, aes(x = max_derivative, y = factor(Description, levels = desired_order), fill = Description)) +
  geom_density_ridges(alpha = 0.7) + 
  labs(
    title = "Ridgeline plot of yield inorganic N-sources",
    x = "GR",
    y = " ",
    fill = "Order"
  ) +
  xlim(-0.5,2)+
  scale_fill_manual(values = color) +
  theme_minimal()+
  theme(legend.position = "none")

# Ridgeline plot of yield for all orders
ggplot(combined_derivatives_organic, aes(x = max_OD, y = factor(Description, levels = desired_order), fill = Description)) +
  geom_density_ridges(alpha = 0.7) +
  labs(
    title = "Ridgeline Plot of Max OD Organic N-sources",
    x = "Max OD",
    y = " ",
    fill = "Order"
  ) +
  xlim(-0.5,2)+
  scale_fill_manual(values = color) +
  theme_minimal()


# Reshape data for heatmap
heatmap_data1 <- merged_data %>%
  group_by(Name, NitrogenSource) %>%
  summarize(mean_derivative = mean(max_derivative, na.rm = TRUE)) %>%
  pivot_wider(names_from = NitrogenSource, values_from = mean_derivative)

# Ensure the `Name` column is set as rownames for the heatmap data
rownames(heatmap_data1) <- heatmap_data1$Name

# Generate heatmap
pheatmap(as.matrix(heatmap_data1[-1]), 
         cluster_rows = TRUE, 
         cluster_cols = FALSE, 
         main = "Mean GR per Strain",
         labels_row = rownames(heatmap_data1), 
         fontsize_row = 4, 
         fontsize_col = 8)  

# Obtain Pseudomonadales stain names
strain_ID_pseudo <- c(
  "3B05", "4A09", "4C11", "4D10", "5G01", "A2R01", "A2R16", "A2R20", "D3M06", "B1R08", 
  "E3M09", "B2M13", "E2M05", "C1R08", "D3M17_2", "C2R13", "D2M19", "F2R10", "F2R15", 
  "G2M07", "I1M15", "F3R11", "I2M14", "I2M16", "4A09_2", "3B05_rep", "I3R07", "C1R06"
)

# Add order a
order_pseudo <- rep("Pseudomonadales", length(strain_ID))  # All entries have the same order

# Add genus name
genus_pseudo <- c(
  "Neptunomonas", "Oceanobacter", "Oceanobacter", "Oceanobacter", "Neptunomonas", 
  "Cobetia", "Neptunomonas", "Marinobacter", "Acinetobacter", "Cobetia", 
  "Gilvimarinus", "Cobetia", "Neptuniibacter", "Oceanobacter", "Saccharophagus", 
  "Cobetia", "Marinobacter", "Amphritea", "Neptuniibacter", "Reinekea", 
  "Neptunomonas", "Marinobacter", "Marinobacter", "Neptunomonas", "Oceanobacter", 
  "Neptunomonas", "Amphritea", "Amphritea"
)

# Combine the data into a DataFrame
df_pseudo <- data.frame(strain_ID_pseudo, order_pseudo, genus_pseudo)
data_OIP_pseudo <- merged_data_organic_inorganic[merged_data_organic_inorganic$Name 
                                                 %in% df_pseudo$strain_ID_pseudo,]
df_pseudo <- df_pseudo[df_pseudo$strain_ID_pseudo %in% data_OIP_pseudo$Name,]

# Merge the sugars and acids data on Name and Description
data_OIP_pseudo <- merge(data_OIP_pseudo, df_pseudo, by.x = 'Name', by.y ="strain_ID_pseudo")

# Calculate mean and standard deviation of OIP by Order
oip_summary_pseudo <- data_OIP_pseudo %>%
  group_by(genus_pseudo) %>%
  summarise(
    mean_OIP = mean(OIP),
    sd_OIP = sd(OIP),
    n = n(),
    .groups = 'drop'
  )

# Order the genus_pseudo factor by mean_OIP
oip_summary_pseudo <- oip_summary_pseudo %>%
  arrange(mean_OIP)  # Sort by mean_OIP ascending

# Update genus_pseudo as a factor with levels sorted by mean_OIP
data_OIP_pseudo$genus_pseudo <- factor(data_OIP_pseudo$genus_pseudo, 
                                       levels = oip_summary_pseudo$genus_pseudo)
oip_summary_pseudo$genus_pseudo <- factor(oip_summary_pseudo$genus_pseudo, 
                                          levels = oip_summary_pseudo$genus_pseudo)

# Create the plot with a gradient for both individual points and mean points
ggplot() +
  geom_point(data = data_OIP_pseudo, aes(x = OIP, y = genus_pseudo, color = OIP), size = 2) +
  geom_point(data = oip_summary_pseudo, aes(x = mean_OIP, y = genus_pseudo, color = mean_OIP), size = 5) +
  scale_color_viridis(option = 'cividis')+
  labs(
    x = "OIP",
    y = "Genus",
    title = "Organic-Inorganic Preference",
    color = "OIP"
  ) +
  theme_minimal() +
  
  # Adjustments to the plot
  theme(
    legend.position = "right",  
    aspect.ratio = 2, 
    plot.title = element_text(size = 9, hjust = 0.5)  
  )


# Obtain strain names of Flavobacteriales
strain_ID_flavo <- c(
  "D3R05", "D3R11", "AS94", "4G03", "5F06", "6B07", "A1M10", "A2M07", "A2R05", 
  "A3M03", "D3M17", "D3R19", "B1M15", "B2M17", "AS56", "B2R14", "E3M18", 
  "B3M12", "B3R18", "C3M11", "C3R15", "C3R19", "AS40", "B2R14_2", "F3M04", 
  "E3R01", "G3R17", "G2R05", "G2R07", "I2M19", "C3R17", "B2M06", "E3R09", 
  "A2M03"
)
order_flavo <- rep("Flavobacteriales", length(strain_ID_flavo))  # All entries have the same order

# Obtain genus information
genus_flavo <- c(
  "Oceanihabitans", "Tenacibaculum_A", "Zobellia", "Tenacibaculum", "Maribacter", 
  "Maribacter", "Cellulophaga", "Zobellia", "Tenacibaculum", "Zobellia", 
  "Arenibacter", "Winogradskyella", "Tamlana_A", "Polaribacter", "Mariniflexile", 
  "Tenacibaculum", "Arenibacter", "Wenyingzhuangia", "Zobellia", "Tenacibaculum", 
  "Oceanihabitans", "Maribacter", "Algibacter", "Tamlana_A", "Zobellia", 
  "Tenacibaculum", "Maribacter", "Cellulophaga", "Cellulophaga", "Tamlana_A", 
  "Zobellia", "Cellulophaga", "Winogradskyella", "Zobellia"
)

# Combine the data into a DataFrame
df_flavo <- data.frame(strain_ID_flavo, order_flavo, genus_flavo)
data_OIP_flavo <- merged_data_organic_inorganic[merged_data_organic_inorganic$Name 
                                                %in% df_flavo$strain_ID_flavo,]
df_flavo <- df_flavo[df_flavo$strain_ID_flavo %in% data_OIP_flavo$Name,]

# Merge the sugars and acids data on Name and Description
data_OIP_flavo <- merge(data_OIP_flavo, df_flavo, by.x = 'Name', by.y ="strain_ID_flavo")

# Calculate mean and standard deviation of OIP by Order
oip_summary_flavo <- data_OIP_flavo %>%
  group_by(genus_flavo) %>%
  summarise(
    mean_OIP = mean(OIP),
    sd_OIP = sd(OIP),
    n = n(),
    .groups = 'drop'
  )

# Order the genus_pseudo factor by mean_OIP
oip_summary_flavo <- oip_summary_flavo %>%
  arrange(mean_OIP)  # Sort by mean_OIP ascending

# Update genus_pseudo as a factor with levels sorted by mean_OIP
data_OIP_flavo$genus_flavo <- factor(data_OIP_flavo$genus_flavo, 
                                     levels = oip_summary_flavo$genus_flavo)
oip_summary_flavo$genus_flavo <- factor(oip_summary_flavo$genus_flavo, 
                                        levels = oip_summary_flavo$genus_flavo)

# Create the plot with a gradient for both individual points and mean points
ggplot() +
  geom_point(data = data_OIP_flavo, aes(x = OIP, y = genus_flavo, color = OIP), size = 2) +
  geom_point(data = oip_summary_flavo, aes(x = mean_OIP, y = genus_flavo, color = mean_OIP), size = 5) +
  scale_color_viridis(option = 'cividis')+
  labs(
    x = "OIP",
    y = "Genus",
    title = "Organic-Inorganic Preference",
    color = "OIP"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    aspect.ratio = 2, 
    plot.title = element_text(size = 9, hjust = 0.5)  
  )

