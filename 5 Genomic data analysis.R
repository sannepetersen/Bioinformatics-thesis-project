# Genomic data analysis

# Get data location
strainnames_location <- 'Data excel/strainNamesToProkkaNames.csv'
order <- 'Data/order.csv'
emapper_final_location <- 'Data excel/allEggnogKOs.csv'

# Load genomic and order data
strainnames <- read_delim(strainnames_location)
order <- read_delim(order, delim = ';')
order <- order[, c(5, 9)]
emapper_final <- read_delim(emapper_final_location)

# Process genomic and order data 
strainnames_with_row <- as.data.frame(t(colnames(strainnames)), stringsAsFactors = FALSE)
colnames(strainnames_with_row) <- colnames(strainnames)
strainnames_with_row <- rbind(strainnames_with_row, strainnames)
colnames(emapper_final) <- sub("ko:", "", colnames(emapper_final))
emapper_final <- emapper_final[,-2]

# Obtain distinct yield and GR data min C
organic_minC_selected <- combined_derivatives_organic_minC %>%
  arrange(Name, Treatment, desc(max_OD), desc(max_derivative)) %>%
  distinct(Name, Treatment, .keep_all = TRUE) %>%
  select(Name, Treatment, max_OD, max_derivative, Description)

# Obtain distinct yield and GR data organic plus C
organic_selected <- combined_derivatives_organic %>%
  arrange(Name, Treatment, desc(max_OD), desc(max_derivative)) %>%
  distinct(Name, Treatment, .keep_all = TRUE) %>%
  select(Name, Treatment, max_OD, max_derivative, Description)

# Obtain distinct yield and GR data inorganic
inorganic_selected <- combined_derivatives_inorganic %>%
  arrange(Name, Treatment, desc(max_OD), desc(max_derivative)) %>%
  distinct(Name, Treatment, .keep_all = TRUE) %>%
  select(Name, Treatment, max_OD, max_derivative, Description)

# Obtain distinct yield and GR data plus N
organic_plusN_selected <- combined_derivatives_organic_plusN %>%
  arrange(Name, Treatment, desc(max_OD), desc(max_derivative)) %>%
  distinct(Name, Treatment, .keep_all = TRUE) %>%
  select(Name, Treatment, max_OD, max_derivative, Description)

# Combine all the data frames into one
combined_t_test_data <- bind_rows(
  organic_minC_selected,
  organic_selected,
  inorganic_selected,
  organic_plusN_selected
)

# Join genomic data with GR and yield data 
combined_t_test_data <- combined_t_test_data %>%
  left_join(emapper_final, by = c("Name" = "strain"))

# Determine growth all data
combined_t_test_data <- combined_t_test_data %>% 
  mutate(
    growth_OD = case_when(
      Treatment == "no N control + C" & max_OD > 0.1 ~ 1, 
      Treatment != "no N control + C" & max_OD > 0.06 ~ 1,  
      TRUE ~ 0                                    
    )
  )

# Obtain used strains in our experiment
used_strains <- unique(distinct_data$Name)

# Summarize gene counts, excluding the first column
genes <- emapper_final %>%
  mutate(total_genes = rowSums(select(., -1)))  # Calculate row sums for all columns except the first
genes <- genes[,c(1,5757)]
genes <- genes[genes$strain %in% used_strains, ]

#Calculate mean yield and GR per strain
mean_max_OD <- distinct_data %>%
  group_by(Name) %>%
  mutate(mean_max_OD = mean(max_OD, na.rm = TRUE), 
         mean_max_gr = mean(max_derivative, na.rm = TRUE))

# Merge mean yield with genes data
genes_data <- merge(mean_max_OD, genes, by.x = "Name", by.y = "strain")

# Create the scatter plot
ggplot(genes_data, aes(x = (total_genes), y = log(mean_max_OD), color = Description)) + 
  geom_point(size = 3) + 
  scale_color_manual(values=color)+
  labs(
    title = "Mean max_OD vs Total Genes ",
    x = "Total genes", 
    y = "log(Mean Yield)",  
    color = 'Order'
  ) + 
  theme_minimal() + 
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "black") +  
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Create the scatter plot
ggplot(genes_data, aes(x = (total_genes), y = log(mean_max_gr), color = Description)) + 
  geom_point(size = 3) + 
  scale_color_manual(values=color)+
  labs(
    title = "Mean yield vs Total Genes ",
    x = "Total genes", 
    y = "log(mean GR)", 
    color = 'Order'
  ) + 
  theme_minimal() +  
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "black") +  
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Calculate mean total_genes per order
genes_data_distinct <- genes_data %>%
  group_by(Description, Name) %>%
  distinct(total_genes)

# Summarize mean total genes
genes_summary <- genes_data_distinct %>%
  group_by(Description) %>%
  summarise(mean_total_genes = mean(total_genes, na.rm = TRUE))

# Create the plot per order
ggplot(genes_data_distinct, aes(x = Description, y = total_genes)) +
  geom_jitter(aes(color = Description), width = 0.2, size = 2, alpha = 0.6) +
  geom_point(data = genes_summary, aes(x = Description, y = mean_total_genes), 
             size = 5, shape = 21, fill = "black", color = "white") +  # Big dot for mean
  scale_color_manual(values=color)+
  labs(
    title = "Total Genes per Order",
    x = "Order (Description)",  # X-axis label
    y = "Total Genes"  # Y-axis label
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.text.y = element_text(size = 10),
    legend.position = "none"  # Remove legend
  )

# Fit a linear model
fit <- lm(log(mean_max_gr) ~ total_genes, data = genes_data)

# Extract R^2 and p-value
r_squared <- summary(fit)$r.squared
p_value <- summary(fit)$coefficients[2, 4]

# Create a label for the plot
stat_label <- paste0("R² = ", round(r_squared, 3), ", p = ", format.pval(p_value, digits = 3))

 # Filter to get the column names that have all ones
  KO_all_present <- emapper_final %>%
    select(-strain) %>%              
    summarise(across(everything(), ~ all(. >0)))  
  columns_with_all_ones <- names(KO_all_present)[KO_all_present == TRUE]
  
  # Obtain a list with all KO's
  ko_columns <- colnames(emapper_final)[-1]  
  ko_matrix <- merge(emapper_final, order, by.x = 'strain', by.y = 'strain ID', all = TRUE)
  ko_matrix <- na.omit(ko_matrix)
  ko_matrix <- ko_matrix[ko_matrix$strain %in% strain_list, ]
  
  # Calculate the number of strains per order
  order_strain_counts <- ko_matrix %>%
    group_by(o) %>%
    summarise(n_strains = n())
  
  # Summing KO values for each order
  order_ko_sum <- ko_matrix %>%
    group_by(o) %>%
    summarise(across(all_of(ko_columns), sum))  
  
  # Calculate the fraction of strains per KO per order
  order_kosum_with_counts <- merge(order_ko_sum, order_strain_counts, by = "o")
  
  # Find the ordered number of similar KO's for all strains
  allstrains_kosum <- apply(ko_matrix[,-c(1, length(ko_matrix))], 2, sum)
  sorted_sum_allstrains <- sort(allstrains_kosum, decreasing = TRUE)
  sorted_sum_allstrains <- data.frame(KO = names(sorted_sum_allstrains), SumValue = sorted_sum_allstrains)
  
  # KOs that all strains have
  ko_matrix_all <- ko_matrix %>% 
    select_if(~ all(. > 0))
  allko <- names(ko_matrix_all)
  
  # KOs that not all strains have
  ko_matrix_unique <- ko_matrix %>%
    select(-one_of(names(ko_matrix_all)))
  
  # KOs that not all strains have
  ko_matrix_sorted_unique <- sorted_sum_allstrains[!rownames(sorted_sum_allstrains) %in% allko, ]
  order_kosum_with_counts <- order_kosum_with_counts %>%
    select(-one_of(allko[-c(1,240)]))
  
  # Show top 100 KOs of all strains
  top_100_KOs <- head(ko_matrix_sorted_unique, 100)
  
  # Plot KO vs fraction for the top 100 KOs
  ggplot(top_100_KOs, aes(x = reorder(KO, -SumValue), y = SumValue)) +
    geom_bar(stat = "identity", fill = "#2A788EFF") +
    theme_minimal() +
    labs(title = "Top 100 most count KOs",
         x = "KO (KEGG Orthology)",
         y = "Number of KO occurence") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7)) 
  
  # Loop through each order and plot the top 50 KOs
  unique_orders <- unique(order_kosum_with_counts$o)
  
  # Select columns that start with "K" or "k", and include column "O"
  order_kosum_with_counts <- order_kosum_with_counts %>%
    select(starts_with("K", ignore.case = TRUE), o, n_strains)
  
  for (order_name in unique_orders) {
    # Subset the data for the current order
    order_subset <- order_kosum_with_counts %>% 
      filter(o == order_name) %>% 
      select(-n_strains)  # Exclude the strain count column
    
    # Reshape the data to long format for plotting (KO and corresponding fraction)
    order_subset_long <- order_subset %>% 
      pivot_longer(cols = starts_with("K"), names_to = "KO", values_to = "Fraction")
    
    # Sort by fraction and take the top 50 KOs
    top_100_KOs <- order_subset_long %>% 
      arrange(desc(Fraction)) %>% 
      head(100)  # Adjusted to top 100 KOs
    
    # Plot the top 50 KOs by fraction for the current order
    plot <- ggplot(top_100_KOs, aes(x = reorder(KO, -Fraction), y = Fraction)) + 
      geom_bar(stat = "identity", fill = "#2A788EFF") + 
      theme_minimal() + 
      labs(title = paste("Top 100 KOs for Order:", order_name), 
           x = "KO (KEGG Orthology)", 
           y = "Fraction of Bacteria") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size=5))  # Rotate x-axis labels for readability
    
    # Print the plot
    print(plot)
  }
  
  # Now plot them in one plot of multiple orders
  first_order_name <- unique(order_kosum_with_counts$o)[1]
  first_order_subset <- order_kosum_with_counts %>%
    filter(o == first_order_name) %>%
    select(-n_strains) 
  first_order_long <- first_order_subset %>%
    pivot_longer(cols = -length(first_order_subset), names_to = "KO", values_to = "Fraction")
  
  # Get the top 30 KOs for the first order
  top_50_KOs <- first_order_long %>%
    arrange(desc(Fraction)) %>%
    head(50) %>%
    pull(KO) 
  
  # Filter the full dataset to include only top 30 KOs
  all_orders_subset <- order_kosum_with_counts %>%
    filter(o %in% unique(order_kosum_with_counts$o)) %>%
    select(-n_strains) %>%
    pivot_longer(cols = starts_with("K"), names_to = "KO", values_to = "Fraction") %>%
    filter(KO %in% top_50_KOs)  # Keep only top 30 KOs
  
  # Plot the data
  ggplot(all_orders_subset, aes(x = reorder(KO, -Fraction), y = Fraction, fill = o)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values=color)+
    theme_minimal() +
    labs(title = "Top 50 most occuring KOs of Alteromonadales",
         x = "KO (KEGG Orthology)",
         y = "KO count",
         fill = "Order") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Rotate x-axis labels for readability
  
  # Now plot them in one plot of multiple orders
  second_order_name <- unique(order_kosum_with_counts$o)[2]
  second_order_subset <- order_kosum_with_counts %>%
    filter(o == second_order_name) %>%
    select(-n_strains) 
  
  second_order_long <- second_order_subset %>%
    pivot_longer(cols = -length(second_order_subset), names_to = "KO", values_to = "Fraction")
  
  # Get the top 30 KOs for the first order
  top_50_KOs_2 <- second_order_long %>%
    arrange(desc(Fraction)) %>%
    head(50) %>%
    pull(KO) 
  
  # Filter the full dataset to include only top 30 KOs
  all_orders_subset_2 <- order_kosum_with_counts %>%
    filter(o %in% unique(order_kosum_with_counts$o)) %>%
    select(-n_strains) %>%
    pivot_longer(cols = starts_with("K"), names_to = "KO", values_to = "Fraction") %>%
    filter(KO %in% top_50_KOs_2)  # Keep only top 30 KOs
  
  # Plot the data
  ggplot(all_orders_subset_2, aes(x = reorder(KO, -Fraction), y = Fraction, fill = o)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values=color)+
    theme_minimal() +
    labs(title = "Top 50 KOs of Flavobacteriales",
         x = "KO (KEGG Orthology)",
         y = "KO count",
         fill = "Order") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Rotate x-axis labels for readability
  
  # Now plot them in one plot of multiple orders
  third_order_name <- unique(order_kosum_with_counts$o)[3]
  third_order_subset <- order_kosum_with_counts %>%
    filter(o == third_order_name) %>%
    select(-n_strains) 
  
  third_order_long <- third_order_subset %>%
    pivot_longer(cols = -length(third_order_subset), names_to = "KO", values_to = "Fraction")
  
  # Get the top 30 KOs for the first order
  top_50_KOs_3 <- third_order_long %>%
    arrange(desc(Fraction)) %>%
    head(50) %>%
    pull(KO) 
  
  # Filter the full dataset to include only top 30 KOs
  all_orders_subset_3 <- order_kosum_with_counts %>%
    filter(o %in% unique(order_kosum_with_counts$o)) %>%
    select(-n_strains) %>%
    pivot_longer(cols = starts_with("K"), names_to = "KO", values_to = "Fraction") %>%
    filter(KO %in% top_50_KOs_3)  # Keep only top 30 KOs
  
  # Plot the data
  ggplot(all_orders_subset_3, aes(x = reorder(KO, -Fraction), y = Fraction, fill = o)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values=color)+
    theme_minimal() +
    labs(title = "Top 30 KOs of Pseudomonadales",
         x = "KO (KEGG Orthology)",
         y = "KO count",
         fill = "Order") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Rotate x-axis labels for readability
  
  # Now plot them in one plot of multiple orders
  fourth_order_name <- unique(order_kosum_with_counts$o)[4]
  fourth_order_subset <- order_kosum_with_counts %>%
    filter(o == fourth_order_name) %>%
    select(-n_strains) 
  
  fourth_order_long <- fourth_order_subset %>%
    pivot_longer(cols = -length(fourth_order_subset), names_to = "KO", values_to = "Fraction")
  
  # Get the top 30 KOs for the first order
  top_50_KOs_4 <- fourth_order_long %>%
    arrange(desc(Fraction)) %>%
    head(50) %>%
    pull(KO) 
  
  # Filter the full dataset to include only top 30 KOs
  all_orders_subset_4 <- order_kosum_with_counts %>%
    filter(o %in% unique(order_kosum_with_counts$o)) %>%
    select(-n_strains) %>%
    pivot_longer(cols = starts_with("K"), names_to = "KO", values_to = "Fraction") %>%
    filter(KO %in% top_50_KOs_4)  # Keep only top 30 KOs
  
  # Plot the data
  ggplot(all_orders_subset_4, aes(x = reorder(KO, -Fraction), y = Fraction, fill = o)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_viridis(discrete = TRUE, option = 'plasma') +  # Apply Viridis color scale
    theme_minimal() +
    labs(title = "Top 30 KOs of Rhodobacterales",
         x = "KO (KEGG Orthology)",
         y = "KO count",
         fill = "Order") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Rotate x-axis labels for readability
  
  # Now plot them in one plot of multiple orders
  fifth_order_name <- unique(order_kosum_with_counts$o)[5]
  fifth_order_subset <- order_kosum_with_counts %>%
    filter(o == fifth_order_name) %>%
    select(-n_strains) 
  
  fifth_order_long <- fifth_order_subset %>%
    pivot_longer(cols = -length(fifth_order_subset), names_to = "KO", values_to = "Fraction")
  
  # Get the top 30 KOs for the first order
  top_50_KOs_5 <- fifth_order_long %>%
    arrange(desc(Fraction)) %>%
    head(50) %>%
    pull(KO) 
  
  # Filter the full dataset to include only top 30 KOs
  all_orders_subset_5 <- order_kosum_with_counts %>%
    filter(o %in% unique(order_kosum_with_counts$o)) %>%
    select(-n_strains) %>%
    pivot_longer(cols = starts_with("K"), names_to = "KO", values_to = "Fraction") %>%
    filter(KO %in% top_50_KOs_5)  # Keep only top 30 KOs
  
  # Plot the data
  ggplot(all_orders_subset_5, aes(x = reorder(KO, -Fraction), y = Fraction, fill = o)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values=color)+
    theme_minimal() +
    labs(title = "Top 30 KOs of Vibrionales",
         x = "KO (KEGG Orthology)",
         y = "KO count",
         fill = "Order") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Rotate x-axis labels for readability
  
  # Identify KOs specific to each order
  specific_kos <- order_kosum_with_counts %>%
    # Use `pivot_longer` to reshape the data, making KO columns into rows
    pivot_longer(cols = -o, names_to = "KO", values_to = "count") %>%
    # Group by KO to calculate occurrences per order
    group_by(KO) %>%
    # Filter for KOs that appear in only one order (i.e., one non-zero count)
    filter(sum(count > 0) == 1) %>%
    # Find the order where the KO is present
    filter(count > 0) %>%
    ungroup()
  
  # Obtain 30 unique KOs alteromonadales
  specific_kos_altero <- specific_kos[specific_kos$o == 'Alteromonadales', ]
  specific_altero_30 <- specific_kos_altero %>%
    arrange(desc(count)) %>%
    head(30) %>%
    pull(KO) 
  
  # Filter the full dataset to include only top 30 KOs
  specific_altero_30_data <- order_kosum_with_counts %>%
    filter(o %in% unique(order_kosum_with_counts$o)) %>%
    select(-n_strains) %>%
    pivot_longer(cols = starts_with("K"), names_to = "KO", values_to = "Count") %>%
    filter(KO %in% specific_altero_30)  # Keep only top 30 KOs
  
  # Extract the first color from the plasma palette
  first_color <- viridis_pal(option = "plasma")(1)
  
  # Plot the data with the custom color
  ggplot(specific_altero_30_data, aes(x = reorder(KO, -Count), y = Count)) + 
    geom_bar(stat = "identity", fill = first_color, position = "dodge") + 
    theme_minimal() +
    labs(title = "Top 30 unique KOs of Alteromonadales",
         x = "KO (KEGG Orthology)",
         y = "KO count",
         fill = "Order") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Rotate x-axis labels
  
  # Generate 5 colors from the plasma palette, one for each order
  colors <- viridis_pal(option = "plasma")(5)
  
  # List to store plots
  plot_list <- list()
  
  # Loop through each unique order, using a different color for each
  for (i in seq_along(unique(specific_kos$o))) {
    order <- unique(specific_kos$o)[i]
    color <- colors[i]
    
    # Filter and get top 30 KOs for the current order
    specific_order_data <- specific_kos %>%
      filter(o == order) %>%
      arrange(desc(count)) %>%
      head(30) %>%
      pull(KO)
    
    specific_order_top_30_data <- order_kosum_with_counts %>%
      filter(o == order) %>%
      select(-n_strains) %>%
      pivot_longer(cols = starts_with("K"), names_to = "KO", values_to = "Count") %>%
      filter(KO %in% specific_order_data)
    
    # Generate plot for the current order with its specific color
    p <- ggplot(specific_order_top_30_data, aes(x = reorder(KO, -Count), y = Count)) + 
      geom_bar(stat = "identity", fill = color, position = "dodge") + 
      theme_minimal() + 
      labs(title = paste("Top 30 unique KOs of", order),
           x = "KO (KEGG Orthology)",
           y = "KO count") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    # Store the plot in the list with the order name
    plot_list[[order]] <- p
  }
  
  # View the plots
  plot_list  # Each plot now has a unique color for each order
  
  # Extract KO lists for each order
  ko_lists_all <- order_kosum_with_counts %>% 
    select(-n_strains) %>% 
    pivot_longer(cols = -c(1, 5519), names_to = "KO", values_to = "Count") %>% 
    group_by(o) %>% 
    summarize(KO_list = list(unique(KO[Count > 0])), .groups = 'drop') %>% 
    pull(KO_list)
  
  # Create names for each set and prepare data
  set_names_all <- unique(order_kosum_with_counts$o)
  venn_data_all <- setNames(ko_lists_all, set_names_all)
  
  # Plot the Venn diagram
  dev.off()
  venn.plot_all <- venn.diagram(
    x = venn_data_all,
    category.names = set_names_all,
    filename = NULL,
    output = TRUE,
    col = "black",
    fill = viridis(length(set_names_all), alpha = 0.5),  # Use Viridis colors
    cex = 1,
    cat.cex = 0.8,
    cat.col = viridis(length(set_names_all)),  # Use Viridis colors for category names
    cat.pos = 0,
    cat.dist = 0.05,
    margin = 0.1
  )
  grid.draw(venn.plot_all)

  # Move order column in front
  ko_matrix_info <- ko_matrix[,c(1,5758)]
  ko_matrix_k <- ko_matrix %>%
    select(starts_with("K", ignore.case = TRUE))
  ko_matrix_k <- cbind(ko_matrix_info, ko_matrix_k)
  
  # Check which KOs in nitrogen_kos exist in ko_matrix_k
  valid_nitrogen_kos <- nitrogen_kos[nitrogen_kos %in% colnames(emapper_final)]
  
  # Assuming ko_matrix_ordered is your data frame
  order_counts <- ko_matrix %>%
    group_by(o) %>%
    summarise(count = n())
  
  # Group and arrange by 'order' (o) before converting counts to binary
  ko_matrix_ordered <- ko_matrix %>%
    arrange(o, strain)
  
  # Convert KO counts to binary presence/absence
  ko_matrix_binary <- ko_matrix %>%
    mutate(across(-c(strain, o), ~ as.integer(. > 0)))
  
  # Prepare data for PCA by selecting only KO columns
  ko_data <- ko_matrix_binary %>%
    select(-strain, -o)
  
  # Remove constant columns (zero variance)
  ko_data <- ko_data[, apply(ko_data, 2, var) != 0]
  
  # Check for zero variance columns
  zero_variance_cols <- names(ko_data)[apply(ko_data, 2, var) == 0]
  if (length(zero_variance_cols) > 0) {
    message("Removing zero variance columns: ", paste(zero_variance_cols, collapse = ", "))
  }
  
  # Select data for PCA 
  distinct_data_test <- distinct_data[,c(1,2,3,6)]
  wide_data <- distinct_data_test %>%
    pivot_wider(names_from = Treatment, values_from = max_OD)
  
  # Scale and center the data
  pca_result <- prcomp(wide_data[,-c(1,2)], scale. = TRUE)
  
  # Get PCA scores (the transformed data)
  pca_scores <- as.data.frame(pca_result$x)
  
  # Add order to the PCA scores for coloring in the plot
  pca_scores$Description <- wide_data$Description
  
  # Create the 3D PCA
  pca_3d <- plot_ly(data = pca_scores, 
                    x = ~PC1, y = ~PC2, z = ~PC3, 
                    color = ~Description, 
                    type = 'scatter3d', 
                    mode = 'markers',
                    marker = list(size = 5),
                    colors = color) %>%
    layout(title = '3D PCA of max_OD Values',
           scene = list(
             xaxis = list(title = 'PC1'),
             yaxis = list(title = 'PC2'),
             zaxis = list(title = 'PC3')
           ))
  
  # Show the 3D plot
  pca_3d
  
  # Perform PCA
  pca_results <- prcomp(ko_data, center = TRUE, scale. = TRUE)
  pca_scores <- as.data.frame(pca_results$x)
  pca_scores <- cbind(ko_matrix_binary$strain, ko_matrix_binary$o, pca_scores)
  colnames(pca_scores)[1:2] <- c("Strain", "Order")
  
  # Assume pca_scores is your data frame containing PCA results
  ggplot(pca_scores, aes(x = PC1, y = PC2, color = Order)) +
    geom_point(size = 1) +
    stat_ellipse(type = "t",  size = 1, aes(fill = Order), alpha = 0.1, geom = "polygon") + 
    labs(title = "PCA of KO Presence/Absence Data",
         x = "PC1",
         y = "PC2") +
    theme_minimal() +
    theme(legend.position = "right") +
    scale_color_manual(values = color)

  # Rename columns
  colnames(pca_scores)[1:5] <- c('Strain', 'Order', "PC1", "PC2", "PC3")
  
  # Create a 3D PCA plot
  pca_3d <- plot_ly(data = pca_scores, 
                    x = ~PC1, y = ~PC2, z = ~PC3, 
                    color = ~Order, 
                    type = 'scatter3d', 
                    mode = 'markers',
                    marker = list(size = 5),
                    colors = color) %>%
    layout(title = '3D PCA of KO Presence/Absence Data',
           scene = list(
             xaxis = list(title = 'PC1'),
             yaxis = list(title = 'PC2'),
             zaxis = list(title = 'PC3')
           ))
  
  # Show the plot
  pca_3d
  
  # Now select only those valid columns
  ko_matrix_nitro <- ko_matrix %>%
    select(strain, o, all_of(valid_nitrogen_kos)) 
  
  # Convert KO counts to binary presence/absence
  ko_matrix_nitro <- ko_matrix_nitro %>%
    mutate(across(-c(strain, o), ~ as.integer(. > 0)))
  
  # Prepare data for PCA by selecting only KO columns
  ko_data_nitro <- ko_matrix_nitro %>%
    select(-strain, -o)
  
  # Remove constant columns (zero variance)
  ko_data_nitro <- ko_data_nitro[, apply(ko_data_nitro, 2, var) != 0]
  
  # Check for zero variance columns
  zero_variance_cols <- names(ko_data_nitro)[apply(ko_data_nitro, 2, var) == 0]
  if (length(zero_variance_cols) > 0) {
    message("Removing zero variance columns: ", paste(zero_variance_cols, collapse = ", "))
  }
  
  # Perform PCA
  pca_results_nitro <- prcomp(ko_data_nitro, center = TRUE, scale. = TRUE)
  pca_scores_nitro <- as.data.frame(pca_results_nitro$x)
  pca_scores_nitro <- cbind(ko_matrix_nitro$strain, ko_matrix_nitro$o, pca_scores_nitro)
  colnames(pca_scores_nitro)[1:2] <- c("Strain", "Order")
  
  pca_scores_nitro_pseudo <- pca_scores_nitro[pca_scores_nitro$Order == 'Pseudomonadales', ]
  
  # Create a 3D PCA plot
  pca_3dnitro <- plot_ly(data = pca_scores_nitro, 
                    x = ~PC1, y = ~PC2, z = ~PC3, 
                    color = ~Order, 
                    type = 'scatter3d', 
                    mode = 'markers',
                    marker = list(size = 5),
                    colors = color) %>%
    layout(title = '3D PCA of KO Presence/Absence Data',
           scene = list(
             xaxis = list(title = 'PC1'),
             yaxis = list(title = 'PC2'),
             zaxis = list(title = 'PC3')
           ))
  
  # Show the plot
  pca_3dnitro
  
  # Assume pca_scores is your data frame containing PCA results
  ggplot(pca_scores_nitro, aes(x = PC1, y = PC2, color = Order)) +
    geom_point(size = 3) +
    labs(title = "PCA of KO Presence/Absence Data Nitrogen KOs",
         x = "PC1",
         y = "PC2") +
    theme_minimal() +
    theme(legend.position = "right") +
    scale_color_manual(values=color)
  
  # PCA plot with confidence ellipses around groups
  ggplot(pca_scores_nitro, aes(x = PC1, y = PC2, color = Order)) +
    geom_point(size = 3) + 
    stat_ellipse(type = "t", linetype = "dashed", size = 1, aes(fill = Order), alpha = 0.1, geom = "polygon") + 
    labs(title = "PCA of KO Presence/Absence Data Nitrogen KOs",
         x = "PC1",
         y = "PC2") + 
    theme_minimal() + 
    theme(legend.position = "right") + 
    scale_color_manual(values = color) +
    scale_fill_manual(values = color) 
  
  loadings <- as.data.frame(pca_results_nitro$rotation)
  loadingsabs <- abs(loadings)
  
  # Set threshold (e.g., top 10% based on absolute loading values)
  threshold <- 0.2
  
  # Identify top KOs based on loadings for each component
  top_loadings <- loadings %>%
    rowwise() %>%
    mutate(
      PC1_abs = abs(PC1),   # Absolute values for easy ranking
      PC2_abs = abs(PC2)
    ) %>%
    filter(PC1_abs >= threshold | 
             PC2_abs >= threshold) %>%
    select(PC1_abs, PC2_abs)  # Keep only selected KOs
  
  selected_kos <- rownames(top_loadings)
  selected_kos <- as.numeric(selected_kos)
  
  # Optional: Subset the original ko_data to only include selected KOs
  ko_data_reduced <- ko_data_nitro[, selected_kos]
  loadings_ko <- colnames(ko_data_reduced)
  loadings_ko_select <- loadings_ko
  
  # Now select only those valid columns
  ko_matrix_loadings <- ko_matrix %>%
    select(strain, o, all_of(loadings_ko_select))  # Use the filtered list of nitrogen KOs
  
  # Convert KO counts to binary presence/absence
  ko_matrix_loadings <- ko_matrix_loadings %>%
    mutate(across(-c(strain, o), ~ as.integer(. > 0)))
  
  # Prepare data for PCA by selecting only KO columns
  ko_data_loadings <- ko_matrix_loadings %>%
    select(-strain, -o)
  
  # Remove constant columns (zero variance)
  ko_data_loadings <- ko_data_loadings[, apply(ko_data_loadings, 2, var) != 0]
  ko_high_loadings <- loadings_ko_select
  
  ko_counts_per_order <- ko_matrix_loadings %>%
    pivot_longer(cols = -c('strain', 'o'), names_to = "KO", values_to = "value") %>%
    filter(!is.na(value) & value > 0) %>%  
    group_by(o, KO) %>% 
    summarise(count = n(), .groups = 'drop') %>%  
    ungroup()
  
  # Plot the counts for each KO per order
  ggplot(ko_counts_per_order, aes(x = KO, y = count, fill = o)) +
    geom_bar(stat = "identity", position = position_dodge()) + 
    theme_minimal() +
    labs(title = "Count of KOs with high loadings", 
         x = "KO", 
         y = "Count of KOs", 
         fill = 'Order') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=7)) + 
    scale_fill_manual(values=color)
  
  # Calculate the number of total strains
  ko_counts <- ko_matrix_loadings %>%
    group_by(o) %>%
    summarise(total_strains = n_distinct(strain), .groups = 'drop')
  
  # Reshape and calculate counts per KO and order
  ko_percentage <- ko_matrix_loadings %>%
    pivot_longer(cols = -c(strain, o), names_to = "KO", values_to = "count") %>%
    filter(count > 0) %>%
    group_by(o, KO) %>%
    summarise(strain_count = n_distinct(strain), .groups = 'drop') %>%
    left_join(ko_counts, by = "o") %>%
    mutate(percentage = (strain_count / total_strains)) %>%
    select(o, KO, percentage)
  
  # View the resulting data frame
  print(ko_percentage)
  
  # Create a bar plot to visualize the percentage of strains containing each KO per order
  ggplot(ko_percentage, aes(x = KO, y = percentage, fill = o)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Percentage of Strains Containing Each KO by Order",
         x = "KEGG Orthology (KO)",
         y = "Percentage of Strains (%)", 
         fill = 'Order') +
    theme(axis.text.x = element_text(angle = 100, hjust = 1), size=2) +  # Rotate x-axis labels for better readability
    scale_fill_manual(values=color_orders)+
    theme_minimal()  # Optional: use a minimal theme for aesthetics
    