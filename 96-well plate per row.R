# 96-well plate per row
# Code to load plate reader data from csv file.  

# Install and load packages
install.packages("zoo")
install.packages(c("readr", "tidyr", 'ggplot2', 'reshape2'))
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(vcd)
library(zoo)
library(zoo)


# Fill in
row = 'B'
row_name = '4F10'
start_colnum = 13
end_colnum =24
  
# Change to right information
col2 = '12B01'
col3 = '13B01'
col4 = '3F01'
col5 = '5G01'
col6 = '4D01'
col7 = 'A2R16'
col8 = '4A10'
col9 = 'A3M03'
col10 = '4F10'
col11 = '4H09'
col12 = 'control'

# Change to right information
new_colnames <- c(
  'A1' = 'normal MB',
  'A2' = 'normal 10x',
  'A3' = 'normal 50x',
  'A4' = 'normal 250x',
  'A5' = 'normal 1250x',
  'A6' = 'normal 6250x',
  'A7' = 'washed MB',
  'A8' = 'washed 10x',
  'A9' = 'washed 50x', 
  'A10' = 'washed 250x',
  'A11' = 'washed 1250x',
  'A12' = 'washed 6250x'
)

# Change 'A' to 'B'
new_colnames <- setNames(sub('A', row, names(new_colnames)), new_colnames)

# Change colors to conditions
colors <- c(
  'normal MB' = "#440154FF", 
  'normal 10x' = "#FDE725FF", 
  'normal 50x' = "#2A788EFF", 
  'normal 250x' = "orange", 
  'normal 1250x' = "#B12A90FF", 
  'normal 6250x' = "#7AD151FF", 
  'washed MB' = "#440154FF", 
  'washed 10x' = '#FDE725FF', 
  'washed 50x' = '#2A788EFF', 
  'washed 250x' = 'orange', 
  'washed 1250x' = '#B12A90FF', 
  'washed 6250x' = '#7AD151FF'
)

# Fill in location of excel sheet saved as CSV file
#exp1
data <- '/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/EXP1 aa as C and N source/edited2207carbonexperiment.csv'

#exp3
data <- '/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/EXP3 different strains /edited0208differentstrainsexperiment.csv'

#exp4a
data <- '/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/EXP4A different strains aa and c source 1/edited508differentstrainsaminoacidsplusc.csv'

#exp4b
data <- '/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/EXP4B aa as N/edited0908 aa as N source diff strains B.csv'

#exp5a
data <- '/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/EXP5A aa no C/1308 aa no C.csv'

#exp5b
data <- '/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/EXP5B aa no C/edited2108 aa as n and c source part 2.csv'

#exp7
data <- '/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/EXP7 weekend dialysis/weekenddialysis.csv'

#exp8a
data <- '/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/EXP8 salt/2608saltexperiment1.csv'

#exp8b
data <- '/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/EXP8 salt/exp2/editedsalt2b.csv'

#exp8c
data <- '/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/EXP8 salt/1609saltexperiment3.csv'

#exp10
data <- '/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/missing data/missingdata1910.csv'

#exp11A
data <- '/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/EXP11 reproduction/11reproduction.csv'

#exp11B
data <- '/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/EXP11 reproduction/reproducible2.csv'

#exp11C
data <- '/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/EXP11 reproduction/reproduction3.csv'

#exp12A
data <- '/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/EXP12 max OD/manydilutionsODovertime.csv'

#exp12B
data <- '/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/EXP12 max OD/B/manydilutions710.csv'

#exp14 
data <- '/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/EXP14/water experiment 1809.csv'

#exp12C
data <- '/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology/Data excel/EXP12 max OD/C/manydilutionsC.csv'

# Read the CSV file with semicolon delimiter
data <- read_delim(data, delim = ";")
data <- data[-1,-1] # Delete first row and column

# Creating a numeric vector for carbon time
time <- data$...2 
time <- data$Time 

# Making time vector numeric
time <- gsub(",", ".", time) 
time <- as.numeric(time)

# Creating a numeric df for data
data <- as.data.frame(data[,-1]) # Now remove the time column
data[] <- lapply(data, function(x) {if (is.character(x)) {gsub(",", ".", x, fixed = TRUE) } else {x}})

# Convert columns to numeric using nested loops
for(letter in LETTERS) {
  for(i in 1:12) {
    col_name <- paste0(letter, i)
    if (col_name %in% colnames(data)) {  # Check if the column exists
      data[[col_name]] <- as.numeric(data[[col_name]])}}}

# Convert columns to numeric using nested loops
for(letter in LETTERS) {
  for(i in 1:12) {
    col_name <- paste0(letter,'0', i)
    if (col_name %in% colnames(data)) {  # Check if the column exists
      data[[col_name]] <- as.numeric(data[[col_name]])}}}

#################################
# Plotting the data with sliding window 
plotdata <- data[, start_colnum:end_colnum]
colnames(plotdata) <- names(new_colnames)
plotdata$Time <- time
plotdata_tidy <- melt(plotdata, id.vars = "Time", variable.name = "Condition", value.name = "Value")

# Apply median smoothing
window_size <- 8

# Apply median smoothing to each condition
plotdata_smoothed <- plotdata_tidy %>%
  group_by(Condition) %>%
  mutate(Value_smoothed = rollapply(Value, width = window_size, FUN = median, fill = NA, align = "center"))

# Remove rows with NA values
plotdata_smoothed <- plotdata_smoothed %>%
  filter(!is.na(Value_smoothed))

# Define custom colors for specific conditions
custom_colors <- c("control" = "black", "MB" = "blue")

# Generate 12 discrete colors from the viridis palette
viridis_colors <- viridis(12)


# Map the colors to the conditions
ggplot(plotdata_smoothed, aes(x = Time, y = Value_smoothed, color = Condition)) +
  geom_line() +
  scale_color_manual(values = colors) +  # Assign the 12 colors to the conditions
  labs(
    title = sprintf("Growth curves for %s", row_name),
    x = "Time (h)",
    y = "OD600", 
    color = 'Condition'
  ) +
  theme_minimal()

# Create an empty list to store dataframes with smoothed points
smoothed_points_list <- list()
for (i in start_colnum:end_colnum) {
  data_yield <- data.frame(time = time, OD600 = data[, i])
  loess_model <- loess(OD600 ~ time, data = data_yield, span = 0.15)
  smoothed_values <- predict(loess_model, data_yield$time)
  smoothed_df <- data.frame(time = data_yield$time, smoothed_OD600 = smoothed_values)
  smoothed_df$Strain <- colnames(data)[i]
  smoothed_points_list[[i]] <- smoothed_df
}

# Get correct information
information <- c( "A2" = col2, "A3" = col3,"A4" = col4,  "A5" = col5, "A6" = col6, "A7" = col7, 'A8' = col8, 
                  'A9' = col9, 'A10' = col10, 'A11' = col11, 'A12' = col12)
new_information <- setNames(information, gsub("A", row, names(information)))

# Data processing
data_smooth <- do.call(rbind, smoothed_points_list)
smoothed_points_df <- data_smooth %>% arrange(time)
colnames(smoothed_points_df) <- c("time", "Measurements", "Well")

# Convert new_information to a data frame
well_mapping <- data.frame(
  Well = names(new_information),
  DescriptiveName = new_information,
  stringsAsFactors = FALSE
)

smoothed_points_df <- smoothed_points_df %>%
  left_join(well_mapping, by = "Well")

# Create the plot
plot <- ggplot(data = smoothed_points_df, aes(x = time, y = Measurements)) +
  geom_line() +
  facet_wrap(~ Well, nrow = 5, ncol = 5) +  # Use DescriptiveName for facets
  labs(x = "Time", y = "OD600", title = sprintf("Smoothed Lines for %s", row_name)) +
  theme_minimal()
print(plot)

# Remove leading zero from Well column
smoothed_points_df$Well <- sub("(?<=\\D)0(?=\\d)", "", smoothed_points_df$Well, perl = TRUE)
print(smoothed_points_df)

# Filter to keep every 20th row per Well after sorting by time
smoothed_points_df_every_20th <- smoothed_points_df %>%
  arrange(Well, time) %>%         # Sort data by Well and time
  group_by(Well) %>%              # Group by Well
  filter(row_number() %% 5 == 0) %>%  # Keep only every 20th row (20th, 40th, 60th, etc.)
  ungroup()                      # Ungroup after operation

smoothed_points_df <- smoothed_points_df_every_20th %>%
  filter(Measurements > 0)

#smoothed_points_df <- smoothed_points_df %>%
 # filter(time >10)

wells <- unique(smoothed_points_df$Well)

# Loop through wells and calculate derivatives
# Initialize a list to store derivative data frames
deriv_dfs <- list()

# Loop through wells and calculate derivatives
for (well in wells) {
  
  # Filter data for the current well
  df_well <- smoothed_points_df %>%
    filter(Well == well)
  
  # Ensure all measurements >= 0.01 (replace negative values with 0.01)
  df_well$Measurements[df_well$Measurements < 0] <- 0.01
  
  # Check if the maximum OD exceeds 0.1
  if (max(df_well$Measurements, na.rm = TRUE) > 0.1) {
    
    # Add 0.01 to all Measurements to avoid log(0)
    df_well$Measurements <- df_well$Measurements + 0.01
    
    # Calculate the derivative (log-transformed measurements)
    deriv <- diff(log(df_well$Measurements)) / diff(df_well$time)
    
    # Create a data frame for the derivative
    deriv_df <- data.frame(
      time = df_well$time[-1],  # Exclude the first time point
      Measurements = df_well$Measurements[-1],  # Exclude the first measurement
      GR = deriv,  # Growth rate
      Well = well  # Well identifier
    )
    
    # Store the data frame in the list
    deriv_dfs[[well]] <- deriv_df
  } else {
    # If the max OD is < 0.1, set the max derivative to 0
    deriv_df <- data.frame(
      time = df_well$time,  # Keep time points
      Measurements = df_well$Measurements,  # Keep measurements
      GR = rep(0, length(df_well$time)),  # Set growth rate to 0
      Well = well  # Well identifier
    )
    
    # Store the data frame with zero growth rates
    deriv_dfs[[well]] <- deriv_df
  }
}

# Combine all derivative data frames into a single data frame
deriv_combined <- bind_rows(deriv_dfs)
deriv_combined <- deriv_combined %>%
  filter(Measurements > 0.1)
deriv_combined <- filter(deriv_combined, !is.na(GR))

# Plot all derivatives in one plot 
ggplot(data = deriv_combined, aes(x = time, y = Measurements, color = Well)) +
  geom_line(aes(y = GR)) +
  labs(x = "Time", y = "Measurements", title = sprintf("Derivatives %s", row_name)) +
  theme_minimal() +
  facet_wrap(~ Well, scales = "free_y", nrow = 3, ncol = 4)

# Filter out NA values in Measurements column
deriv_combined <- deriv_combined %>% 
  filter(!is.na(Measurements))

# Remove the lowest timepoint for each Well
deriv_combined <- deriv_combined %>%
  group_by(Well) %>%                 # Group by Well
  filter(time != min(time)) %>%      # Remove the row with the minimum time
  ungroup()   
wells <- unique(deriv_combined$Well)

# Initialize a list to store summary data frames
summary_list <- list()

# Loop through each well
for (well in wells) { 
  # Subset data for the current well
  deriv_combined_well <- subset(deriv_combined, Well == well)
  smoothed_data <- subset(smoothed_points_df, Well == well)
  
  # Calculate max_OD and min_OD from smoothed_points_df
  max_OD <- max(smoothed_data$Measurements, na.rm = TRUE)
  min_OD <- min(smoothed_data$Measurements, na.rm = TRUE)
  
  # Calculate summary statistics using deriv_combined_well and max/min_OD
  summary_well <- deriv_combined_well %>%
    summarize(
      max_GR = max(GR, na.rm = TRUE),  # Maximum growth rate
      max_GR_time = time[which.max(GR)],  # Time of max growth rate
      max_GR_OD = Measurements[which.max(GR)],  # OD at max growth rate
      max_OD = max_OD,  # Use max_OD from smoothed_points_df
      min_OD = min_OD,  # Use min_OD from smoothed_points_df
      lag_time = ifelse(max_GR > 0, ((-(max_GR_OD - max_GR * max_GR_time)) / max_GR), 0),  # Lag time
      doub_time = ifelse(max_GR > 0, (log(2) / max_GR), 0),  # Doubling time
      max_time = time[which.max(Measurements)]  # Time of max OD
    )
  
  # If max_GR is 0, set all other summary values to 0
  if (summary_well$max_GR == 0) {
    summary_well <- summary_well %>%
      mutate(
        max_GR_time = 0,
        max_GR_OD = 0,
        max_OD = 0,
        min_OD = 0,
        lag_time = 0,
        doub_time = 0,
        max_time = 0
      )
  }
  
  # Store the summary for the current well in the list
  summary_list[[well]] <- summary_well
}

# Resulting list `summary_list` contains the summary data for each well
growthdata_summary <- bind_rows(summary_list, .id = "Well")

#plot growth curve with max growth rate tangent line and lag time.
ggplot(data = deriv_combined, aes(x = time, y = log(Measurements))) +
  geom_point(size = 0.1) +
  facet_wrap(~Well, nrow = 3, ncol = 4) +
  geom_abline(data = growthdata_summary, color = "red",
              aes(intercept = log(max_GR_OD) - (max_GR) * max_GR_time,
                  slope = (max_GR))) +
  geom_vline(data = growthdata_summary, 
             aes(xintercept = (-((max_GR_OD) - max_GR * max_GR_time))/ max_GR), 
             color = "blue", linetype = "dashed") +
  labs(x = "Time", y = "OD", title = sprintf("%s Growth Data with Fitted Line", row_name)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(growthdata_summary)


