# General information

# Set working directory 
setwd('/Users/sannepetersen/Documents/Bioinformatics and systems biology/Internship systems biology')

# Order orders
desired_order <- c("Alteromonadales", 'Rhodobacterales', 'Flavobacteriales', 'Pseudomonadales', 'Vibrionales')  

# Specify the descriptions you want to keep
desired_descriptions <- c("Alteromonadales", 'Rhodobacterales', 'Flavobacteriales', 'Pseudomonadales', 'Vibrionales')  

# Define colors for each order
color_orders <- c(
  'Alteromonadales' = brewer.pal(5, "Set1")[1],
  'Flavobacteriales' = brewer.pal(5, "Set1")[2],
  'Pseudomonadales' = brewer.pal(5, "Set1")[3],
  'Rhodobacterales' = brewer.pal(5, "Set1")[4],
  'Vibrionales' = brewer.pal(5, "Set1")[5]
)

# Pick five colors from the Brewer palette
inorganic_colors <- c("Ammonium + C" = brewer.pal(5, "Set3")[1], 
                      "Nitrate + C" = brewer.pal(5, "Set3")[2], 
                      "Nitrite + C" = brewer.pal(5, "Set3")[3], 
                      "Urea + C" = brewer.pal(5, "Set3")[4], 
                      "no N control + C" = brewer.pal(5, "Set3")[5])

# Create a list from the vector
strain_list <- list(
  "12B01", "1A01", "1A01_rep", "3B05", "3C02", "3D04", "4B03", "4B04", "4D01", 
  "4G03", "4G09", "5F06", "5G01", "6B07", "6C01", "6D03", "6E01", "6E02", 
  "6E03", "A1M10", "A1R05", "A1R06", "A1R11", "A2M07", "A2R01", "A2R16", "A2R20", 
  "A3M03", "A3R06", "A3R12", "AS40", "AS88", "AS94", "B1M15", "B1R08", "B1R10", 
  "B1R15", "B2M13", "B2R09", "B2R14_2", "B3M02", "B3M03", "B3M08", "B3R02", "B3R02_2", 
  "C2M01", "C2M04", "C2R07_2", "C2R09", "C2R11", "C2R13", "C2R21", "C3M06", "C3M08", 
  "C3M10", "C3M11", "C3R12", "C3R14", "D2M19", "D2R05", "D3M06", "D3M17", "D3R04", 
  "D3R19", "E2M18", "E2R12", "E3M02", "E3M18", "F1R01", "F1R06", "F2M12", "F2R02", 
  "F2R05", "F2R13", "F3M04", "F3M06", "F3M07", "F3M13", "G1M15", "G2R05", "G2R07", 
  "G2R10", "G2R13", "G3M07", "G3M13", "I1M15", "I1R14", "I2M14", "I2M16", "I2M19", 
  "YB2"
)

# Install packages
install.packages(c("readr", "tidyr", 'ggplot2', 'reshape2', 'RColorBrewer',
                   "pheatmap", "ggrepel", "ggbeeswarm"))

emapper_final1

# Load packages
library(vegan)
library(ggrepel)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(pheatmap)
library(readxl)
library(readr)
library(tidyr)
library(viridis)
library(dplyr)
library(VennDiagram)
library(ggbeeswarm)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(reshape2)
library(vcd)
library(dplyr)
library(pheatmap)

# Set names of conditions
conditionA1 = 'Ammonium + C'  
conditionA2 = 'Nitrite + C'  
conditionB1 = 'Nitrate + C'  
conditionB2 = 'Alanine + C'  
conditionC1 = 'Glycine + C'  
conditionC2 = 'Serine + C'  
conditionD1 = 'Glutamate + C'  
conditionD2 = 'Proline + C'  
conditionE1 = 'Aspartate + C'  
conditionE2 = 'Glucosamine + C'  
conditionF1 = 'no N control + C'  
conditionF2 = 'Urea + C'  
conditionG1 = 'Threonine + C'  
conditionG2 = 'Glutamine + C'  
conditionH1 = 'Arginine + C'  
conditionH2 = 'Asparagine + C'  
conditionI1 = 'GlcNAc + C'  
conditionI2 = 'Alanine - C'  
conditionJ1 = 'Glycine - C'  
conditionJ2 = 'Serine - C'  
conditionK1 = 'Glutamate - C'  
conditionK2 = 'Proline - C'  
conditionL1 = 'Aspartate - C'  
conditionL2 = 'Glucosamine - C'  
conditionM1 = 'Threonine - C'  
conditionM2 = 'MB'  
conditionN1 = 'Glutamine - C'  
conditionN2 = 'Arginine - C'  
conditionO1 = 'Asparagine - C'  
conditionO2 = 'GlcNAc - C'  
conditionP1 = 'Alanine + N'  
conditionP2 = 'Glycine + N'  
conditionQ1 = 'Serine + N'  
conditionQ2 = 'Glutamate + N'  
conditionR1 = 'Proline + N'  
conditionR2 = 'Aspartate + N'  
conditionS1 = 'Asparagine + N'  
conditionS2 = 'Threonine + N'  
conditionT1 = 'Glutamine + N'  
conditionT2 = 'Arginine + N'  

#Order of all strains
order_strains_flavo <- c('B2M01', 'B3M12', 'B2M17', 'D3R11', 'C3M11', '4G03', 'A2R05', 
                         'B2R14', 'E3R09', 'D3R19', 'B1M15', 'I2M19', 'D3R05', 'C3R15',
                         'D3M17', 'E3M18', 'A1M10', 'G2R07', 'G3R17', 'C3R19', '5F06',
                         '6B07', 'F3M04', 'A3M03', 'A2M03', 'B3R18', 'C3R17', 'A2M07',
                         'AS40', 'AS94', 'G2R05')

order_strains_rhodo <- c('C2R09', 'F2R14', 'D2R04', 'I2M02', 'G3M19', 'B3M18',
                         'B2R22', 'D2R18', 'D2R19', 'E3M02', 'C3M06', 'B2M14',
                         'B3M03', 'A3M17', '4E07', 'B2R04', 'A3R06', '4A10',
                         'B2R09', '4F10', 'A2R07', 'G2R13', 'F2R13', 'I1M13',
                         '4H09', '5F01', '5D01', 'B3M08')

order_strains_pseudo <- c('D3MO6', 'E3M09', 'B3M10', 'G2M07', 'D2M19', 'I2M14',
                          'F3R11', 'A2R20', 'B1R08', 'C2R13', 'B2M13', 'A2R01',
                          '4D10', '4C11', 'C1R08', '4A09', 'I1M15', '5G01', '3B05',
                          'A2R16', 'F2R15', 'E2M05', 'F2R10', 'C1R06', 'I3R07',
                          'D3M06', 'I2M16')

order_strains_vibrio <- c('G1M15', 'E2M18', 'C2M01', '1A06', 'YB2', '6D03', 
                          '6E01', 'I3M07', '1A01', '1A01_rep', '4B04', '12B01', '6E02', 
                          '3C02', 'F2R02', 'F2R05', '6E03', 'C3R12', 'G2R10')

order_strains_altero <- c('B2RO3', 'B3M02', 'G1M02', '6D02', 'B1R10', 'B1R15',
                          'F1R01', 'C3R14', 'C2R11', 'B2R08', 'D3R04', 'I3M08',
                          'I2R04', 'D2M02', 'F1R02', 'G1M14', 'G3M07', 'C2M11',
                          'C2M19', 'C2R07', 'C2R07_2', 'C3R04', 'E2R12', 'A1R05', 'A3R12',
                          'C1R02', 'B3R02', 'B3R02_2', 'D2R05', 'C1M14', 'G3M13', 'A3R04',
                          '4B03', 'F2M13', '3D04', 'D3M08', 'F3M13', 'F3R08',
                          'B3R15', 'F3M07', 'B3R10', 'I2R16', '3D05', '3F01',
                          '4D01', '4G09', 'C2R21', 'C3M08', 'C3M10','A1R11', 
                          'I1R14', 'A1R06', 'F1R06', '6C01', 'AS88', 'C2M04',
                          'F2M12', 'F3M06')

# Combine the data of the order of all strains
allstrains_order <- c(order_strains_flavo, 
                      order_strains_rhodo, 
                      order_strains_pseudo, 
                      order_strains_vibrio, 
                      order_strains_altero, 'B2R14_2')

