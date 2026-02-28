# Path --------------------------------------------------------------------
#setwd("D:/OneDrive - CGIAR/RMI WP1/Armonizacion")
setwd("C:/Users/feder.DESKTOP-471V2SD/OneDrive - CGIAR/RMI WP1/Armonizacion")
#setwd("C:/Users/rszap/OneDrive - Universidad EAFIT/CIAT/RFM WP1/Armonizacion/")

library(tidyverse); library(dplyr); library(readxl); library(purrr); library(tidyr); library(stringr); library(fuzzyjoin)

# Calling databases and Functions -----------------------------------------
rm(list=ls())

#To update the dictionary (xlsx to csv)
#dictionary <- read_excel("dictionary.xlsx")
#write.csv(dictionary, "dictionary.csv", row.names = FALSE)

#To update labels treatment
#ttmAssign_labels <- read_excel("ttmAssign_labels.xlsx", sheet = "ttmAssign2")
#write.csv(ttmAssign_labels, "ttmAssign2.csv", row.names = FALSE)

# Load the required functions from the provided R scripts
source("combineFunction.R")
source("graphFunctions.R")
source("regressionFunctions.R")

# Set the file paths for the variable dictionary, data, spatial files, and output directory
var_dict_file <- "dictionary.csv"        # Replace with actual file path
wide_file     <- "wideDF.csv"            # Replace with actual file path
output_dir    <- "Results"               # Replace with the actual output directory
ttm_file      <- "ttmAssign2.csv"
spatial_data_dep <- "Maps/hnd_admbnda_adm1_sinit_20161005.shp"
spatial_data_mun <- "Maps/hnd_admbnda_adm2_sinit_20161005.shp"
spatial_data_com <- "Maps/Caserios_HND.shp"

# Set the file paths for baseline and endline datasets
baseline_path <- "Baseline/baselineAgg.csv"
endline_path  <- "Endline/endlineAgg.csv"


# Combine datasets --------------------------------------------------------
# Call the combine_datasets_long function to combine the datasets
longDF <- combine_datasets(baseline_path, endline_path,"surveyID",var_dict_file,ttm_file, combine_type = "long")
wideDF <- combine_datasets(baseline_path, endline_path,"surveyID",var_dict_file,ttm_file, combine_type = "wide")

# Now that the datasets are combined, save the combined dataset as a CSV (optional)
write.csv(longDF, file = "longDF.csv", row.names = FALSE)
write.csv(wideDF, file = "wideDF.csv", row.names = FALSE)


# Regression models -------------------------------------------------------
wideDF <- read.csv("wideDF.csv")
results <- run_regression_models(var_dict_file, wideDF, "model_summaries.html")


# Graphs ------------------------------------------------------------------
longDF <- read.csv("longDF.csv")
colnames(longDF) <- gsub("\\.", "/", colnames(longDF))
longDF <- longDF %>%
  mutate(farmAspectsImproved = case_when(
    is.na(farmAspectsImproved) ~ 0,
    farmAspectsImproved == "none" ~ 0,
    TRUE ~ 1
  ))
generate_plots(var_dict_file, longDF, output_dir, spatial_data_dep, spatial_data_mun, spatial_data_com)


# Print the completion message
cat("All functions have been executed successfully./n")
