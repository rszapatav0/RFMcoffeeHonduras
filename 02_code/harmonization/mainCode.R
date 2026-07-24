#' ------------------------------------------------------------------------
#' Joining RFM 2023 baseline with RFM 2024 endline, and generating results
#' Author:       Federico Ceballos
#' Creation:     August, 2024
#' Last edition: May, 2026
#' Editor:       Raquel Sofía
#' 
#' This code: - Joins baseline with endline. 
#'            - Calculate regression models
#'            - Create graphs
#' ------------------------------------------------------------------------


#' ------------------------------------------------------------------------
# Setting up the data paths and the dictionary ----------------------------
#' ------------------------------------------------------------------------

# Loading Packages
packageList <- c("dplyr", "readxl", "purrr", "tidyr","stringr", "tidyverse", "fuzzyjoin")
lapply(packageList,require,character.only=TRUE)
rm(list=ls())

#To update the dictionary (xlsx to csv)
dictionary <- read_excel("02_code/dictionary.xlsx")
write.csv(dictionary, "02_code/dictionary.csv", row.names = FALSE)

#To update labels treatment
#ttmAssign_labels <- read_excel("01_data/raw/implementation/ttmAssign_labels.xlsx", sheet = "ttmAssign2")
#write.csv(ttmAssign_labels, "01_data/raw/implementation/ttmAssign.csv", row.names = FALSE)

# Load the required functions from the provided R scripts
source("02_code/harmonization/combineFunction.R")
source("02_code/harmonization/regressionFunctions.R")
source("02_code/harmonization/graphFunctions.R")

# Set the file paths
##Main data
var_dict_file <- "02_code/dictionary.csv"
baseline_path <- "01_data/processed/baseline/baselineAgg.csv"
endline_path  <- "01_data/processed/endline/endlineAgg.csv"
ttm_file      <- "01_data/raw/implementation/ttmAssign.csv"
##Spatial data
spatial_data_dep <- "01_data/maps/hnd_admbnda_adm1_sinit_20161005.shp"
spatial_data_mun <- "01_data/maps/hnd_admbnda_adm2_sinit_20161005.shp"
spatial_data_com <- "01_data/maps/Caserios_HND.shp"
##Results
long_file     <- "01_data/processed/harmonization/longDF.csv"
wide_file     <- "01_data/processed/harmonization/wideDF.csv"
tables_output <- "03_tables/harmonization"
plots_output  <- "04_plots/harmonization"



#' ------------------------------------------------------------------------
# Joining baseline and endline --------------------------------------------
#' ------------------------------------------------------------------------

# Function to combine the datasets
## LongDF
longDF <- combine_datasets(baseline_path, endline_path,"surveyID",var_dict_file,ttm_file, combine_type = "long")
write.csv(longDF, file = long_file, row.names = FALSE)

## WideDF
wideDF <- combine_datasets(baseline_path, endline_path,"surveyID",var_dict_file,ttm_file, combine_type = "wide")
write.csv(wideDF, file = wide_file, row.names = FALSE)



#' ------------------------------------------------------------------------
# Regression models (wideDF) ----------------------------------------------
#' ------------------------------------------------------------------------

wideDF  <- read.csv(wide_file)
colnames(wideDF) <- gsub("\\.", "/", colnames(wideDF))
# results <- run_regression_models(
#   var_dict_file, wideDF, paste0(tables_output, "/model_summaries.html"))


#' ------------------------------------------------------------------------
# Graphs (longDF) ---------------------------------------------------------
#' ------------------------------------------------------------------------

longDF <- read.csv(long_file)
colnames(longDF) <- gsub("\\.", "/", colnames(longDF))
longDF <- longDF %>%
  mutate(farmAspectsImproved = case_when(
    is.na(farmAspectsImproved) ~ 0,
    farmAspectsImproved == "none" ~ 0,
    TRUE ~ 1
  ))
win.graph()
generate_plots(var_dict_file, longDF, plots_output, spatial_data_dep, spatial_data_mun, spatial_data_com)


# Print the completion message
cat("All functions have been executed successfully./n")











## Descriptive statistics -------------------------------------------------
# X_corr <- DF[, 1:number_covariables]
# colnames(X_corr) <- X_names[1:number_covariables]
# 
# summ_stats <- fBasics::basicStats(X_corr)
# summ_stats <- as.data.frame(t(summ_stats))
# summ_stats <- summ_stats %>% 
#   select("nobs", "Mean", "Stdev", "Minimum", "1. Quartile", "Median", "3. Quartile", "Maximum") %>%
#   rename('No. Obs.'='nobs', 'St. Dev.'='Stdev', 'Lower quartile'='1. Quartile', 'Upper quartile'='3. Quartile')
# ### Printing in HTML
# summ_stats_table <- kable(summ_stats, 'html', digits=3)
# kable_styling(summ_stats_table,
#               bootstrap_options=c("striped", "hover", "condensed", "responsive"),
#               full_width=FALSE) %>%
#   save_kable("./05_Tables/CF/1_DescriptiveStatistics.html")

