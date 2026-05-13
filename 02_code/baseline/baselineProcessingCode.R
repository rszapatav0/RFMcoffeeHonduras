#' ------------------------------------------------------------------------
#' Processing the RFM 2023 baseline survey data
#' Author:       Federico Ceballos
#' Creation:     August, 2024
#' Last edition: May, 2026
#' Editor:       Raquel Sofía
#' 
#' This code:    Processes the Baseline database from baselineRaw.xlsx to 
#'               baselineAgg.csv
#' ------------------------------------------------------------------------


#' ------------------------------------------------------------------------
# Setting up the data and the dictionary ----------------------------------
#' ------------------------------------------------------------------------

rm(list=ls())

# Loading script with functions
source("02_code/baseline/baselineProcessingFunctions.R")

# Defining file paths
data_path       <- "01_data/raw/baseline/baselineRaw.xlsx"
changes_path    <- "01_data/processed/baseline/baselineCorrections.xlsx"
dictionary_path <- "02_code/dictionary.xlsx"
output_path     <- "02_code/dictionary.csv"

# Dictionary to csv and adding Tab variable
classify_variables_by_tab(data_path, dictionary_path, output_path)

# Load the dictionary for the endline variables
dictionary <- read.csv(output_path)
dictionary <- dictionary[which(dictionary$baseline == 1),]

# Define the tabs and their corresponding suffixes
tabs <- list(
  "area_terr"            = "_trr",
  "repeatvar"            = "_var",
  "pnd_repeat"           = "_dop",
  "sales_repeat"         = "_buy",
  "sales_process_repeat" = "_typ",
  "tech_repeat"          = "_tec",
  "hh_pop_repeat"        = "_pop"
)



#' ------------------------------------------------------------------------
# BaselineAgg: Creating a single database for the baseline survey ---------
#' ------------------------------------------------------------------------

## Step 1: Load data ------------------------------------------------------
## Loop by tab sheets
for (tab in names(tabs)) {
  ## Database
  assign(paste0(tab, "_df"), read_excel(data_path, sheet = tab))
  ## Dictionary
  assign(paste0(tab, "_dict"), dictionary %>% filter(bl_loop == tab))
}
## Main sheet
base_df <- read_excel(data_path, sheet = "mod_combined")

# Making corrections
source("02_code/baseline/baselineCorrections.R")


## Step 2: Do aggregate_data function for each Tab and join ---------------
# Creating an object that contains all loops processed
aggregated_data <- map2(tabs, names(tabs), function(suffix, tab) {
    df       <- get(paste0(tab, "_df"))
    tab_dict <- get(paste0(tab, "_dict"))
    aggregated_df <- aggregate_data(df, tab_dict, "ID_ENCUESTA", suffix)
    return(aggregated_df)
})

# Merge all aggregated data frames by _submission__uuid
aggregated_df <- reduce(aggregated_data, full_join, by = "ID_ENCUESTA")
df            <- left_join(base_df, aggregated_df,  by = "ID_ENCUESTA")
df            <- df %>% rename(surveyID = ID_ENCUESTA)


## Step 3: Organize aggregated data ----------------------------------------

#Other changes
##There are some problems with accessedCreditOrLoan for turning numerical
df$accessedCreditOrLoan <- ifelse(
  df$accessedCreditOrLoan == "Y", 1,
  ifelse(df$accessedCreditOrLoan == "N", 0, NA)
)
##To create verified variable
df <- df %>% select(-c(totalAreaUsedForAgricultureInHa))
#Adjusting numeric columns and creating dummies for multiple selection variables
df <- convert_numeric_columns(df,dictionary)
df <- convert_to_dummies_multiple(df, dictionary)
##Rename geodata
df <- df %>% rename(
  Latitud = '_LATITUDE',
  Longitud = '_LONGITUDE'
)

#Turning dummies into 1 and 0
binary_vars          <- dictionary %>% filter(is.na(bl_loop)) %>% filter(!is.na(Binaria) & Binaria == 1) %>% pull(new)
existing_binary_vars <- intersect(binary_vars, names(df))
df <- df %>% mutate(
  across(all_of(existing_binary_vars), ~ ifelse(. %in% c("Y", "y", "yes", 1), 1, 0)))

# Standardize areas, weights and distances
df <- standardize_areas(df, dictionary)
df <- standardize_weights_prod(df, dictionary)
colnames(df)


## Step 4: Organizing variables for the merge with enline ------------------

# Creating new variables
df$yieldKgPerHa       <- ifelse(df$coffeeAreaLastHarvestInHa == 0,
                                NA, df$amountOfCoffeeProducedLastHarvestInKgGreen / df$coffeeAreaLastHarvestInHa)
df$densityPlantsPerHa <- ifelse(df$coffeeAreaLastHarvestInHa == 0,
                                NA, df$totalCoffeePlantsOnFarm / df$coffeeAreaLastHarvestInHa)
#summary(df$yieldKgPerHa)
#summary(df$densityPlantsPerHa)

# Save 
write.csv(df, "01_data/processed/baseline/baselineAgg.csv", row.names = FALSE)


# Assessing data quality --------------------------------------------------
rm(list=ls())
df      <- read.csv("01_data/processed/baseline/baselineAgg.csv")
source("02_code/baseline/baselinequalityCheckFunctions.R")
results <- checkDataQuality(df)

saveRDS(results, file="03_tables/baseline/baselineQualityCheck.RData")
#results <- readRDS("03_tables/baseline/baselineQualityCheck.RData")
