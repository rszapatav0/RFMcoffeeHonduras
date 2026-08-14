#' ------------------------------------------------------------------------
#' Processing the RFM 2023 baseline survey data
#' Author:       Federico Ceballos
#' Creation:     August, 2024
#' Last edition: August, 2026
#' Editor:       Raquel Sofía
#' 
#' This code:    Processes the Baseline database from baselineRaw.xlsx to 
#'               baselineAgg.csv
#'               
#' Input:  - File "baselineRaw.xlsx"
#'         - File "dictionary.xlsx" updated for baseline variables
#'         - File "baselineCorrections.xlsx" with the changes to include in the data
#' Output: - File "baselineAgg.csv" with all baseline variables.
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
# Loop by tab sheets
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

# Keeping surveys that agree to participate
df <- df %>% filter(df$agreesToSurveyParticipation=="Y")


# Turning dummies into 1 and 0
binary_vars          <- dictionary %>% filter(is.na(bl_loop)) %>% filter(!is.na(Binaria) & Binaria == 1) %>% pull(new)
existing_binary_vars <- intersect(binary_vars, names(df))
df <- df %>% mutate(across(
  all_of(existing_binary_vars), ~ case_when(
    . %in% c("Y", "y", "yes", 1) ~ 1,
    . %in% c("N", "n", "no", 0) ~ 0,
    TRUE ~ NA_real_)))


# Adjusting numeric columns and creating dummies for multiple selection variables
df <- convert_numeric_columns(df,dictionary)
df <- convert_to_dummies_multiple(df, dictionary)


# Standardize areas, weights and distances
## Deleting original variable
df <- df %>% select(-c(totalAreaUsedForAgricultureInHa))
## Standardizing
df <- standardize_areas(df, dictionary)
df <- standardize_weights_prod(df, dictionary)
colnames(df)


# Creating new variables
df$yieldKgPerHa       <- ifelse(df$coffeeAreaLastHarvestInHa == 0,
                                NA, df$amountOfCoffeeProducedLastHarvestInKgGreen / df$coffeeAreaLastHarvestInHa)
df$densityPlantsPerHa <- ifelse(df$coffeeAreaLastHarvestInHa == 0,
                                NA, df$totalCoffeePlantsOnFarm / df$coffeeAreaLastHarvestInHa)
## Differentiating times
### Distance to the "gold" number
df$timeBetweenHarvestAndDeliveryDiff <- abs(df$timeBetweenHarvestAndDelivery-0)
df$timeBetweenHarvestAndPulpingDiff  <- abs(df$timeBetweenHarvestAndPulping-0)
df$timeBetweenPulpingAndWashingDiff  <- abs(df$timeBetweenPulpingAndWashing-15)
df$timeFromWashingToDryCoffeeDiff    <- abs(df$timeFromWashingToDryCoffee-30) #mean
### Distance to the "gold" range (no substantial differences inside the range)
df$timeBetweenHarvestAndDeliveryDiffr <- pmax(df$timeBetweenHarvestAndDelivery-6, 0)
df$timeBetweenHarvestAndPulpingDiffr  <- pmax(df$timeBetweenHarvestAndPulping-6, 0)
df$timeBetweenPulpingAndWashingDiffr  <- pmax(14-df$timeBetweenPulpingAndWashing,0) + pmax(df$timeBetweenPulpingAndWashing-16,0)
df$timeFromWashingToDryCoffeeDiffr    <- pmax(6-df$timeFromWashingToDryCoffee,0) + pmax(df$timeFromWashingToDryCoffee-36,0)
## Probability of selling to the intermediary
df$probSellInter <- ifelse(is.na(df$maximumReceivedPriceForCoffeeInKgGreenInter_typ),0,1)
## Percentage of amount sold to the intermediary
df$percSellInter <- df$amountSoldInKgGreenInter_typ / df$amountSoldInKgGreen_typ


# Other changes
## There are some problems with accessedCreditOrLoan for turning numerical
df$accessedCreditOrLoan <- ifelse(
  df$accessedCreditOrLoan == "Y", 1,
  ifelse(df$accessedCreditOrLoan == "N", 0, NA)
)
## Rename geodata
df <- df %>% rename(
  Latitud = '_LATITUDE',
  Longitud = '_LONGITUDE'
)
## Number of pests and diseases
no_pest_condition <- str_detect(
  str_to_lower(df$otherPestsOrDiseases),
  "ningun|ninguna|ninguno|nada|no tuvo|no tiene|no les afecta|no le afect|no lo atac|no la atac|no hubo|no ha tenido|no reconoce|finca nueva|plantación nueva|primer corte")
df$mainPestsOrDiseasesLastYear[no_pest_condition] <- "non"
df$E35C <- as.numeric(replace(df$E35C, no_pest_condition, 0))
## People with no sold coffee
df$numberDifferentBuyersLastHarvest <- replace(
  df$numberDifferentBuyersLastHarvest, df$amountSoldInKgGreen_typ==0, 0)
## sellsToIntermediary only for Darío and Ramiro
df <- df %>% mutate(
  sellsToIntermediary = case_when(
    sellsToIntermediary %in% c("becamo","felix") ~ "ninguno",
    TRUE ~ sellsToIntermediary))
## bonus variables for inter
df <- df %>% mutate(
  receivedQualityBonusInter_typ = if_else(
    is.na(maximumReceivedPriceForCoffeeInKgGreenInter_typ), NA, receivedQualityBonusInter_typ),
  receivedQualityDiscountInter_typ = if_else(
    is.na(maximumReceivedPriceForCoffeeInKgGreenInter_typ), NA, receivedQualityDiscountInter_typ)
)
## Changing missing on usesWhatsAppForMessages:
#' The household i) doesn't have mobile phone or ii) have mobile phone but doesn't have smartphone
df <- df %>% mutate(
  usesWhatsAppForMessages = case_when(
    householdHasMobilePhone == 0 ~ 0,
    hasSmartphone == 0 ~ 0,
    TRUE ~ usesWhatsAppForMessages))


# Save 
write.csv(df, "01_data/processed/baseline/baselineAgg.csv", row.names = FALSE)
#df2 <- df



#' ------------------------------------------------------------------------
# Assessing data quality --------------------------------------------------
#' ------------------------------------------------------------------------

#rm(list=ls())
df      <- read.csv("01_data/processed/baseline/baselineAgg.csv")
source("02_code/baseline/baselinequalityCheckFunctions.R")
results <- checkDataQuality(df)

saveRDS(results, file="03_tables/baseline/baselineQualityCheck.RData")
#results <- readRDS("03_tables/baseline/baselineQualityCheck.RData")
