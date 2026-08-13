#' ------------------------------------------------------------------------
#' Processing the RFM 2024 endline survey data
#' Author:       Federico Ceballos
#' Creation:     August, 2024
#' Last edition: August, 2026
#' Editor:       Raquel Sofía
#' 
#' This code: Processes the Endline database. 
#'            
#' Input:  - File "endlineRaw.xlsx", which is a copy from 
#'         "Encuesta_de_Línea_Intermedia_Piloto_Transformando_Mercados_de_Café_-_all_versions_-_Español_es_-_2024-09-17-17-43-42.xlsx"
#'         - File "dictionary.xlsx" updated for endline variables
#'         - File "endlineCorrections.xlsx" with the changes to include in the data
#' Output: - File "endlineAgg.csv" with all endline variables.
#' ------------------------------------------------------------------------


#' ------------------------------------------------------------------------
# Setting up the data and the dictionary ----------------------------------
#' ------------------------------------------------------------------------

rm(list=ls())

# Loading script with functions
source("02_code/endline/endlineProcessingFunctions.R")

# Defining file paths
data_path       <- "01_data/raw/endline/endlineRaw.xlsx"
changes_path    <- "01_data/processed/endline/endlineCorrections.xlsx"
ttmAssign_path  <- "01_data/raw/implementation/ttmAssign.csv"
dictionary_path <- "02_code/dictionary.xlsx"
output_path     <- "02_code/dictionary.csv"

# Dictionary to csv and adding Tab variable
classify_variables_by_tab(data_path, dictionary_path, output_path)

# Load the dictionary for the endline variables
dictionary <- read.csv(output_path) 
dictionary <- dictionary[which(dictionary$endline == 1),]

# Define the tabs and their corresponding suffixes
tabs <- list(
  "lands"                = "_trr",
  "repeatvar"            = "_var",
  "pnd_repeat"           = "_dop",
  "sales_repeat"         = "_buy",
  "sales_process_repeat" = "_typ",
  "tech_repeat"          = "_tec"
)



#' ------------------------------------------------------------------------
# EndlineAgg: Creating a single database for the endline survey -----------
#' ------------------------------------------------------------------------

## Step 1: Load data ------------------------------------------------------

# Loading databases and dictionaries
## Loop by tab sheets
for (tab in names(tabs)) {
  ## Database
  assign(paste0(tab, "_df"), read_excel(data_path, sheet = tab))
  ## Dictionary
  assign(paste0(tab, "_dict"), dictionary %>% filter(el_loop == tab))
}
## Main sheet
base_df <- read_excel(data_path, sheet = "Encuesta de Línea Intermedia...")

# Making corrections
source("02_code/endline/endlineCorrections.R")


## Step 2: Do aggregate_data function for each Tab and join ---------------
# Creating an object that contains all loops processed
aggregated_data <- map2(tabs, names(tabs), function(suffix, tab) {
  df       <- get(paste0(tab, "_df"))
  tab_dict <- get(paste0(tab, "_dict"))
  aggregated_df <- aggregate_data(df, tab_dict, "_submission__uuid", suffix)
  return(aggregated_df)
})

# Merge all aggregated data frames by _submission__uuid
aggregated_df <- reduce(aggregated_data, full_join, by = "_submission__uuid")
df            <- left_join(base_df, aggregated_df,  by = c("_uuid"="_submission__uuid"))


## Step 3: Organize aggregated data ----------------------------------------

# Keeping surveys that agree to participate
df <- df %>% filter(df$agreesToSurveyParticipation=="Y")
df2 <- df

#Adjusting numeric columns and creating dummies for multiple selection variables
df <- convert_numeric_columns(df)
df <- df %>% 
  mutate(
    E2XC = as.numeric(E2XC),
    E3XC = as.numeric(E3XC),
    E4C  = as.numeric(E4C))


#Matching to baseline (before dummy transformation)
df <- df %>% mutate(
  hasInternetAtHome = case_when(
    hasInternetAtHome == 1  ~ "Y",
    hasInternetAtHome %in% c(2, 3, 4) ~ "N",
    hasInternetAtHome == 78 ~ "NS",
    TRUE ~ NA_character_))


#Turning dummies into 1 and 0
binary_vars          <- dictionary %>% filter(is.na(el_loop)) %>% filter(!is.na(Binaria) & Binaria == 1) %>% pull(new)
existing_binary_vars <- intersect(binary_vars, names(df))
df <- df %>% mutate(across(
  all_of(existing_binary_vars), ~ case_when(
    . %in% c("Y", "y", "yes", 1) ~ 1,
    . %in% c("N", "n", "no", 0) ~ 0,
    TRUE ~ NA_real_)))


# Standardize weights, distances and production
df <- standardize_distances(df, dictionary)
df <- standardize_weights_prod(df, dictionary)
colnames(df)


# Fixing version problems
## Version 2 to Version 3: loop by land.
df <- df %>%
  ### Land size
  mutate(
    totalFarmAreaInHa_trr               = if_else(
      is.na(totalFarmAreaInHa_trr),              E3XC, totalFarmAreaInHa_trr),
    totalAreaUsedForAgricultureInHa_trr = if_else(
      is.na(totalAreaUsedForAgricultureInHa_trr), E4C, totalAreaUsedForAgricultureInHa_trr),
    coffeeAreaLastHarvestInHa_trr       = if_else(
      is.na(coffeeAreaLastHarvestInHa_trr),       E2XC, coffeeAreaLastHarvestInHa_trr)
    ) %>%
  ### Land legal 
  mutate(
    `landLegalStatusForCultivation/own_land`      = if_else(
      is.na(`landLegalStatusForCultivation/own_land`),      `landLegalStatusForCultivation/own_land_trr`, `landLegalStatusForCultivation/own_land`),
    `landLegalStatusForCultivation/rent_in_land`  = if_else(
      is.na(`landLegalStatusForCultivation/rent_in_land`),  `landLegalStatusForCultivation/rent_in_land_trr`,  `landLegalStatusForCultivation/rent_in_land`),
    `landLegalStatusForCultivation/rent_out_land` = if_else(
      is.na(`landLegalStatusForCultivation/rent_out_land`), `landLegalStatusForCultivation/rent_out_land_trr`, `landLegalStatusForCultivation/rent_out_land`),
    `landLegalStatusForCultivation/communal_land` = if_else(
      is.na(`landLegalStatusForCultivation/communal_land`), `landLegalStatusForCultivation/communal_land_trr`, `landLegalStatusForCultivation/communal_land`)
    ) %>%
  select(-c(`landLegalStatusForCultivation/own_land_trr`, `landLegalStatusForCultivation/rent_in_land_trr`,
            `landLegalStatusForCultivation/rent_out_land_trr`, `landLegalStatusForCultivation/communal_land_trr`))


# Creating new variables
## Yield and density
df$yieldKgPerHa       <- ifelse(df$coffeeAreaLastHarvestInHa_trr == 0,
                                NA, df$amountOfCoffeeProducedLastHarvestInKgGreen / df$coffeeAreaLastHarvestInHa_trr)
df$densityPlantsPerHa <- ifelse(df$coffeeAreaLastHarvestInHa_trr == 0,
                                NA, df$totalCoffeePlantsOnFarm / df$coffeeAreaLastHarvestInHa_trr)
## Differentiating times
### Distance to the "gold" number
df$timeBetweenHarvestAndDeliveryDiff <- abs(df$timeBetweenHarvestAndDelivery-0)
df$timeBetweenHarvestAndPulpingDiff  <- abs(df$timeBetweenHarvestAndPulping-0)
df$timeBetweenPulpingAndWashingDiff  <- abs(df$timeBetweenPulpingAndWashing-15) #mode
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


# Organizing location variables
## Reading treatment file
ttmAssign  <- read.csv(ttmAssign_path, stringsAsFactors = FALSE) %>%
  group_by(municipality_name, communityOrHamlet_name) %>% slice(1) %>% 
  ungroup() %>% rename_with(~ paste0(.x, "_ttm"))
## Creating auxiliar variables
df$municipality_name_ttm      <- df$producer_municipality
df$communityOrHamlet_name_ttm <- df$producer_community
df <- df %>% left_join(ttmAssign, by = c("municipality_name_ttm", "communityOrHamlet_name_ttm"))
df <- df %>% mutate(
  department        = coalesce(department, department_ttm),
  municipality      = coalesce(municipality, municipality_ttm),
  communityOrHamlet = coalesce(communityOrHamlet, communityOrHamlet_ttm)
) %>% select(-ends_with("_ttm"))


# Other changes
## Rename geodata
df <- df %>% rename(
  Latitud = '_GPS_latitude',
  Longitud = '_GPS_longitude'
)
## People with no sold coffee
df$numberDifferentBuyersLastHarvest <- replace(
  df$numberDifferentBuyersLastHarvest, df$amountSoldInKgGreen_typ==0, 0)
## sellsToIntermediary only for Darío and Ramiro
df <- df %>% mutate(
  sellsToIntermediary = case_when(
    sellsToIntermediary %in% c("becamo","felix") ~ "ninguno",
    TRUE ~ sellsToIntermediary))
## triedFormalCredit=Y but reasons
df <- df %>% mutate(
  across(starts_with("reasonsDidNotTriedCredit"),~ if_else(triedFormalCredit == "Y", NA, .)))
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
write.csv(df, "01_data/processed/endline/endlineAgg.csv", row.names = FALSE)
#df2 <- df


# Assessing data quality --------------------------------------------------
#rm(list=ls())
df <- read.csv("01_data/processed/endline/endlineAgg.csv")
source("02_code/endline/endlineQualityCheckFunctions.R")
results <- checkDataQuality(df)

saveRDS(results, file="03_tables/endline/endlineQualityCheck.RData")
#results <- readRDS("03_tables/baseline/baselineQualityCheck.RData")
