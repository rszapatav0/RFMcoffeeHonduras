#' ------------------------------------------------------------------------
#' Processing the RFM 2024 endline survey data
#' Author:       Federico Ceballos
#' Creation:     August, 2024
#' Last edition: April, 2026
#' This code: Processes the Endline database. 
#'            It joins the variables inside loops by operating them (average, 
#'            mode, ifany...) and charge some corrections for the data.
#'            
#' Input:  - File "endlineRaw.xlsx", which is a copy from 
#'         "Encuesta_de_Línea_Intermedia_Piloto_Transformando_Mercados_de_Café_-_all_versions_-_Español_es_-_2024-09-17-17-43-42"
#'         - File "dictionary.xlsx" updated for endline variables
#'         - File "corrections.xlsx" with the changes to include in the data
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
changes_path    <- "01_data/processed/endline/corrections.xlsx"
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
source("02_code/endline/corrections.R")


## Step 2: Do aggregate_data function for each Tab and join ---------------

# Assessing loop inside the loop
## Aggregate the sales_process_repeat data
agg_sales_process_repeat <- aggregate_data(sales_process_repeat_df, sales_process_repeat_dict, "_submission__uuid", "_typ")
## Merge aggregated sales_process_repeat data into sales_repeat data
sales_repeat_df <- left_join(sales_repeat_df, agg_sales_process_repeat, by = "_submission__uuid")

# Creating an object that contains all loops processed
aggregated_data <- map2(tabs, names(tabs), function(suffix, tab) {
  if (tab == "sales_process_repeat") {
    return(agg_sales_process_repeat)  # Return the already aggregated sales data
  } else {
    df       <- get(paste0(tab, "_df"))
    tab_dict <- get(paste0(tab, "_dict"))
    aggregated_df <- aggregate_data(df, tab_dict, "_submission__uuid", suffix)
    return(aggregated_df)
  }
})

# Merge all aggregated data frames by _submission__uuid
aggregated_df <- reduce(aggregated_data, full_join, by = "_submission__uuid")
df            <- left_join(base_df, aggregated_df,  by = c("_uuid"="_submission__uuid"))


## Step 3: Organize aggregated data ----------------------------------------

#Adjusting numeric columns and creating dummies for multiple selection variables
 df <- convert_numeric_columns(df)
 df <- df %>% 
  mutate(
    E2XC = as.numeric(E2XC),
    E3XC = as.numeric(E3XC),
    E4C  = as.numeric(E4C))
#df <- convert_to_dummies_multiple(df, dictionary)

#Turning dummies into 1 and 0
binary_vars          <- dictionary %>% filter(is.na(el_loop)) %>% filter(!is.na(Binaria) & Binaria == 1) %>% pull(new)
existing_binary_vars <- intersect(binary_vars, names(df))
df <- df %>% mutate(
 across(all_of(existing_binary_vars), ~ ifelse(. %in% c("Y", "y", "yes", 1), 1, 0)))

# Standardize weights and distances
df <- standardize_weights(df, dictionary)
df <- standardize_distances(df, dictionary)
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
df$yieldKgPerHa       <- ifelse(df$coffeeAreaLastHarvestInHa_trr == 0,
  NA, df$amountOfCoffeeProducedLastHarvestInKgGreen / df$coffeeAreaLastHarvestInHa_trr)
df$densityPlantsPerHa <- ifelse(df$coffeeAreaLastHarvestInHa_trr == 0,
  NA, df$totalCoffeePlantsOnFarm / df$coffeeAreaLastHarvestInHa_trr)

# summary(df$yieldKgPerHa)
# summary(df$densityPlantsPerHa)
# sum(df$coffeeAreaLastHarvestInHa_trr == 0, na.rm = TRUE) #2
# sum(is.na(df$coffeeAreaLastHarvestInHa_trr)) #71
# sum(df$amountOfCoffeeProducedLastHarvestInKgGreen == 0, na.rm = TRUE) #0
# sum(is.na(df$amountOfCoffeeProducedLastHarvestInKgGreen)) #80
# sum(df$totalCoffeePlantsOnFarm == 0, na.rm = TRUE) #1
# sum(is.na(df$totalCoffeePlantsOnFarm)) #71

# Rename geodata
df <- df %>% rename(
  Latitud = X_GPS_latitude,
  Longitud = X_GPS_longitude
)

# Save
write.csv(df, "01_data/processed/endline/endlineAgg.csv", row.names = FALSE)


# Assessing data quality --------------------------------------------------
#rm(list=ls())
df <- read.csv("01_data/processed/endline/endlineAgg.csv")
source("02_code/endline/endlineQualityCheckFunctions.R")
results <- checkDataQuality(df)


