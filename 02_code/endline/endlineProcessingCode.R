# Path --------------------------------------------------------------------
#setwd("D:/OneDrive - CGIAR/RMI WP1/Armonizacion")
#setwd("C:/Users/feder.DESKTOP-471V2SD/OneDrive - CGIAR/RMI WP1/Armonizacion")
 setwd("C:/Users/rszap/OneDrive - Universidad Nacional de Colombia/CIAT/RFM WP1/Armonizacion/")


# General -----------------------------------------------------------------
rm(list=ls())
source("Endline/endlineProcessingFunctions.R")

excel_path <- "./Endline/endlineRaw.xlsx"
dictionary_path <- "./dictionary.xlsx"
output_path <- "./dictionary.csv"

classify_variables_by_tab(excel_path, dictionary_path, output_path)

# Load the Excel file and the dictionary
dictionary <- read.csv("dictionary.csv")
dictionary <- dictionary[which(dictionary$endline == 1),]

# Define the tabs and their corresponding suffixes
tabs <- list(
  "lands" = "_trr",
  "repeatvar" = "_var",
  "pnd_repeat" = "_dop",
  "sales_repeat" = "_buy",
  "sales_process_repeat" = "_typ",
  "tech_repeat" = "_tec"
)


# EndlineAgg --------------------------------------------------------------
# Step 1: Aggregate the sales_process_repeat tab
sales_process_df <- read_excel(excel_path, sheet = "sales_process_repeat")

# Filter dictionary for the sales_process_repeat tab
sales_process_dict <- dictionary %>% filter(el_loop == "sales_process_repeat")

# Aggregate the sales_process_repeat data
aggregated_sales_process <- aggregate_data(sales_process_df, sales_process_dict, "_submission__uuid", "_typ")


# Step 2: Read the sales_repeat tab and merge the aggregated_sales_process into it
sales_df <- read_excel(excel_path, sheet = "sales_repeat")

# Merge the aggregated sales_process data into sales_repeat data based on a common identifier
sales_df <- left_join(sales_df, aggregated_sales_process, by = "_submission__uuid")


# Step 3: Aggregate the sales_repeat tab that now includes aggregated_sales_process data
sales_dict <- dictionary %>% filter(el_loop == "sales_repeat")

# Aggregate the final sales_repeat data
aggregated_sales <- aggregate_data(sales_df, sales_dict, "_submission__uuid", "_buy")


# Step 4: Aggregate data for other tabs and include the final aggregated_sales data
aggregated_data <- map2(tabs, names(tabs), function(suffix, tab) {
  if (tab == "sales_repeat") {
    return(aggregated_sales)  # Return the already aggregated sales data
  } else {
    df <- read_excel(excel_path, sheet = tab)
    
    # Filter dictionary for the current tab using the Tab column
    tab_dict <- dictionary %>% filter(el_loop == tab)
    
    aggregated_df <- aggregate_data(df, tab_dict, "_submission__uuid", suffix)
    return(aggregated_df)
  }
})

# Merge all aggregated data frames by _submission__uuid
aggregated_df <- reduce(aggregated_data, full_join, by = "_submission__uuid")
base_df <- read_excel(excel_path, sheet = "Encuesta de Línea Intermedia...")
df <- left_join(base_df, aggregated_df, by = c("_uuid"="_submission__uuid"))

#Adjusting numeric columns and creating dummies
df <- convert_numeric_columns(df)
#df <- convert_to_dummies_multiple(df, dictionary)

# Standardize weights and distances
df <- standardize_weights(df, dictionary)
df <- standardize_distances(df, dictionary)
colnames(df)

# Changing names and classes
df <- df %>% 
  select(-amountSoldInKgGreen_typ_buy) %>% 
  rename(amountSoldInKgGreen_typ_buy = E69CA) %>%
  mutate(
    amountSoldInKgGreen_typ_buy = as.numeric(amountSoldInKgGreen_typ_buy),
    E2XC                               = as.numeric(E2XC),
    E3XC                               = as.numeric(E3XC),
    E4C                                = as.numeric(E4C))

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

#Turning dummies into 1 and 0
binary_vars <- dictionary %>% filter(is.na(el_loop)) %>% filter(!is.na(Binaria) & Binaria == 1) %>% pull(new)
existing_binary_vars <- intersect(binary_vars, names(df))
df <- df %>% mutate(
  across(all_of(existing_binary_vars), ~ ifelse(. %in% c("Y", "y", "yes", 1), 1, 0)))

# Save
write.csv(df, "./Endline/endlineAgg.csv", row.names = FALSE)


# Assessing data quality --------------------------------------------------
#rm(list=ls())
df <- read.csv("./Armonizacion/Endline/endlineAgg.csv")
#source("./qualityCheckFunctions.R")
#results <- checkDataQuality(df)

