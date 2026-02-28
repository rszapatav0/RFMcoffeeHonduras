# Path --------------------------------------------------------------------
#setwd("D:/OneDrive - CGIAR/RMI WP1/Armonizacion")
#setwd("C:/Users/feder.DESKTOP-471V2SD/OneDrive - CGIAR/RMI WP1/Armonizacion")
 setwd("C:/Users/rszap/OneDrive - Universidad EAFIT/CIAT/RFM WP1/Armonizacion/")


# General -----------------------------------------------------------------
rm(list=ls())
source("Baseline/baselineProcessingFunctions.R")

excel_path <- "Baseline/baselineRaw.xlsx"
dictionary_path <- "dictionary.xlsx"
output_path <- "dictionary.csv"

classify_variables_by_tab(excel_path, dictionary_path, output_path)

# Load the Excel file and the dictionary
dictionary <- read.csv("dictionary.csv")
dictionary <- dictionary[which(dictionary$baseline == 1),]

# Define the tabs and their corresponding suffixes
tabs <- list(
  "area_terr" = "_trr",
  "repeatvar" = "_var",
  "pnd_repeat" = "_dop",
  "sales_repeat" = "_buy",
  "sales_process_repeat" = "_typ",
  "tech_repeat" = "_tec",
  "hh_pop_repeat" = "_pop"
)


# BaselineAgg -------------------------------------------------------------
# Step 1: Aggregate the sales_process_repeat tab
sales_process_df <- read_excel(excel_path, sheet = "sales_process_repeat")

# Filter dictionary for the sales_process_repeat tab
sales_process_dict <- dictionary %>% filter(bl_loop == "sales_process_repeat")

# Aggregate the sales_process_repeat data
aggregated_sales_process <- aggregate_data(sales_process_df, sales_process_dict, "ID_ENCUESTA", "_typ")


# Step 2: Read the sales_repeat tab and merge the aggregated_sales_process into it
sales_df <- read_excel(excel_path, sheet = "sales_repeat")

# Merge the aggregated sales_process data into sales_repeat data based on a common identifier
sales_df <- left_join(sales_df, aggregated_sales_process, by = "ID_ENCUESTA")


# Step 3: Aggregate the sales_repeat tab that now includes aggregated_sales_process data
sales_dict <- dictionary %>% filter(bl_loop == "sales_repeat")

# Aggregate the final sales_repeat data
aggregated_sales <- aggregate_data(sales_df, sales_dict, "ID_ENCUESTA", "_buy")


# Step 4: Aggregate data for other tabs and include the final aggregated_sales data
aggregated_data <- map2(tabs, names(tabs), function(suffix, tab) {
  if (tab == "sales_repeat") {
    return(aggregated_sales)  # Return the already aggregated sales data
  } else {
    df <- read_excel(excel_path, sheet = tab)
    
    # Filter dictionary for the current tab using the Tab column
    tab_dict <- dictionary %>% filter(bl_loop == tab)
    
    aggregated_df <- aggregate_data(df, tab_dict, "ID_ENCUESTA", suffix)
    return(aggregated_df)
  }
})

# Merge all aggregated data frames by _submission__uuid
aggregated_df <- reduce(aggregated_data, full_join, by = "ID_ENCUESTA")
base_df <- read_excel(excel_path, sheet = "mod_combined")
df <- left_join(base_df, aggregated_df, by = "ID_ENCUESTA")
df <- df %>% rename(surveyID = ID_ENCUESTA)

#Adjusting numeric columns and creating dummies
#*There are some problems with accessedCreditOrLoan. Temporal solution:
df$accessedCreditOrLoan2 <- df$accessedCreditOrLoan   #!!!!!!!!!!!!!!!!!!!!!!!!!
df <- convert_numeric_columns(df,dictionary)
df <- convert_to_dummies_multiple(df, dictionary)
df$accessedCreditOrLoan <- df$accessedCreditOrLoan2   #!!!!!!!!!!!!!!!!!!!!!!!!!
df$accessedCreditOrLoan2 <- NULL                      #!!!!!!!!!!!!!!!!!!!!!!!!!


# Standardize areas, weights and distances
df <- standardize_areas(df, dictionary) #yes
df <- standardize_weights(df, dictionary) #yes, production
colnames(df)

# Creating new variables
df$yieldKgPerHa       <- ifelse(df$coffeeAreaLastHarvestInHa == 0,
                                NA, df$amountOfCoffeeProducedLastHarvestInKgGreen / df$coffeeAreaLastHarvestInHa)
df$densityPlantsPerHa <- ifelse(df$coffeeAreaLastHarvestInHa == 0,
                                NA, df$totalCoffeePlantsOnFarm / df$coffeeAreaLastHarvestInHa)
summary(df$yieldKgPerHa)
summary(df$densityPlantsPerHa)

#Turning dummies into 1 and 0
binary_vars <- dictionary %>% filter(is.na(bl_loop)) %>% filter(!is.na(Binaria) & Binaria == 1) %>% pull(new)
existing_binary_vars <- intersect(binary_vars, names(df))
df <- df %>%
  mutate(across(all_of(existing_binary_vars), ~ ifelse(. %in% c("Y", "y", "yes", 1), 1, 0)))

# Save 
write.csv(df, "./Baseline/baselineAgg.csv", row.names = FALSE)
#df <- read.csv("./Baseline/baselineAgg.csv")


# Assessing data quality --------------------------------------------------
#rm(list=ls())
#df <- read.csv("./Baseline/baselineAgg.csv")
#source("./qualityCheckFunctions.R")
#results <- checkDataQuality(df)

