
setwd("C:/Users/feder.DESKTOP-471V2SD/OneDrive - CGIAR/RMI WP1/Armonizacion")

source("Baseline/baselineProcessingFunctions.R")

excel_path <- "C:/Users/feder.DESKTOP-471V2SD/OneDrive - CGIAR/RMI WP1/Armonizacion/Baseline/baselineRaw.xlsx"
dictionary_path <- "C:/Users/feder.DESKTOP-471V2SD/OneDrive - CGIAR/RMI WP1/Armonizacion/dictionary.xlsx"
output_path <- "C:/Users/feder.DESKTOP-471V2SD/OneDrive - CGIAR/RMI WP1/Armonizacion/dictionary.csv"

classify_variables_by_tab(excel_path, dictionary_path, output_path)

# Load the Excel file and the dictionary
excel_path <- "Baseline/baselineRaw.xlsx"
dictionary <- read.csv("dictionary.csv")
dictionary <- dictionary[which(dictionary$baseline == 1),]

# Define the tabs and their corresponding suffixes
tabs <- list(
  "lands" = "_trr",
  "repeatvar" = "_var",
  "pnd_repeat" = "_dop",
  "sales_repeat" = "_buy",
  "sales_process_repeat" = "_typ",
  "tech_repeat" = "_tec"
)

# Step 1: Aggregate the sales_process_repeat tab
sales_process_df <- read_excel(excel_path, sheet = "sales_process_repeat")

# Filter dictionary for the sales_process_repeat tab
sales_process_dict <- dictionary %>% filter(bl_loop == "sales_process_repeat")

# Aggregate the sales_process_repeat data
aggregated_sales_process <- aggregate_data(sales_process_df, sales_process_dict, "ROWUUID", "_typ")

# Step 2: Read the sales_repeat tab and merge the aggregated_sales_process into it
sales_df <- read_excel(excel_path, sheet = "sales_repeat")

# Merge the aggregated sales_process data into sales_repeat data based on a common identifier
sales_df <- left_join(sales_df, aggregated_sales_process, by = "ROWUUID")

# Step 3: Aggregate the sales_repeat tab that now includes aggregated_sales_process data
sales_dict <- dictionary %>% filter(bl_loop == "sales_repeat")

# Aggregate the final sales_repeat data
aggregated_sales <- aggregate_data(sales_df, sales_dict, "ROWUUID", "_buy")

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

