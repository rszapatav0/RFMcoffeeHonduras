setwd("C:/Users/rszap/OneDrive - Universidad EAFIT/CIAT/RFM WP1/Armonizacion/")
excel_path <- "Baseline/baselineRaw.xlsx"
dictionary_path <- "dictionary.xlsx"
output_path <- "dictionary.csv"
classify_variables_by_tab(excel_path, dictionary_path, output_path)
dictionary <- read.csv("dictionary.csv")
dictionary <- dictionary[which(dictionary$baseline == 1),]


##################################
# Function for multiple selection variables -------------------------------
df <- read_excel(excel_path, sheet = "area_terr")
convert_to_dummies_multiple <- function(df, dictionary) {
  # Iterate over each column in the dataframe
  for (column in names(df)) {
    # Check if the column is identified as "Categorica" in the dictionary
    if (column %in% dictionary$new) {
      dict_row <- dictionary %>% filter(new == column)
      
      if (!is.na(dict_row$Categorica) && dict_row$Categorica == 3) {
        
        # Step 1: Find all unique values in the column
        # First, split the values by commas, then flatten the list and remove duplicates
        unique_values <- unique(unlist(strsplit(as.character(df[[column]]), ",")))
        print(unique_values)
        
        # Step 2: Create dummy variables for each unique value
        for (value in unique_values) {
          # Create a new dummy column for each unique value
          dummy_name <- paste0(colnames(df[column]),"/", value)
          df[[dummy_name]] <- ifelse(grepl(value, df[[column]]), 1, 0)
        }
      }
    }
  }
  return(df)
}

# Applying the function to a dataframe
df2 <- convert_to_dummies_multiple(df, dictionary)
df2 <- df %>% select(-ROWUUID)


#########################################################
# Replace row prefixes ----------------------------------------------------
df <- read_excel(excel_path, sheet = "sales_process_repeat")


replaceColumnPrefixes <- function(dataframe, prefixMapping) {
  # Iterate over each prefix mapping
  for (oldPrefix in names(prefixMapping)) {
    # Construct a pattern to find columns that are exactly the old prefix or
    # start with the old prefix followed by an underscore and any characters
    pattern <- paste0("^", oldPrefix, "(_|$)")
    
    # Find column names matching the pattern
    colsToChange <- grep(pattern, colnames(dataframe), value = TRUE)
    
    # Replace old prefix with new prefix in these column names
    newColNames <- sub(oldPrefix, prefixMapping[[oldPrefix]], colsToChange)
    
    # Assign the new column names back to the dataframe
    names(dataframe)[names(dataframe) %in% colsToChange] <- newColNames
  }
  
  return(dataframe)
}






