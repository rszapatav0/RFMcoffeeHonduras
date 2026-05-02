# Loading Packages
packageList <- c("dplyr", "readxl", "purrr", "tidyr", "tidyr", "stringr")
lapply(packageList,require,character.only=TRUE)

# Function to identify variables inside each spreadsheet 
classify_variables_by_tab <- function(data_path, dictionary_path, output_path) {
  
  # Load the dictionary
  dictionary <- read_excel(dictionary_path) %>%
    ## Initialize a Tab column in the dictionary as an empty string
    mutate(Tab = "")
  
  # Get the names of all sheets (tabs) in the Data file
  sheet_names <- excel_sheets(data_path)
  
  # Loop by sheet
  for (sheet_name in sheet_names) {
    ## Skip the "Encuesta de Linea Intermedia..." tab
    if (sheet_name == "Encuesta de Línea Intermedia...") {
      next
    }
    ## Reading sheet variables
    cat("\nProcessing Sheet:", sheet_name, "\n")
    column_names <- colnames(read_excel(data_path, sheet = sheet_name, n_max = 1))
    ## Update the Tab column in the dictionary for variables found in the current sheet
    dictionary <- dictionary %>%
      rowwise() %>%
      mutate(Tab = if_else(any(str_detect(column_names, regex(new, ignore_case = TRUE))), sheet_name, Tab))
  }
  
  # Write the updated dictionary to a CSV file
  write.csv(dictionary, output_path, row.names = FALSE)
}

# Convert all-numeric columns into numeric type
convert_numeric_columns <- function(df) {
  for (column in names(df)) {
    # Skip already numeric columns
    if (is.numeric(df[[column]])) {
      next
    }
    
    # Convert non-numeric columns to character first (handles factors, logicals, etc.)
    if (!is.character(df[[column]])) {
      df[[column]] <- as.character(df[[column]])
    }
    
    # Trim whitespace
    trimmed_column <- trimws(df[[column]])
    
    # Replace empty strings with NA to avoid false negatives
    trimmed_column[trimmed_column == ""] <- NA
    
    # Check if all non-NA values in the column are numeric
    if (all(!is.na(suppressWarnings(as.numeric(trimmed_column))))) {
      # Convert the column to numeric
      df[[column]] <- as.numeric(trimmed_column)
    }
  }
  return(df)
}

# Function to convert categorical variables to dummy variables
convert_to_dummies <- function(df, dictionary) {
  # Iterate over each column in the dataframe
  for (column in names(df)) {
    # Check if the column is identified as "Categorica" in the dictionary
    if (column %in% dictionary$new) {
      dict_row <- dictionary %>% filter(new == column)
      if (all(!is.na(dict_row$Categorica) & dict_row$Categorica == 1)) {
        
        df[[column]] <- as.character(df[[column]])  # Ensure it's a character type
        df[[column]][is.na(df[[column]])] <- "missing"  # Convert blank strings to NA
        
        # Convert the categorical variable into dummy variables
        dummies <- model.matrix(~.-1, data = df[column])
        
        # Rename the columns of dummies to indicate the original variable and its levels
        colnames(dummies) <- gsub(colnames(df[column]), paste0(colnames(df[column]),"_"), colnames(dummies))
        
        # Add the new dummy variables to the dataframe
        df <- cbind(df, dummies[, !colnames(dummies) %in% names(df), drop = FALSE])
      }
    }
  }
  
  # Remove the original categorical columns from the dataframe
  #df <- df[, !names(df) %in% dictionary$new[dictionary$Categorica == 1]]   #!!!!!!!!!!!!!!!!!!!!!!!!!
  
  return(df)
}

# Function to convert dummies to multiple variables
convert_to_dummies_multiple <- function(df, dictionary) {
  # Iterate over each column in the dataframe
  for (column in names(df)) {
    # Check if the column is identified as "Categorica" in the dictionary
    if (column %in% dictionary$new) {
      dict_row <- dictionary %>% filter(new == column)
      
      if (all(!is.na(dict_row$Categorica) && dict_row$Categorica == 2)) {
        
        # Step 1: Find all unique values in the column
        # First, split the values by commas, then flatten the list and remove duplicates
        unique_values <- unique(unlist(strsplit(as.character(df[[column]]), ",")))
        unique_values <- unique_values[!is.na(unique_values) & unique_values != ""]
        
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

# Function to standardize weights to KG
standardize_weights <- function(df, dictionary) {
  # Function to convert the weight to green coffee weight in kgs for vector inputs
  convert_to_green_kgs <- function(presentation, weight, unit) {
    conversion_factors <- c(
      "qq"    = 100 * 0.453592,   #Quintales (100 libras)
      "lata"  = 33 * 0.453592,    #Latas
      "carga" = 200 * 0.453592,   #Cargas
      "lb"    = 0.453592          #Libras
    )
    
    presentation_factors <- c(
      "cherry"                = 0.2155 * 0.8,   #Uva
      "green"                 = 1.0000,         #Verde
      "soaking_wet_parchment" = 0.48 * 0.8,     #Pergamino mojado
      "dry_parchment"         = 0.8,            #Pergamino seco
      "roasted"               = 1.19            #Tostado
      )
    
    #All weights to kilograms
    #weight_in_kgs <- weight * conversion_factors[unit]
    weight_in_kgs <- ifelse(
      weight == 777, NA,
      ifelse(
        unit %in% names(conversion_factors),
        weight * conversion_factors[unit],
        weight)
    )
    #All presentations to green
    green_coffee_weight <- ifelse(
        presentation %in% names(presentation_factors),
        weight_in_kgs * presentation_factors[presentation],
        weight_in_kgs)
    
    return(green_coffee_weight)
  }
  
  convert_to_green_kgs <- Vectorize(convert_to_green_kgs)
  
  # Function to create a standardized column name
  create_standardized_col_name <- function(base_name, suffix) {
    paste0(base_name, suffix)
  }
  
  # Standardize produced weight
  if ("weight_produced" %in% names(dictionary)) {
    presentation_col <- dictionary$new[dictionary$weight_produced == 1 & !is.na(dictionary$weight_produced)]
    weight_col <- dictionary$new[dictionary$weight_produced == 2 & !is.na(dictionary$weight_produced)]
    unit_col <- dictionary$new[dictionary$weight_produced == 3 & !is.na(dictionary$weight_produced)]
    if (length(presentation_col) == 1 && !is.na(presentation_col) && presentation_col != "" &&
        length(weight_col) == 1       && !is.na(weight_col)       && weight_col != ""       &&
        length(unit_col) == 1         && !is.na(unit_col)         && unit_col != "")  {
      if (presentation_col %in% names(df) && weight_col %in% names(df) && unit_col %in% names(df)) {
        new_col_name <- create_standardized_col_name(weight_col, "InKgGreen")
        df[[new_col_name]] <- convert_to_green_kgs(df[[presentation_col]], df[[weight_col]], df[[unit_col]])
      }
    }
  }
  
  # Standardize sold weight
  if ("weight_sold" %in% names(dictionary)) {
    presentation_col <- dictionary$new[dictionary$weight_sold == 1 & !is.na(dictionary$weight_sold)]
    weight_col <- dictionary$new[dictionary$weight_sold == 2 & !is.na(dictionary$weight_sold)]
    unit_col <- dictionary$new[dictionary$weight_sold == 3 & !is.na(dictionary$weight_sold)]
    if (length(presentation_col) == 1 && !is.na(presentation_col) && presentation_col != "" &&
        length(weight_col) == 1       && !is.na(weight_col)       && weight_col != ""       &&
        length(unit_col) == 1         && !is.na(unit_col)         && unit_col != "")  {
      if (presentation_col %in% names(df) && weight_col %in% names(df) && unit_col %in% names(df)) {
        new_col_name <- create_standardized_col_name(weight_col, "InKgGreen")
        df[[new_col_name]] <- convert_to_green_kgs(df[[presentation_col]], df[[weight_col]], df[[unit_col]])
      }
    }
  }

  return(df)
}

# Function to standardize prices to green KG
standardize_prices <- function(df, dictionary) {
  
  # Function to convert the price to green coffee price in kgs for vector inputs
  convert_to_green_kgs <- function(presentation, price, unit) {
    conversion_factors <- c(
      "qq"    = 100 * 0.453592,   #Quintales (100 libras)
      "lata"  = 33 * 0.453592,    #Latas
      "carga" = 200 * 0.453592,   #Cargas
      "lb"    = 0.453592          #Libras
      )
    
    presentation_factors <- c(
      "cherry"                = 0.2155 * 0.8,   #Uva
      "green"                 = 1.0000,         #Verde
      "soaking_wet_parchment" = 0.48 * 0.8,     #Pergamino mojado
      "dry_parchment"         = 0.8,            #Pergamino seco
      "roasted"               = 1.19            #Tostado
      )
    
    #All weights to kilograms
    #price_in_kgs <- price / conversion_factors[unit]
    price_in_kgs <- ifelse(
      price == 777, NA,
      ifelse(
        unit %in% names(conversion_factors),
        price / conversion_factors[unit],
        price)
      )
    #All presentations to green
    green_coffee_price <- ifelse(
        presentation %in% names(presentation_factors),
        price_in_kgs / presentation_factors[presentation],
        price_in_kgs)
    
    return(green_coffee_price)
    }
    
  convert_to_green_kgs <- Vectorize(convert_to_green_kgs)
  
  # Function to create a standardized column name
  create_standardized_col_name <- function(base_name, suffix) {
    paste0(base_name, suffix)
    }
  
  # Standardize min weight
  if ("price_min" %in% names(dictionary)) {
    presentation_col <- dictionary$new[dictionary$price_min == 1 & !is.na(dictionary$price_min)]
    price_col <- dictionary$new[dictionary$price_min == 2 & !is.na(dictionary$price_min)]
    unit_col <- dictionary$new[dictionary$price_min == 3 & !is.na(dictionary$price_min)]
    if (presentation_col %in% names(df) && price_col %in% names(df) && unit_col %in% names(df)) {
      new_col_name <- create_standardized_col_name(price_col, "InKgGreen")
      df[[new_col_name]] <- convert_to_green_kgs(df[[presentation_col]], df[[price_col]], df[[unit_col]])
    }
    }
  
  # Standardize min weight
  if ("price_max" %in% names(dictionary)) {
    presentation_col <- dictionary$new[dictionary$price_max == 1 & !is.na(dictionary$price_max)]
    price_col <- dictionary$new[dictionary$price_max == 2 & !is.na(dictionary$price_max)]
    unit_col <- dictionary$new[dictionary$price_max == 3 & !is.na(dictionary$price_max)]
    if (presentation_col %in% names(df) && price_col %in% names(df) && unit_col %in% names(df)) {
      new_col_name <- create_standardized_col_name(price_col, "InKgGreen")
      df[[new_col_name]] <- convert_to_green_kgs(df[[presentation_col]], df[[price_col]], df[[unit_col]])
    }
  }
  
  return(df)
}

# Function to standardize areas to hectares
standardize_areas <- function(df, dictionary) {
  
  # Function to convert any area to area in hectares for vector inputs
  convert_to_hectares <- function(area, unit) {
    conversion_factors <- c(
      "mz" = 7000 / 10000,
      "tarea" = 437.5 / 10000,
      "hectare" = 1,
      "metros2" = 0.0001,
      "vr" = 0.00006987,
      "other" = 0
    )
    
    #if (!unit %in% names(conversion_factors)) {
    #  stop("Unknown unit of area")
    #}
    
    #area_in_hectares <- area * conversion_factors[[unit]]
    area_in_hectares <- ifelse(
      unit %in% names(conversion_factors),
      area * conversion_factors[[unit]],
      area)    
    return(area_in_hectares)
  }
  
  convert_to_hectares <- Vectorize(convert_to_hectares)
  
  # Function to create a standardized column name
  create_standardized_col_name <- function(base_name, suffix) {
    paste0(base_name, suffix)
  }
  
  # Standardize areas based on the dictionary
  if ("area_total" %in% names(dictionary)) {
    unit_col <- dictionary$new[dictionary$area_total == 2 & !is.na(dictionary$area_total)]
    area_col <- dictionary$new[dictionary$area_total == 1 & !is.na(dictionary$area_total)]
    if (length(unit_col) == 1 && !is.na(unit_col) && unit_col != "" &&
        length(area_col) == 1 && !is.na(area_col) && area_col != "") {
      if (unit_col %in% names(df) && area_col %in% names(df)) {
        new_col_name <- create_standardized_col_name(area_col, "InHa")
        df[[new_col_name]] <- convert_to_hectares(df[[area_col]], df[[unit_col]])
      }
    }
  }
  
  if ("area_coffee" %in% names(dictionary)) {
    unit_col <- dictionary$new[dictionary$area_coffee == 2 & !is.na(dictionary$area_coffee)]
    area_col <- dictionary$new[dictionary$area_coffee == 1 & !is.na(dictionary$area_coffee)]
    if (length(unit_col) == 1 && !is.na(unit_col) && unit_col != "" &&
        length(area_col) == 1 && !is.na(area_col) && area_col != "") {
      if (unit_col %in% names(df) && area_col %in% names(df)) {
        new_col_name <- create_standardized_col_name(area_col, "InHa")
        df[[new_col_name]] <- convert_to_hectares(df[[area_col]], df[[unit_col]])
      }
    }
  }
  
  if ("area_productive" %in% names(dictionary)) {
    unit_col <- dictionary$new[dictionary$area_productive == 2 & !is.na(dictionary$area_productive)]
    area_col <- dictionary$new[dictionary$area_productive == 1 & !is.na(dictionary$area_productive)]
    if (length(unit_col) == 1 && !is.na(unit_col) && unit_col != "" &&
        length(area_col) == 1 && !is.na(area_col) && area_col != "") {
      if (unit_col %in% names(df) && area_col %in% names(df)) {
        new_col_name <- create_standardized_col_name(area_col, "InHa")
        df[[new_col_name]] <- convert_to_hectares(df[[area_col]], df[[unit_col]])
      }
    }
  }
  
  return(df)
}

# Function to standardize distances to meters
standardize_distances <- function(df, dictionary) {
  
  # Function to convert any distance to meters for vector inputs
  convert_to_meters <- function(distance, unit) {
    conversion_factors <- c(
      "brazadas" = 1.67,   # 1 Brazada ≈ 1.67 meters
      "in" = 0.0254,       # 1 inch (pulgada) = 0.0254 meters
      "pie" = 0.3048,      # 1 foot (pie) = 0.3048 meters
      "vr" = 0.836127,     # 1 vara = 0.836127 meters
      "cm" = 0.01,         # 1 centimeter (centimetro) = 0.01 meters
      "cuartas" = 0.208,   # 1 cuarta ≈ 0.208 meters
      "yd" = 0.9144,       # 1 yard (yarda) = 0.9144 meters
      "mt" = 1             # 1 meter = 1 meter
    )
    
    distance_in_meters <- ifelse(
      distance == 777,
      NA,
      ifelse(
        unit %in% names(conversion_factors),
        distance * conversion_factors[[unit]],
        distance)
      )
    
    return(distance_in_meters)
  }
  
  convert_to_meters <- Vectorize(convert_to_meters)
  
  # Function to create a standardized column name
  create_standardized_col_name <- function(base_name, suffix) {
    paste0(base_name, suffix)
  }
  
  # Standardize distances based on the dictionary
  if ("distance_within" %in% names(dictionary)) {
    unit_col <- dictionary$new[dictionary$distance_within == 2 & !is.na(dictionary$distance_within)]
    distance_col <- dictionary$new[dictionary$distance_within == 1 & !is.na(dictionary$distance_within)]
    if (length(unit_col)     == 1 && !is.na(unit_col)     && unit_col != "" &&
        length(distance_col) == 1 && !is.na(distance_col) && distance_col != "") {
      if (unit_col %in% names(df) && distance_col %in% names(df)) {
        new_col_name <- create_standardized_col_name(distance_col, "InMt")
        df[[new_col_name]] <- convert_to_meters(df[[distance_col]], df[[unit_col]])
      }
    }
  }
  
  if ("distance_between" %in% names(dictionary)) {
    unit_col <- dictionary$new[dictionary$distance_between == 2 & !is.na(dictionary$distance_between)]
    distance_col <- dictionary$new[dictionary$distance_between == 1 & !is.na(dictionary$distance_between)]
    if (length(unit_col)     == 1 && !is.na(unit_col)     && unit_col != "" &&
        length(distance_col) == 1 && !is.na(distance_col) && distance_col != "") {
      if (unit_col %in% names(df) && distance_col %in% names(df)) {
        new_col_name <- create_standardized_col_name(distance_col, "InMt")
        df[[new_col_name]] <- convert_to_meters(df[[distance_col]], df[[unit_col]])
      }
    }
  }
  
  return(df)
}

# Function to apply the desired aggregation
aggregate_data <- function(df, dict, farm_id_col, tab_suffix) {
  
  df <- convert_numeric_columns(df)
  df <- convert_to_dummies(df, dictionary)
  #df <- convert_to_dummies_multiple(df, dictionary)


  # Convert binary variables (Y -> 1, others -> 0)
  binary_vars <- dict %>% filter(!is.na(Binaria) & Binaria == 1) %>% pull(new)
  df <- df %>%
    mutate(across(all_of(binary_vars), ~ ifelse(. %in% c("Y", "y", "yes", 1), 1, 0)))

  # Renaming columns based on the tab (el_loop)
  if (tab_suffix == "_trr") {  # Assuming "_trr" is the suffix for a specific tab
    df <- df %>% rename(
      totalFarmAreaInHa = E3XC,
      totalAreaUsedForAgricultureInHa = E4C,
      coffeeAreaLastHarvestInHa = E2XC
    )
  } else if (tab_suffix == "_buy") {  # Assuming "_buy" is the suffix for another specific tab
    df <- df %>% 
      select(-amountSoldInKgGreen_typ) %>%
      rename(
        amountSoldInKgGreen_typ = E69C,
        buyerCategory = E64C,
        numberOfCoffeeSales = E67C
    )
  } else if (tab_suffix == "_typ") {  # Assuming "_typ" is the suffix for another specific tab
     # Changing names
     df <- df %>% rename(
       typeCoffeeSoldToBuyer = form1,
       amountSoldInKgGreen = E70CB,
       amountSold = saleAmountInUnits
    )
    df$amountSoldInKgGreen <- as.numeric(df$amountSoldInKgGreen)
    # Conditionally standardize measurements before aggregation
    df <- standardize_prices(df, dict)
  }
  
  df_agg <- df %>%
    group_by(!!sym(farm_id_col)) %>%
    summarise(across(everything(), ~ {
      # Get the corresponding row in the dictionary
      dict_row <- dict %>% filter(new == cur_column())
     
      if (nrow(dict_row) == 0) {
        return(NA_real_)  # If no match is found in the dictionary, return NA
      }
      
      # Check if the variable is continuous and apply the appropriate aggregation
      if (!is.na(dict_row$Continua) && dict_row$Continua == 1) {
        if (!is.na(dict_row$Promedio) && dict_row$Promedio == 1) {
          return(mean(.x, na.rm = TRUE))
        }
        if (!is.na(dict_row$Suma) && dict_row$Suma == 1) {
          return(sum(.x, na.rm = TRUE))
        }
        if (!is.na(dict_row$Maximo) && dict_row$Maximo == 1) {
          #return(max(.x, na.rm = TRUE))
          return(if   (all(is.na(.x))) NA_real_ 
                 else max(.x, na.rm = TRUE))
        }
        if (!is.na(dict_row$Minimo) && dict_row$Minimo == 1) {
          #return(min(.x, na.rm = TRUE))
          return(if   (all(is.na(.x))) NA_real_ 
                 else min(.x, na.rm = TRUE))
        }
      }
      
      # Check if the variable is discrete and apply the appropriate aggregation
      if (!is.na(dict_row$Discreta) && dict_row$Discreta == 1) {
        if (!is.na(dict_row$Promedio) && dict_row$Promedio == 1) {
          return(mean(.x, na.rm = TRUE))
        }
        if (!is.na(dict_row$Suma) && dict_row$Suma == 1) {
          return(sum(.x, na.rm = TRUE))
        }
        if (!is.na(dict_row$Maximo) && dict_row$Maximo == 1) {
          return(max(.x, na.rm = TRUE))
        }
        if (!is.na(dict_row$Minimo) && dict_row$Minimo == 1) {
          return(min(.x, na.rm = TRUE))
        }
      }
      
      # Check if the variable is binary and apply the appropriate aggregation
      if (!is.na(dict_row$Binaria) && dict_row$Binaria == 1) {
        if (!is.na(dict_row$Porcentaje) && dict_row$Porcentaje == 1) {
          return(mean(.x, na.rm = TRUE) * 100)
        }
        if (!is.na(dict_row$IfAny) && dict_row$IfAny == 1) {
          return(ifelse(any(.x == 1),1,0))
        }
        if (!is.na(dict_row$Count) && dict_row$Count == 1) {
          return(sum(.x, na.rm = TRUE))
        }
      }
      
      return(NA_real_)  # If no conditions are met, return NA
    }))
  
  # Remove columns that are entirely NA
  df_agg <- df_agg %>% select(where(~ !all(is.na(.))))

  # Rename columns to include the tab suffix
  names(df_agg)[-1] <- paste0(names(df_agg)[-1], tab_suffix)

  return(df_agg)
}


