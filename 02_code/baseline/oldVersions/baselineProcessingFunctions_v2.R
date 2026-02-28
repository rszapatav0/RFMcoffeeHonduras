library(dplyr)
library(readxl)
library(purrr)
library(tidyr)
library(stringr)  # For string detection

# Function to identify variables inside each spreadsheet 
classify_variables_by_tab <- function(excel_path, dictionary_path, output_path) {
  
  # Load the dictionary
  dictionary <- read_excel(dictionary_path)
  
  # Get the names of all sheets (tabs) in the Excel file
  sheet_names <- excel_sheets(excel_path)
  
  # Initialize a Tab column in the dictionary as an empty string
  dictionary <- dictionary %>%
    mutate(Tab = "")
  
  # Function to read a sheet and return the column names
  get_column_names <- function(sheet_name) {
    df <- read_excel(excel_path, sheet = sheet_name, n_max = 1)  # Read only the first row to get the column names
    column_names <- colnames(df)
    return(column_names)
  }
  
  # Iterate through each sheet and classify variables by tab
  for (sheet_name in sheet_names) {
    # Skip the "Encuesta de Línea Intermedia..." tab
    if (sheet_name == "Encuesta de Línea Intermedia...") {
      next
    }
    
    cat("\nProcessing Sheet:", sheet_name, "\n")
    column_names <- get_column_names(sheet_name)
    
    # Update the Tab column in the dictionary for variables found in the current sheet
    dictionary <- dictionary %>%
      rowwise() %>%
      mutate(Tab = if_else(any(str_detect(column_names, regex(new, ignore_case = TRUE))), sheet_name, Tab))
  }
  
  # Write the updated dictionary to a CSV file
  write.csv(dictionary, output_path, row.names = FALSE)
}

convert_numeric_columns <- function(df, dictionary) {
  for (column in names(df)) {
    # Check if the column is identified as "Continua" or "Discreta" in the dictionary
    if (column %in% dictionary$new) {
      dict_row <- dictionary[dictionary$new == column, ]
      if (!is.na(dict_row$Continua) && dict_row$Continua == 1 || 
          !is.na(dict_row$Discreta) && dict_row$Discreta == 1) {
        
        # Convert non-numeric columns to character first (handles factors, logicals, etc.)
        if (!is.character(df[[column]])) {
          df[[column]] <- as.character(df[[column]])
        }
        
        # Trim whitespace
        trimmed_column <- trimws(df[[column]])
        
        # Replace empty strings with NA to avoid false negatives
        trimmed_column[trimmed_column == ""] <- NA
        
        # Force conversion to numeric
        df[[column]] <- as.numeric(trimmed_column)
      }
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
      if (!is.na(dict_row$Categorica) && dict_row$Categorica == 1) {
        
        # Convert the categorical variable into dummy variables
        dummies <- model.matrix(~.-1, data = df[column])
        
        # Rename the columns of dummies to indicate the original variable and its levels
        colnames(dummies) <- paste(column, gsub("[^[:alnum:]_]", "", colnames(dummies)), sep = "_")
        
        # Add the new dummy variables to the dataframe
        df <- cbind(df, dummies)
      }
    }
  }
  
  # Remove the original categorical columns from the dataframe
  df <- df[, !names(df) %in% dictionary$new[dictionary$Categorica == 1]]
  
  return(df)
}

# Function to standardize weights to KG
standardize_weights <- function(df, dictionary) {
  # Function to convert the weight to green coffee weight in kgs for vector inputs
  convert_to_green_kgs <- function(presentation, weight, unit) {
    conversion_factors <- c(
      "Quintales (100 libras)" = 100 * 0.453592, 
      "Latas" = 33 * 0.453592,
      "Cargas (200 libras)" = 200 * 0.453592,
      "Libras" = 0.453592
    )
    
    presentation_factors <- c(
      "Uva" = 0.1823,
      "Oro" = 1.0000,
      "Pergamino mojado" = 0.4185,
      "Pergamino seco" = 0.8192
    )
    
    weight_in_kgs <- weight * conversion_factors[unit]
    
    green_coffee_weight <- ifelse(presentation %in% names(presentation_factors),
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
    if (presentation_col %in% names(df) && weight_col %in% names(df) && unit_col %in% names(df)) {
      new_col_name <- create_standardized_col_name(weight_col, "InGreenKgs")
      df[[new_col_name]] <- convert_to_green_kgs(df[[presentation_col]], df[[weight_col]], df[[unit_col]])
    }
  }
  
  # Standardize sold weight
  if ("weight_sold" %in% names(dictionary)) {
    presentation_col <- dictionary$new[dictionary$weight_sold == 1 & !is.na(dictionary$weight_sold)]
    weight_col <- dictionary$new[dictionary$weight_sold == 2 & !is.na(dictionary$weight_sold)]
    unit_col <- dictionary$new[dictionary$weight_sold == 3 & !is.na(dictionary$weight_sold)]
    if (presentation_col %in% names(df) && weight_col %in% names(df) && unit_col %in% names(df)) {
      new_col_name <- create_standardized_col_name(weight_col, "InGreenKgs")
      df[[new_col_name]] <- convert_to_green_kgs(df[[presentation_col]], df[[weight_col]], df[[unit_col]])
    }
  }
  
  return(df)
}


standardize_prices <- function(df, dictionary) {
  
  # Function to convert the price to price of green coffee in kgs for vector inputs
  calculate_green_price_per_kg <- function(presentation, green_coffee_weight, unit, price_per_unit) {
    invalid_unit <- unit == "Otro"
    green_coffee_weight[invalid_unit] <- NA
    price_per_unit[invalid_unit] <- NA
    
    price_per_kg <- price_per_unit / green_coffee_weight
    
    return(price_per_kg)
  }
  
  calculate_green_price_per_kg <- Vectorize(calculate_green_price_per_kg)
  
  # Function to create a standardized column name
  create_standardized_col_name <- function(base_name, suffix) {
    paste0(base_name, suffix)
  }
  
  # Function to process and standardize prices
  process_prices <- function(presentation_col, weight_col, unit_col, price_col) {
    new_col_name <- create_standardized_col_name(price_col, "PerKgGreen")
    df[[new_col_name]] <- NA  
    
    df[, new_col_name] <- calculate_green_price_per_kg(
      df[, presentation_col], 
      df[, weight_col], 
      df[, unit_col], 
      df[, price_col])
    
    return(df)
  }
  
  # Loop through the price types to standardize
  for (price_type in c("price_min", "price_max")) {
    if (price_type %in% names(dictionary)) {
      presentation_cols <- dictionary$new[dictionary[[price_type]] == 1 & !is.na(dictionary[[price_type]])]
      weight_cols <- dictionary$new[dictionary[[price_type]] == 2 & !is.na(dictionary[[price_type]])]
      unit_cols <- dictionary$new[dictionary[[price_type]] == 3 & !is.na(dictionary[[price_type]])]
      price_cols <- dictionary$new[dictionary[[price_type]] == 4 & !is.na(dictionary[[price_type]])]
      
      for (i in seq_along(presentation_cols)) {
        df <- process_prices(presentation_cols[i], weight_cols[i], unit_cols[i], price_cols[i])
      }
    }
  }
  
  return(df)
}

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
    
    if (!unit %in% names(conversion_factors)) {
      stop("Unknown unit of area")
    }
    
    area_in_hectares <- area * conversion_factors[[unit]]
    
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
    if (unit_col %in% names(df) && area_col %in% names(df)) {
      new_col_name <- create_standardized_col_name(area_col, "_ha")
      df[[new_col_name]] <- convert_to_hectares(df[[area_col]], df[[unit_col]])
    }
  }
  
  if ("area_coffee" %in% names(dictionary)) {
    unit_col <- dictionary$new[dictionary$area_coffee == 2 & !is.na(dictionary$area_coffee)]
    area_col <- dictionary$new[dictionary$area_coffee == 1 & !is.na(dictionary$area_coffee)]
    if (unit_col %in% names(df) && area_col %in% names(df)) {
      new_col_name <- create_standardized_col_name(area_col, "_ha")
      df[[new_col_name]] <- convert_to_hectares(df[[area_col]], df[[unit_col]])
    }
  }
  
  if ("area_productive" %in% names(dictionary)) {
    unit_col <- dictionary$new[dictionary$area_productive == 2 & !is.na(dictionary$area_productive)]
    area_col <- dictionary$new[dictionary$area_productive == 1 & !is.na(dictionary$area_productive)]
    if (unit_col %in% names(df) && area_col %in% names(df)) {
      new_col_name <- create_standardized_col_name(area_col, "_ha")
      df[[new_col_name]] <- convert_to_hectares(df[[area_col]], df[[unit_col]])
    }
  }
  
  return(df)
}

# Function to standardize distances
standardize_distances <- function(df, dictionary) {
  
  # Function to convert any distance to meters for vector inputs
  convert_to_meters <- function(distance, unit) {
    conversion_factors <- c(
      "Brazadas" = 1.67,         # 1 Brazada ≈ 1.67 meters
      "in" = 0.0254,             # 1 inch (pulgada) = 0.0254 meters
      "pie" = 0.3048,            # 1 foot (pie) = 0.3048 meters
      "vr" = 0.836127,           # 1 vara = 0.836127 meters
      "cm" = 0.01,               # 1 centimeter (centimetro) = 0.01 meters
      "cuarta" = 0.208,          # 1 cuarta ≈ 0.208 meters
      "yd" = 0.9144,             # 1 yard (yarda) = 0.9144 meters
      "mt" = 1                   # 1 meter = 1 meter
    )
    
    if (!unit %in% names(conversion_factors)) {
      stop("Unknown unit of distance")
    }
    
    distance_in_meters <- distance * conversion_factors[[unit]]
    
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
    if (unit_col %in% names(df) && distance_col %in% names(df)) {
      new_col_name <- create_standardized_col_name(distance_col, "_mt")
      df[[new_col_name]] <- convert_to_meters(df[[distance_col]], df[[unit_col]])
    }
  }
  
  if ("distance_between" %in% names(dictionary)) {
    unit_col <- dictionary$new[dictionary$distance_between == 2 & !is.na(dictionary$distance_between)]
    distance_col <- dictionary$new[dictionary$distance_between == 1 & !is.na(dictionary$distance_between)]
    if (unit_col %in% names(df) && distance_col %in% names(df)) {
      new_col_name <- create_standardized_col_name(distance_col, "_mt")
      df[[new_col_name]] <- convert_to_meters(df[[distance_col]], df[[unit_col]])
    }
  }
  
  return(df)
}


# Function to apply the desired aggregation
aggregate_data <- function(df, dict, farm_id_col, tab_suffix) {
  
  df <- convert_numeric_columns(df,dictionary)  # Assuming this function is defined to convert appropriate columns to numeric
  
  # Convert binary variables (Y -> 1, others -> 0)
  binary_vars <- dict %>% filter(!is.na(Binaria) & Binaria == 1) %>% pull(new)
  df <- df %>%
    mutate(across(all_of(binary_vars), ~ ifelse(. %in% c("Y", "y"), 1, 0)))
  
  # Conditionally standardize measurements before aggregation
  if (tab_suffix == "_typ") {
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
          return(max(.x, na.rm = TRUE))
        }
        if (!is.na(dict_row$Minimo) && dict_row$Minimo == 1) {
          return(min(.x, na.rm = TRUE))
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


