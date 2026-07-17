# Utility function to restore original column classes
restore_original_classes <- function(data, original_classes_bl, original_classes_el) {
  for (col in names(original_classes_bl)) {
    class_type <- original_classes_bl[[col]]
    
    # Restore the original column
    if (col %in% names(data)) {
      if (class_type == "numeric") {
        data[[col]] <- as.numeric(data[[col]])
      } else if (class_type == "integer") {
        data[[col]] <- as.integer(data[[col]])
      } else if (class_type == "factor") {
        data[[col]] <- as.factor(data[[col]])
      } else if (class_type == "logical") {
        data[[col]] <- as.logical(data[[col]])
      } else if (class_type == "character") {
        data[[col]] <- as.character(data[[col]])
      }
    }
    
    # Restore the corresponding _baseline column, if it exists
    baseline_col <- paste0(col, "_baseline")
    if (baseline_col %in% names(data)) {
      if (class_type == "numeric") {
        data[[baseline_col]] <- as.numeric(data[[baseline_col]])
      } else if (class_type == "integer") {
        data[[baseline_col]] <- as.integer(data[[baseline_col]])
      } else if (class_type == "factor") {
        data[[baseline_col]] <- as.factor(data[[baseline_col]])
      } else if (class_type == "logical") {
        data[[baseline_col]] <- as.logical(data[[baseline_col]])
      } else if (class_type == "character") {
        data[[baseline_col]] <- as.character(data[[baseline_col]])
      }
    }
  }
  
  for (col in names(data)[!(names(data) %in% names(original_classes_bl)) &
                          !(grepl("_baseline$", names(data))) &
                          (names(data) %in% names(original_classes_el))]) {
    class_type <- original_classes_el[[col]]
    if (col %in% names(data)) {
      if (class_type == "numeric") {
        data[[col]] <- as.numeric(data[[col]])
      } else if (class_type == "integer") {
        data[[col]] <- as.integer(data[[col]])
      } else if (class_type == "factor") {
        data[[col]] <- as.factor(data[[col]])
      } else if (class_type == "logical") {
        data[[col]] <- as.logical(data[[col]])
      } else if (class_type == "character") {
        data[[col]] <- as.character(data[[col]])
      }
    }
  }
  
  return(data)
}


# Main function with simplified class restoration
combine_datasets <- function(baseline_path, endline_path, id_column = "surveyID", dictionary_path, ttmAssign_path, combine_type = "long") {
  # Load necessary libraries
  library(dplyr)
  library(readxl)  # Load readxl for reading Excel files
  library(fuzzyjoin)  # For fuzzy matching
  library(tidyr)  # For pivot_wider
  
  # Read the CSV files and the dictionary from Excel
  baselineDF <- read.csv(baseline_path, stringsAsFactors = FALSE)
  endlineDF  <- read.csv(endline_path, stringsAsFactors = FALSE)
  dictionary <- read.csv(dictionary_path)
  ttmAssign  <- read.csv(ttmAssign_path, stringsAsFactors = FALSE)  # Read the ttmAssign CSV file
  
  # Replace all dots (.) with slashes (/) in the column names
  ## Note: this issue sometimes occurs when saving the ...Agg.csv's. 
  colnames(baselineDF) <- gsub("\\.", "/", colnames(baselineDF))
  colnames(endlineDF)  <- gsub("\\.", "/", colnames(endlineDF))
  
  # Keeping only cleaned variables
  ## Baseline
  bl_vars <- intersect(
    dictionary$new[!is.na(dictionary$joined_clean) & dictionary$baseline == 1],
    names(baselineDF))
  baselineDF <- baselineDF %>% select(all_of(bl_vars))
  ## Baseline
  el_vars <- intersect(
    dictionary$new[!is.na(dictionary$joined_clean) &dictionary$endline == 1],
    names(endlineDF))
  endlineDF <- endlineDF %>%  select(all_of(el_vars))
  
  # Save original column classes
  original_classes_bl <- sapply(baselineDF, class)
  original_classes_el <- sapply(endlineDF, class)

  # Join treatment information
  ## Replace NAs in columns used for matching with empty strings to avoid issues during fuzzy matching
  #matching_columns <- c("communityOrHamlet", "municipality", "sellsToIntermediary", "department")
  matching_columns <- c("communityOrHamlet", "municipality", "department")
  ## Ensure the endline columns exist
  existing_columns_endline  <- matching_columns[matching_columns %in% colnames(endlineDF)]
  
  if (length(existing_columns_endline) > 0) {
    # Replace NA values for baseline columns in combinedDF
    endlineDF <- endlineDF %>%
      mutate(across(all_of(existing_columns_endline), ~replace_na(., "")))
    
    # Replace NA values for matching columns in ttmAssign
    ttmAssign <- ttmAssign %>%
      select(-sellsToIntermediary) %>%
      mutate(across(all_of(matching_columns), ~replace_na(., "")))
    
    # Simple join
    endlineDF <- endlineDF %>% 
      left_join(ttmAssign,
                by = c("communityOrHamlet", "municipality", "department")) %>%
      mutate(
        treatment = ifelse(is.na(treatment), "Control", treatment),
        treatment_eng = ifelse(is.na(treatment_eng), "Control", treatment_eng)
      )
  }
  
  # Add time indicator and ensure it's character
  baselineDF <- baselineDF %>% mutate(time = "0")
  endlineDF  <- endlineDF  %>% mutate(time = "1")
  
  # Ensure matching column types by converting all columns to character type
  baselineDF <- baselineDF %>% mutate(across(everything(), as.character))
  endlineDF  <- endlineDF  %>% mutate(across(everything(), as.character))
  
  # Find common IDs present in both baseline and endline datasets
  #IDs only once
  unique_baseline_ids <- baselineDF %>% group_by(!!sym(id_column)) %>%
    filter(n() == 1) %>% pull(!!sym(id_column))
  unique_endline_ids  <- endlineDF  %>% group_by(!!sym(id_column)) %>%
    filter(n() == 1) %>% pull(!!sym(id_column))
  ## Common IDs
  common_ids <- intersect(unique_baseline_ids, unique_endline_ids)
  
  # Filter both datasets to keep only common IDs
  baselineDF <- baselineDF %>% filter(!!sym(id_column) %in% common_ids)
  endlineDF  <- endlineDF  %>% filter(!!sym(id_column) %in% common_ids)
  
  # Identify columns that matched between baseline and endline
  matchedColumns <- intersect(names(baselineDF), names(endlineDF))
  cat("Columns that matched between baseline and endline:\n", paste(matchedColumns, collapse = ", "), "\n")
  print(length(matchedColumns))
  
  # Identify unmatched columns and print them
  unmatchedBaselineCols <- setdiff(names(baselineDF), names(endlineDF))
  unmatchedEndlineCols <- setdiff(names(endlineDF), names(baselineDF))
  
  if (length(unmatchedBaselineCols) > 0) {
    cat("Columns in baseline not in endline:\n", paste(unmatchedBaselineCols, collapse = ", "), "\n")
    print(length(unmatchedBaselineCols))
  }
  
  if (length(unmatchedEndlineCols) > 0) {
    cat("Columns in endline not in baseline:\n", paste(unmatchedEndlineCols, collapse = ", "), "\n")
    print(length(unmatchedEndlineCols))
  }
  
  ### Handle wide or long format based on 'combine_type' parameter ###
  if (combine_type == "wide") {
    # Perform a left join on surveyID to get a wide dataset
    combinedDF <- baselineDF %>%
      rename_with(~ paste0(.x, "_baseline"),-all_of(c(
        id_column,"time"))) %>%
      left_join(endlineDF,by = id_column, suffix = c("_baseline", ""),
                relationship = "one-to-one")
    
    # Unifying location variables
    #combinedDF <- combinedDF %>%
      #mutate(department = coalesce(department, department_baseline))
  } else {
    # Combine datasets in long format (default behavior)
    combinedDF <- bind_rows(baselineDF, endlineDF)
  }
  
  ### Force-joining columns based on dictionary.csv ###
  
  # Get columns that need to be force-joined from the dictionary
  force_join_dict <- dictionary %>% filter(!is.na(force_join) & joined_clean==1)
  
  # Loop through each unique group in the force_join column
  if (combine_type == "long") {
    for (group in unique(force_join_dict$force_join)) {
      # Get the columns that need to be merged for this group
      columns_to_merge <- force_join_dict %>% filter(force_join == group) %>% pull(new)

      # Create a new column with the merged values (taking first non-NA value across the columns)
      combinedDF <- combinedDF %>%
        mutate(!!columns_to_merge[1] := coalesce(!!!syms(columns_to_merge)))

      # Drop the additional columns after merging (keep only the first one)
      combinedDF <- combinedDF %>%
        select(-all_of(columns_to_merge[-1]))
    }
    
    #Expanding treatment var
    combinedDF <- combinedDF %>%
      group_by(!!sym(id_column)) %>%
      mutate(treatment     = first(na.omit(treatment)),
             treatment_eng = first(na.omit(treatment_eng)) ) %>%  # Apply the same treatment to both time == 0 and time == 1
      ungroup()
    
  }


  # Restore the original classes of the columns
  combinedDF <- restore_original_classes(combinedDF, original_classes_bl, original_classes_el)
  return(combinedDF)
}
