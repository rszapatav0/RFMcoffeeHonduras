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


# # Main function with simplified class restoration
# combine_datasets <- function(baseline_path, endline_path, id_column = "surveyID", dictionary_path, ttmAssign_path, combine_type = "long") {
#   # Load necessary libraries
#   library(dplyr)
#   library(readxl)  # Load readxl for reading Excel files
#   library(fuzzyjoin)  # For fuzzy matching
#   library(tidyr)  # For pivot_wider
#   
#   # Read the CSV files and the dictionary from Excel
#   baselineDF <- read.csv(baseline_path, stringsAsFactors = FALSE)
#   endlineDF  <- read.csv(endline_path, stringsAsFactors = FALSE)
#   dictionary <- read.csv(dictionary_path)  # Read the dictionary CSV file
#   ttmAssign  <- read.csv(ttmAssign_path, stringsAsFactors = FALSE)  # Read the ttmAssign CSV file
#   
#   # Save original column classes
#   original_classes_bl <- sapply(baselineDF, class)
#   original_classes_el <- sapply(endlineDF, class)
#   
#   # Replace all dots (.) with slashes (/) in the column names
#   ## Note: this issue sometimes occurs when saving the ...Agg.csv's. 
#   colnames(baselineDF) <- gsub("\\.", "/", colnames(baselineDF))
#   colnames(endlineDF) <- gsub("\\.", "/", colnames(endlineDF))
#   
#   # Join treatment information
#   ## Replace NAs in columns used for matching with empty strings to avoid issues during fuzzy matching
#   #matching_columns <- c("communityOrHamlet", "municipality", "sellsToIntermediary", "department")
#   matching_columns <- c("communityOrHamlet", "municipality", "department")
#   ## Ensure the baseline columns exist
#   existing_columns_baseline <- matching_columns[matching_columns %in% colnames(baselineDF)]
#   
#   if (length(existing_columns_baseline) > 0) {
#     # Replace NA values for baseline columns in combinedDF
#     baselineDF <- baselineDF %>%
#       mutate(across(all_of(existing_columns_baseline), ~replace_na(., "")))
#     
#     # Replace NA values for matching columns in ttmAssign
#     ttmAssign <- ttmAssign %>%
#       mutate(across(all_of(matching_columns), ~replace_na(., "")))
#     
#     # Perform the fuzzy join on the baseline columns
#     # baselineDF <- stringdist_left_join(
#     #   baselineDF, ttmAssign,
#     #   #by = c("communityOrHamlet", "municipality", "sellsToIntermediary", "department"),
#     #   by = c("communityOrHamlet", "municipality", "department"),
#     #   max_dist = 1,  # Set a maximum distance for fuzzy matching
#     #   distance_col = NULL
#     # ) %>%
#     #   # Remove duplicate columns from the join
#     #   select(-ends_with(".y")) %>%  # Remove all columns that end with ".y" (i.e., from ttmAssign)
#     #   rename_with(~ gsub(".x$", "", .), ends_with(".x"))  # Remove ".x" suffix from the original columns
#     
#     # Simple join
#     baselineDF <- baselineDF %>% 
#       left_join(ttmAssign,
#                 by = c("communityOrHamlet", "municipality", "department"))
#   }
#   
#   # Add time indicator and ensure it's character
#   baselineDF <- baselineDF 
#   endlineDF  <- endlineDF  
# 
#   # Ensure matching column types by converting all columns to character type
#   baselineDF <- baselineDF %>%
#     mutate(across(everything(), as.character))
#   endlineDF  <- endlineDF  %>%
#     mutate(across(everything(), as.character))
#   
#   # Find common IDs present in both baseline and endline datasets
#   #IDs only once
#   unique_baseline_ids <- baselineDF %>% group_by(!!sym(id_column)) %>%
#     filter(n() == 1) %>% pull(!!sym(id_column))
#   unique_endline_ids  <- endlineDF  %>% group_by(!!sym(id_column)) %>%
#     filter(n() == 1) %>% pull(!!sym(id_column))
#   ## Common IDs
#   common_ids <- intersect(unique_baseline_ids, unique_endline_ids)
#   
#   # Filter both datasets to keep only common IDs
#   baselineDF <- baselineDF %>% filter(!!sym(id_column) %in% common_ids)
#   endlineDF  <- endlineDF  %>% filter(!!sym(id_column) %in% common_ids)
#   
#   # Identify columns that matched between baseline and endline
#   matchedColumns <- intersect(names(baselineDF), names(endlineDF))
#   cat("Columns that matched between baseline and endline:\n", paste(matchedColumns, collapse = ", "), "\n")
# 
#   # Identify unmatched columns and print them
#   unmatchedBaselineCols <- setdiff(names(baselineDF), names(endlineDF))
#   unmatchedEndlineCols <- setdiff(names(endlineDF), names(baselineDF))
# 
#   if (length(unmatchedBaselineCols) > 0) {
#     cat("Columns in baseline not in endline:\n", paste(unmatchedBaselineCols, collapse = ", "), "\n")
#   }
# 
#   if (length(unmatchedEndlineCols) > 0) {
#     cat("Columns in endline not in baseline:\n", paste(unmatchedEndlineCols, collapse = ", "), "\n")
#   }
#   
#   ### Handle wide or long format based on 'combine_type' parameter ###
#   if (combine_type == "wide") {
#     # Perform a left join on surveyID to get a wide dataset
#     combinedDF <- baselineDF %>%
#       left_join(endlineDF, by = id_column, suffix = c("_baseline", ""),
#                 relationship = "many-to-many") #PENDING for duplicates in endline
#   } else {
#     # Combine datasets in long format (default behavior)
#     combinedDF <- bind_rows(baselineDF, endlineDF)
#   }
#   
#   
#   ### NEW CODE: Force-joining columns based on dictionary.csv ###
#   
#   # Get columns that need to be force-joined from the dictionary
#   force_join_dict <- dictionary %>% filter(!is.na(force_join))
#   
#   if (combine_type == "long") {
#     for (group in unique(force_join_dict$force_join)) {
#       # Get the columns that need to be merged for this group
#       columns_to_merge <- force_join_dict %>% filter(force_join == group) %>% pull(new)
#       
#       # Filter out missing columns from the dataset
#       existing_columns_to_merge <- columns_to_merge[columns_to_merge %in% names(combinedDF)]
#       
#       if (length(existing_columns_to_merge) > 0) {
#         # Create a new column with the merged values (taking first non-NA value across the columns)
#         combinedDF <- combinedDF %>%
#           mutate(!!existing_columns_to_merge[1] := coalesce(!!!syms(existing_columns_to_merge)))
#         
#         # Drop the additional columns after merging (keep only the first one)
#         combinedDF <- combinedDF %>%
#           select(-all_of(existing_columns_to_merge[-1]))
#       } else {
#         warning(paste("No existing columns found for group:", group))
#       }
#     }
#   }
# 
#   #Expanding treatment var
#   if (combine_type == "long") {
#     combinedDF <- combinedDF %>%
#       group_by(surveyID) %>%
#       mutate(treatment     = first(na.omit(treatment)),
#              treatment_eng = first(na.omit(treatment_eng)) ) %>%  # Apply the same treatment to both time == 0 and time == 1
#       ungroup()
#   }
#   
#   # Restore the original classes of the columns
#   combinedDF <- restore_original_classes(combinedDF, original_classes_bl, original_classes_el)
#   return(combinedDF)
# }

combine_datasets <- function(baseline_path, endline_path, id_column = "surveyID", var_dict_file, ttm_file, combine_type = "long") {
  # Load necessary libraries
  library(dplyr)
  library(readxl)
  
  # Read the CSV files and dictionary
  baselineDF <- read.csv(baseline_path, stringsAsFactors = FALSE)
  endlineDF <- read.csv(endline_path, stringsAsFactors = FALSE)
  var_dict <- read.csv(var_dict_file, stringsAsFactors = FALSE)  # Read the dictionary CSV file
  ttm <- read.csv(ttm_file, stringsAsFactors = FALSE)  # Read the ttm file
  
  # Standardize column names (replace dots with slashes)
  colnames(baselineDF) <- gsub("\\.", "/", colnames(baselineDF))
  colnames(endlineDF) <- gsub("\\.", "/", colnames(endlineDF))
  
  # Ensure column types match
  baselineDF <- baselineDF %>% mutate(across(everything(), as.character))
  endlineDF <- endlineDF %>% mutate(across(everything(), as.character))
  
  # Replace NA values for ttm matching columns with empty strings
  ttm_matching_cols <- c("communityOrHamlet", "municipality", "department")
  baselineDF <- baselineDF %>%
    mutate(across(all_of(ttm_matching_cols), ~replace_na(., "")))
  
  ttm <- ttm %>%
    mutate(across(all_of(ttm_matching_cols), ~replace_na(., "")))
  
  # Join baselineDF with ttm using left_join
  baselineDF <- baselineDF %>%
    left_join(ttm, by = ttm_matching_cols)
  
  # Find common columns and IDs
  matchedColumns <- intersect(names(baselineDF), names(endlineDF))
  cat("Matched columns:\n", paste(matchedColumns, collapse = ", "), "\n")
  
  common_ids <- intersect(baselineDF[[id_column]], endlineDF[[id_column]])
  
  # Filter to keep only matched columns and common IDs
  baselineDF <- baselineDF %>%
    select(all_of(matchedColumns)) %>%
    filter(!!sym(id_column) %in% common_ids)
  
  endlineDF <- endlineDF %>%
    select(all_of(matchedColumns)) %>%
    filter(!!sym(id_column) %in% common_ids)
  
  # Combine datasets based on the specified type
  if (combine_type == "wide") {
    combinedDF <- baselineDF %>%
      left_join(endlineDF, by = id_column, suffix = c("_baseline", "_endline"))
  } else {
    combinedDF <- bind_rows(
      baselineDF %>% mutate(time = "0"),
      endlineDF %>% mutate(time = "1")
    )
  }
  
  # Force-join columns based on var_dict_file (if applicable)
  force_join_dict <- var_dict %>% filter(!is.na(force_join))
  
  if (combine_type == "long") {
    for (group in unique(force_join_dict$force_join)) {
      # Get columns to merge for this group
      columns_to_merge <- force_join_dict %>% filter(force_join == group) %>% pull(new)
      
      # Filter out missing columns
      existing_columns <- columns_to_merge[columns_to_merge %in% names(combinedDF)]
      
      if (length(existing_columns) > 0) {
        # Merge columns by taking the first non-NA value
        combinedDF <- combinedDF %>%
          mutate(!!existing_columns[1] := coalesce(!!!syms(existing_columns))) %>%
          select(-all_of(existing_columns[-1]))  # Drop additional columns after merging
      }
    }
  }
  
  return(combinedDF)
}
