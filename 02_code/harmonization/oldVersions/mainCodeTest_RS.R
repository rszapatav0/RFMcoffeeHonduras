
# combine_datasets --------------------------------------------------------
combine_datasets <- function(baseline_path, endline_path, id_column = "surveyID", dictionary_path, ttmAssign_path, combine_type = "long") {
  # Load necessary libraries
  library(dplyr)
  library(readxl)  # Load readxl for reading Excel files
  library(fuzzyjoin)  # For fuzzy matching
  library(tidyr)  # For pivot_wider
  
  id_column = "surveyID"
  dictionary_path <- var_dict_file
  ttmAssign_path <- ttm_file
  combine_type = "wide"
  
  
  # Read the CSV files and the dictionary from Excel
  baselineDF <- read.csv(baseline_path, stringsAsFactors = FALSE)
  endlineDF  <- read.csv(endline_path, stringsAsFactors = FALSE)
  dictionary <- read.csv(dictionary_path)  # Read the dictionary CSV file
  ttmAssign  <- read.csv(ttmAssign_path, stringsAsFactors = FALSE)  # Read the ttmAssign CSV file
  
  # Save original column classes
  original_classes_bl <- sapply(baselineDF, class)
  original_classes_el <- sapply(endlineDF, class)
  
  # Replace all dots (.) with slashes (/) in the column names
  ## Note: this issue sometimes occurs when saving the ...Agg.csv's. 
  colnames(baselineDF) <- gsub("\\.", "/", colnames(baselineDF))
  colnames(endlineDF) <- gsub("\\.", "/", colnames(endlineDF))
  
  # Join treatment information
  ## Replace NAs in columns used for matching with empty strings to avoid issues during fuzzy matching
  matching_columns <- c("communityOrHamlet", "municipality", "department")
  ## Ensure the baseline columns exist
  existing_columns_baseline <- matching_columns[matching_columns %in% colnames(baselineDF)]
  
  #if (length(existing_columns_baseline) > 0) {
    # Replace NA values for baseline columns in combinedDF
    baselineDF <- baselineDF %>%
      mutate(across(all_of(existing_columns_baseline), ~replace_na(., "")))
    
    # Replace NA values for matching columns in ttmAssign
    ttmAssign <- ttmAssign %>%
      mutate(across(all_of(matching_columns), ~replace_na(., "")))
    
    # Perform the fuzzy join on the baseline columns
    baselineDF <- baselineDF %>% 
      left_join(ttmAssign,
      by = c("communityOrHamlet", "municipality", "department"),
      #max_dist = 1,  # Set a maximum distance for fuzzy matching
      #distance_col = NULL # Store the distance of the match
    ) #%>%
      # Remove duplicate columns from the join
      #select(-ends_with(".y")) %>%  # Remove all columns that end with ".y" (i.e., from ttmAssign)
      #rename_with(~ gsub(".x$", "", .), ends_with(".x"))  # Remove ".x" suffix from the original columns
  #}
  
  # Add time indicator and ensure it's character
  baselineDF <- baselineDF %>%
    filter(agreesToSurveyParticipation == 1, !is.na(agreesToSurveyParticipation)) %>%
    mutate(time = "0")  # Add the time column
  endlineDF  <- endlineDF  %>%
    filter(agreesToSurveyParticipation == 1, !is.na(agreesToSurveyParticipation)) %>%
    mutate(time = "1")
  
  # Ensure matching column types by converting all columns to character type
  baselineDF <- baselineDF %>%
    mutate(across(everything(), as.character))
  endlineDF  <- endlineDF  %>%
    mutate(across(everything(), as.character))
  
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
  
  # Identify unmatched columns and print them
  unmatchedBaselineCols <- setdiff(names(baselineDF), names(endlineDF))
  unmatchedEndlineCols <- setdiff(names(endlineDF), names(baselineDF))
  
  if (length(unmatchedBaselineCols) > 0) {
    cat("Columns in baseline not in endline:\n", paste(unmatchedBaselineCols, collapse = ", "), "\n")
  }
  
  if (length(unmatchedEndlineCols) > 0) {
    cat("Columns in endline not in baseline:\n", paste(unmatchedEndlineCols, collapse = ", "), "\n")
  }
  
  ### Handle wide or long format based on 'combine_type' parameter ###
  if (combine_type == "wide") {
    # Perform a left join on surveyID to get a wide dataset
    combinedDF <- baselineDF %>%
      left_join(endlineDF, by = id_column, suffix = c("_baseline", ""),
                relationship = "many-to-many") #PENDING for duplicates in endline
  } else {
    # Combine datasets in long format (default behavior)
    combinedDF <- bind_rows(baselineDF, endlineDF)
  }
  
  ### NEW CODE: Force-joining columns based on dictionary.csv ###
  
  # Get columns that need to be force-joined from the dictionary
  force_join_dict <- dictionary %>% filter(!is.na(force_join))
  
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
    # } elseif (combine_type == "wide") {
    # # Loop through each unique group in the force_join column - wide
    #   force_join_names <- force_join_dict %>% pull(new)
    #   for (name in intersect(force_join_names, unmatchedBaselineCols)) {
    #     new_name <- paste0(name, "_baseline")
    #     colnames(combinedDF)[colnames(combinedDF) == name] <- new_name
    #   }
  }
  
  # Restore the original classes of the columns
  combinedDF <- restore_original_classes(combinedDF, original_classes_bl, original_classes_el)
  
  return(combinedDF)
}

###########################################

# restore_original_clases -------------------------------------------------
# Utility function to restore original column classes
restore_original_classes <- function(data, original_classes) {
  for (col in names(original_classes)) {
    class_type <- original_classes[[col]]
    
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
  return(data)
  }

longDF <- restore_original_classes(longDF, original_classes_bl, original_classes_el)
wideDF <- restore_original_classes(wideDF, original_classes_bl, original_classes_el)


###########################################

# run_regression_models ----------------------------------------------------

# Custom function for regression modeling and output
run_regression_models <- function(var_dict_file, data, output_file) {
  
  results <- run_regression_models(var_dict_file, wideDF, "model_summaries.html")
  
  wideDF_route <- "wideDF.csv"
  output_file <- "model_summaries.html"
  
  # Load the variable dictionary and wideDF
  var_dict <- read.csv(var_dict_file)
  data <- read.csv(wideDF_route)
  ## Change treatment definition
  data <- data %>%
    mutate(treatment = case_when(
      treatment == "Control" ~ "0",
      treatment == "Evaluacion de calidad" ~ "1",
      treatment == "Asistencia tecnica" ~ "2",
      treatment == "Asistencia tecnica y evaluacion de calidad" ~ "3",
      TRUE ~ treatment  # Keep any other values unchanged if they exist
    ))
  
  # Extracting outcome, treatment, and baseline variables based on the dictionary
  outcome_vars <- var_dict[which(var_dict$type_column == 1), "new"]  # 1 for outcome variables
  treatment_var <- var_dict[which(var_dict$type_column == 2), "new"] # 2 for treatment variable
  baseline_vars <- var_dict[which(var_dict$type_column == 3), "new"] # 3 for baseline variables
  
  # Initialize list to store models
  models_list <- list()
  
  # Loop through each outcome variable and estimate the model
  for (outcome_var in outcome_vars) {
    
    # Check if corresponding baseline variable for outcome exists
    outcome_baseline <- paste0(outcome_var, "_baseline")
    #if (!outcome_baseline %in% colnames(data)) {
    #  warning(paste("Baseline for outcome", outcome_var, "not found, skipping."))
    #  next
    #}
    
    # Use the treatment variable, independent variables, and their baselines in the formula
    ## Dependent var and its lag. Logarithmic transformation if necessary
    outcome_var_row <- var_dict[var_dict$new == outcome_var, ]
    if (all(!is.na(outcome_var_row$logarithm) & outcome_var_row$logarithm == 1)) {
      data[outcome_var]      <- data[outcome_var]      + 1    #To avoid log(0) = -Inf
      data[outcome_baseline] <- data[outcome_baseline] + 1    #To avoid log(0) = -Inf
      formula_str <- paste("log(", outcome_var, ") ~", treatment_var, "+ log(", outcome_baseline, ")")
    } else {
      formula_str <- paste(outcome_var, "~", treatment_var, "+", outcome_baseline)
    }
    ## Check if baseline_var's and baseline_var_baseline's exists in the data 
    ## and if Logarithmic transformation is necessary
    for (baseline_var in baseline_vars) {
      baseline_var_row      <- var_dict[var_dict$new == baseline_var, ]
      baseline_var_baseline <- paste0(baseline_var, "_baseline")
      if (all(!is.na(baseline_var_row$logarithm) & baseline_var_row$logarithm == 1)) {
        if (baseline_var %in% colnames(data)) {
          data[baseline_var] <- data[baseline_var] + 1    #To avoid log(0) = -Inf
          formula_str <- paste(formula_str, "+log(", baseline_var,")")
        } 
        if (baseline_var_baseline %in% colnames(data)) {
          data[baseline_var_baseline] <- data[baseline_var_baseline] + 1    #To avoid log(0) = -Inf
          formula_str <- paste(formula_str, "+log(", baseline_var_baseline,")")
        }
      }  else {
        if (baseline_var %in% colnames(data)) {
          formula_str <- paste(formula_str, "+", baseline_var)
        }
        if (baseline_var_baseline %in% colnames(data)) {
          formula_str <- paste(formula_str, "+", baseline_var_baseline)
        }
      }
    }
    model_formula <- as.formula(formula_str)
    
    # Check if the outcome variable is binary (0/1), use logit if so, otherwise use linear regression
    if (all(data[[outcome_var]] %in% c(0, 1))) {
      # Logistic regression (logit model) for binary outcomes
      logit_model <- glm(model_formula, data = data, family = binomial(link = "logit"))
      models_list[[outcome_var]] <- logit_model
    } else {
      # Linear regression for continuous outcomes
      linear_model <- lm(model_formula, data = data)
      models_list[[outcome_var]] <- linear_model
      }
  }
  
  # Use stargazer to create a table for all models
  stargazer(models_list, type = "text", 
            title = "Regression Model Summaries",
            report = "vc*p", 
            star.cutoffs = c(0.1, 0.05, 0.01),
            out = output_file)
  
  return(models_list)
}


###########################################

# generate_plots ----------------------------------------------------------

library(ggplot2);library(dplyr);library(sf);library(scales);library(viridis); library(maps)

generate_plots <- function(var_dict_file, data_file, spatial_file, output_dir) {
  
  generate_plots(var_dict_file, "workingDF.csv", spatial_file, output_dir)
  
  # Load dictionary, data, and spatial data
  var_dict <- read.csv(var_dict_file)

  generate_plots <- function(var_dict_file, data_file, spatial_file, output_dir) {
    
    # Load dictionary, data, and spatial data
    var_dict <- read.csv(var_dict_file)
    #data <- read.csv(data_file)
    data <- longDF
    data <- data %>%
      mutate(treatment_eng = case_when(
        treatment_eng == "Quality evaluation" ~ "QE",
        treatment_eng == "Technical assistance" ~ "TA",
        treatment_eng == "Technical assistance and quality evaluation" ~ "QE + TA",
        TRUE ~ treatment_eng  # Keep the original value if no match
      )) %>%
      mutate(time = case_when(
        time == 0 ~ "Baseline",
        time == 1 ~ "Endline",
        TRUE ~ as.character(time)  # Keep the original value if no match
      ))
    
    #Load spatial data
    ## National
    countries <- world.cities %>% filter(country.etc == "Honduras")
    spatial_data_hnd <- st_as_sf(map("world", regions = "Honduras", fill = TRUE, plot = FALSE))
    ## Department
    spatial_data_dep <- st_read(spatial_data_dep)
    spatial_data_dep <- spatial_data_dep %>%
      rename(department_name = ADM1_ES)
    ## Municipality
    spatial_data_mun <- st_read(spatial_data_mun)
    spatial_data_mun <- spatial_data_mun %>%
      rename(department_name = ADM1_ES,
             municipality_name = ADM2_REF)
    ## Community
    spatial_data_com <- st_read(spatial_data_com) # For choropleth maps
    spatial_data_com <- st_transform(spatial_data_com, crs = 4326) #Adjusting coordenates
    coords <- st_coordinates(spatial_data_com)
    
    # Extract variables for each plot type from the dictionary
    bar_avg_vars    <- var_dict[which(var_dict$bar_plot_avg == 1), "new"]
    bar_cnt_vars    <- var_dict[which(var_dict$bar_plot_cnt == 1), "new"]
    line_vars_y     <- var_dict[which(var_dict$line_plot_y == 1), "new"]
    line_vars_x     <- var_dict[which(var_dict$line_plot_x == 1), "new"]
    donut_vars      <- var_dict[which(var_dict$donut_plot == 1), "new"]
    choropleth_vars <- var_dict[which(var_dict$choropleth_plot == 1), "new"]
    grouping_vars   <- var_dict[which(var_dict$grouping_var == 1), "new"]
    
    
    
    var <- bar_avg_vars[[1]]
    grouping_var <- grouping_vars
    # Generate grouped bar plots for averages
    for (var in bar_avg_vars) {
      
      ## Grouped graphs
      if (!is.null(grouping_vars) && any(grouping_vars != "")) {
        for (grouping_var in grouping_vars) {
          ### Average by treatment and grouping_var
          summary_data <- data %>%
            mutate(!!sym(grouping_var) := as.factor(!!sym(grouping_var))) %>%  # Convert grouping_var column to factor
            filter(!is.na(treatment_eng) & !is.na(!!sym(grouping_var))) %>%     # To avoid NA's 
            group_by(treatment_eng, !!sym(grouping_var)) %>%
            summarise(
              mean_var = mean(get(var), na.rm = TRUE),              # Promedio
              se = sd(get(var), na.rm = TRUE) / sqrt(n()),          # Error estándar
              ci_lower = mean_var - qt(0.975, df = n() - 1) * se,   # Límite inferior del IC 95%
              ci_upper = mean_var + qt(0.975, df = n() - 1) * se    # Límite superior del IC 95%
            )
          ### Grouped bar plot
          p <- ggplot(summary_data, aes_string(x = "treatment_eng", y= "mean_var", fill = grouping_var)) +
            geom_col(position = "dodge") +
            geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(width = 0.9), width = 0.25) +
            scale_fill_viridis_d(option = "C") +  # Viridis color for categorical variables
            labs(title = paste("Grouped Bar Plot of", var, "\nby", grouping_var), 
                 x = "Treatment", y = "Average",
                 caption = "QE: Quality Evaluation. TA: Technical assitance.") +
            theme_minimal() +
            theme(legend.position = "bottom") +
            guides(fill = guide_legend(title = NULL))
          #plot.title = element_text(size = 12), 
          ### Save plot
          ggsave(filename = paste0(output_dir, "/", var, "_", grouping_var, "_bar_plot_avg.png"), plot = p)
        }
      }
      
      ## General graph.
      ### Average by treatment
      summary_data <- data %>% 
        filter(!is.na(treatment_eng)) %>%     # To avoid NA's 
        group_by(treatment_eng) %>%
        summarise(
          mean_var = mean(get(var), na.rm = TRUE),              # Promedio
          se = sd(get(var), na.rm = TRUE) / sqrt(n()),          # Error estándar
          ci_lower = mean_var - qt(0.975, df = n() - 1) * se,   # Límite inferior del IC 95%
          ci_upper = mean_var + qt(0.975, df = n() - 1) * se    # Límite superior del IC 95%
        )
      ### Regular bar plot
      p <- ggplot(summary_data, aes_string(x = "treatment_eng", y = "mean_var", fill = "treatment_eng")) +
        geom_col() +
        geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) + 
        scale_fill_viridis_d(option = "C") +  # Viridis color for the regular bar plot
        labs(title = paste("Bar Plot of", var), 
             x = "Treatment", y = "Average",
             caption = "QE: Quality Evaluation. TA: Technical assitance.") +
        theme_minimal() +
        theme(legend.position = "none")
      ### Save plot
      ggsave(filename = paste0(output_dir, "/", var, "_bar_plot_avg.png"), plot = p)
    }
    
    
    
    # Generate grouped bar plots for sum
    for (var in bar_cnt_vars) {
      
      ## Grouped graphs
      if (!is.null(grouping_vars) && any(grouping_vars != "")) {
        for (grouping_var in grouping_vars) {
          ### Count by treatment and grouping_var
          summary_data <- data %>%
            mutate(!!sym(grouping_var) := as.factor(!!sym(grouping_var))) %>%  # Convert grouping_var column to factor
            filter(!is.na(treatment_eng) & !is.na(!!sym(grouping_var))) %>%     # To avoid NA's 
            group_by(treatment_eng, !!sym(grouping_var)) %>%
            summarise(mean_var = sum(get(var), na.rm = TRUE))
          ### Grouped bar plot
          p <- ggplot(summary_data, aes_string(x = "treatment_eng", y= "mean_var", fill = grouping_var)) +
            geom_col(position = "dodge") +
            scale_fill_viridis_d(option = "C") +  # Viridis color for categorical variables
            labs(title = paste("Grouped Bar Plot of", var, "\nby", grouping_var), 
                 x = "Treatment", y = "Count",
                 caption = "QE: Quality Evaluation. TA: Technical assitance.") +
            theme_minimal() +
            theme(legend.position = "bottom") +
            guides(fill = guide_legend(title = NULL))
          #plot.title = element_text(size = 12), 
          ### Save plot
          ggsave(filename = paste0(output_dir, "/", var, "_", grouping_var, "_bar_plot_cnt.png"), plot = p)
        }
      }
      
      ## General graph.
      ### Count by treatment
      summary_data <- data %>% 
        filter(!is.na(treatment_eng)) %>%     # To avoid NA's 
        group_by(treatment_eng) %>%
        summarise(mean_var = sum(get(var), na.rm = TRUE))
      ### Regular bar plot
      p <- ggplot(summary_data, aes_string(x = "treatment_eng", y = "mean_var", fill = "treatment_eng")) +
        geom_col() +
        scale_fill_viridis_d(option = "C") +  # Viridis color for the regular bar plot
        labs(title = paste("Bar Plot of", var), 
             x = "Treatment", y = "Average",
             caption = "QE: Quality Evaluation. TA: Technical assitance.") +
        theme_minimal() +
        theme(legend.position = "none")
      ### Save plot
      ggsave(filename = paste0(output_dir, "/", var, "_bar_plot_cnt.png"), plot = p)
    }
    
    
    
    # Generate grouped line plots (assuming time series or continuous variables)
    for (line_var_y in line_vars_y) {
      for (line_var_x in line_vars_x) {
        
        ## Grouped graphs
        if (!is.null(grouping_vars) && any(grouping_vars != "")) {
          for (grouping_var in grouping_vars) {
            ### Grouped line plot
            p <- ggplot(data, aes_string(x = line_var_x, y = line_var_y, color = grouping_var)) +
              geom_line() +
              #scale_color_viridis_d(option = "C") +  # Viridis color for the grouped line plot
              labs(title = paste("Grouped Line Plot of", line_var_y, "\nover", line_var_x,", by", grouping_var),
                   x = line_var_x, y = line_var_y) +
              theme_minimal() +
              theme(legend.position = "bottom")
            ### Save plot
            ggsave(filename = paste0(output_dir, "/", line_var_y, "Over", line_var_x, "_", grouping_var, "_line_plot.png"), plot = p)
          }
        }
        
        ## General graph.
        ### Regular line plot
        p <- ggplot(data, aes_string(x = line_var_x, y = line_var_y)) +
          geom_line() +
          labs(title = paste("Line Plot of", line_var_y, "\nover", line_var_x), x = line_var_x, y = line_var_y) +
          theme_minimal()
        ### Save plot
        ggsave(filename = paste0(output_dir, "/", var, "_line_plot.png"), plot = p)
      }
    }
    
    
    
    # Generate donut charts (for categorical variables)
    for (var in donut_vars) {
      ## Collapsing data
      data_donut <- data %>%
        mutate(!!sym(var) := as.factor(!!sym(var))) %>%  # Convert var column to factor
        filter(!is.na(!!sym(var))) %>%     #To avoid NAs
        group_by_at(var) %>%
        summarise(count = n()) %>%
        mutate(percentage = count / sum(count))
      ## Donut plot
      p <- ggplot(data_donut, aes(x = 2, y = percentage, fill = !!sym(var))) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        scale_fill_viridis_d(option = "C") +  # Viridis color for donut chart
        xlim(0.5, 2.5) +
        theme_void() +
        theme(legend.title = element_blank()) +
        labs(title = paste("Donut Chart of", var))
      ## Save plot
      ggsave(filename = paste0(output_dir, "/", var, "_donut_chart.png"), plot = p)
    }
    
  }
  
  choropleth_vars <- "amountOfCoffeeProducedLastHarvestInKgGreen"
  var <- "amountOfCoffeeProducedLastHarvestInKgGreen"
  
  ### Loading spatial data
  #### National
  library(maps)
  countries <- world.cities %>% filter(country.etc == "Honduras")
  spatial_data_hnd <- st_as_sf(map("world", regions = "Honduras", fill = TRUE, plot = FALSE))
  #### Department
  spatial_data_dep <- st_read("./Maps/hnd_adm_sinit_20161005_SHP/hnd_admbnda_adm1_sinit_20161005.shp")
  spatial_data_dep <- spatial_data_dep %>%
    rename(department_name = ADM1_ES)
  #### Municipality
  spatial_data_mun <- st_read("./Maps/hnd_adm_sinit_20161005_SHP/hnd_admbnda_adm2_sinit_20161005.shp")
  spatial_data_mun <- spatial_data_mun %>%
    rename(department_name = ADM1_ES,
           municipality_name = ADM2_REF)
  #### Community
  spatial_data_com <- st_read(spatial_data_com) # For choropleth maps
  spatial_data_com <- st_transform(spatial_data_com, crs = 4326)
  coords <- st_coordinates(spatial_data_com)
  
  
  
  ### Generate choropleth maps (assuming geographic data)
  for (var in choropleth_vars) {
    ## Map by departments
    ### Average by department
    summary_data <- data %>%
      filter(!is.na(department_name)) %>%     # To avoid NA's 
      group_by(department_name) %>%
      summarise(mean_var = mean(!!sym(var), na.rm = TRUE))
    ### Combining data
    merged_data <- spatial_data_dep %>%
      left_join(summary_data, by = "department_name")
    ### Choroplet
    p <- ggplot(data = merged_data) +
      geom_sf(aes(fill = mean_var), color = "black", alpha = 0.6) +
      labs(title = paste("Mapa de", var, "por departamento"), fill = "") +
      theme_minimal() +
      scale_fill_gradientn(colors = viridis(256), na.value = "white") + 
      theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
    ### Save plot
    ggsave(filename = paste0(output_dir, "/", var, "_choropleth_map_department.png"), plot = p)    
    
    ## May by municipality
    ### Average by municipality
    summary_data <- data %>%
      filter(!is.na(municipality_name)) %>%     # To avoid NA's 
      group_by(municipality_name, department_name) %>%
      summarise(mean_var = mean(!!sym(var), na.rm = TRUE))
    ### Combining data
    merged_data <- stringdist_left_join(
      spatial_data_mun, summary_data,
      by = c("municipality_name", "department_name"),
      max_dist = 1,  # Set a maximum distance for fuzzy matching
      distance_col = NULL
    ) %>% # Remove duplicate columns from the join
      select(-ends_with(".y")) %>%  # Remove all columns that end with ".y" (i.e., from ttmAssign)
      rename_with(~ gsub(".x$", "", .), ends_with(".x")) # Remove ".x" suffix from the original columns
      #mutate(municipality_name = if_else(is.na(mean_var), NA_character_, municipality_name))
      ### Choroplet
    p <- ggplot(data = merged_data) +
      geom_sf(aes(fill = mean_var, geometry = geometry), color = "black", alpha = 0.6) +
      #geom_sf_text(aes(label = municipality_name, geometry = geometry), size = 2, color = "black") +
      labs(title = paste("Mapa de", var, "por municipio"), fill = "") +
      theme_minimal() +
      scale_fill_gradientn(colors = viridis(256), na.value = "white") + 
      theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
    ### Save plot
    ggsave(filename = paste0(output_dir, "/", var, "_choropleth_map_municipality.png"), plot = p)
    
    ## Map by community
    ### Average by community
    summary_data <- data %>%
      filter(!is.na(communityOrHamlet_name)) %>%
      group_by(communityOrHamlet_name, municipality_name, department_name) %>%
      summarise(mean_var = mean(!!sym(var), na.rm = TRUE)) %>%
      mutate(communityOrHamlet_name = recode(communityOrHamlet_name,
                                             "Aguacatal" = "El Aguacatal",
                                             "Macuzal"   = "El Macuzal"))
    ### Creating data: coordenates, location names, mean_var
    spatial_data_com_new <- data.frame(
      long = coords[, 1],
      lat = coords[, 2],
      communityOrHamlet_name = spatial_data_com$CASERIO,
      municipality_name = spatial_data_com$Municipio,
      department_name = spatial_data_com$Departamen) %>%
      filter(!is.na(communityOrHamlet_name) & !is.na(municipality_name) & !is.na(department_name)) #Se van alredor de 5000 comunidades sin coordenadas
    ### Combining data
    merged_data <- stringdist_left_join(
      spatial_data_com_new, summary_data,
      by = c("communityOrHamlet_name", "municipality_name", "department_name"),
      max_dist = 1,  # Set a maximum distance for fuzzy matching
      distance_col = NULL
    ) %>% # Remove duplicate columns from the join
      select(-ends_with(".y")) %>%  # Remove all columns that end with ".y" (i.e., from ttmAssign)
      rename_with(~ gsub(".x$", "", .), ends_with(".x")) %>% # Remove ".x" suffix from the original columns
      filter(!is.na(mean_var)) # To avoid other points.
    ### Choroplet
    p <- ggplot() +
      geom_sf(data = spatial_data_hnd, fill = "white", color = "black", alpha = 0.5) +  # Mapa de Honduras
      geom_point(data = merged_data, aes(x = long, y = lat, color = mean_var), size = 2, alpha = 0.7) +  # Puntos del df_basic
      scale_color_viridis() +
      labs(title = paste("Mapa de", var, "por comunidad"), color = "") +
      theme_minimal() +
      theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
    ### Save plot
    ggsave(filename = paste0(output_dir, "/", var, "_choropleth_map_community.png"), plot = p)
    
  }




