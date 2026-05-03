library(kableExtra)
library(stargazer)
library(haven)

# Function to add significance asterisks
add_significance_asterisks <- function(p_value) {
  if (p_value < 0.01) {            #if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.05) {     #} else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.1) {      #} else if (p_value < 0.05) {
    return("*")
  } else {
    return("")
  }
}

# Custom function for regression modeling and output
run_regression_models <- function(var_dict_file, data, output_file) {
  
  # Load the variable dictionary and wideDF
  var_dict <- read.csv(var_dict_file)
  #data     <- read.csv(wideDF)
  data     <- data %>%
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
    if (!outcome_baseline %in% colnames(data)) {
      warning(paste("Baseline for outcome", outcome_var, "not found, skipping."))
      next
    }
    
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
