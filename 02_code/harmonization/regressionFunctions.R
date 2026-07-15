# Loading Packages
packageList <- c("kableExtra", "stargazer", "haven", "fixest","modelsummary")
lapply(packageList,require,character.only=TRUE)

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
  outcome_vars  <- var_dict[which(var_dict$type_column == 1), "new"] # 1 for outcome variables
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
      if (baseline_var_baseline %in% colnames(data)) {
        if (all(!is.na(baseline_var_row$logarithm) & baseline_var_row$logarithm == 1)) {
          data[baseline_var_baseline] <- data[baseline_var_baseline] + 1    #To avoid log(0) = -Inf
          formula_str <- paste(formula_str, "+log(", baseline_var_baseline,")")
        } else {
          formula_str <- paste(formula_str, "+", baseline_var_baseline)
        }
      }
    }
    
    model_formula <- as.formula(formula_str)
    outcome_var_cl <- paste0(outcome_var, "_cl")
    
    # Logistic regression (logit model) for binary outcomes
    if (all(data[[outcome_var]] %in% c(0, 1))) {
      ## Simple regression
      #logit_model    <- glm(model_formula, data = data, family = binomial(link = "logit"))
      #models_list[[outcome_var]]    <- logit_model
      ## Fixed effects
      logit_model_cl <- feglm(model_formula, data = data, cluster = ~communityOrHamlet_baseline)
      models_list[[outcome_var_cl]] <- logit_model_cl
      
    # Linear regression for continuous outcomes
    } else {
      ## Simple regression
      #linear_model <- lm(model_formula, data = data)
      #models_list[[outcome_var]] <- linear_model
      ## Fixed effects
      linear_model_cl <- feols(model_formula, data = data, cluster = ~communityOrHamlet_baseline)
      models_list[[outcome_var_cl]] <- linear_model_cl
    }
  }

  # Ceate a table for all models
  modelsummary(models_list, output = output_file,
    stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01),
    gof_map = c("nobs","adj.r.squared"),
    add_rows = bind_rows(
      
      ## Errors
      tibble(term = "Errors clustered",!!!setNames(
        lapply(names(models_list), function(m) {
        if (grepl("_cl$", m)) "Yes" else "No"}),names(models_list))),
      
      ## Controls
      tibble(term = "Controls",!!!setNames(
        rep(list("No"), length(models_list)),names(models_list)))
    ))

  return(models_list)
}
