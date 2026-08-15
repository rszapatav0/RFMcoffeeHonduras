#' ------------------------------------------------------------------------
#' Balance tests for RFM
#' Author:       Raquel Sofía
#' Creation:     August, 2026
#' Last edition: August, 2026
#' Editor:       Raquel Sofía
#' 
#' This code: Construct the balance tests for RFM.
#'            It uses the sample from the baseline, but assign the treatment
#'            based on the endline location (when observations didn't match
#'            for baseline and endline, it uses baseline location).
#' ------------------------------------------------------------------------


#' ------------------------------------------------------------------------
# Setting up the data and the dictionary ----------------------------------
#' ------------------------------------------------------------------------

# Loading Packages
packageList <- c("dplyr","RCT","kableExtra","nnet","tidyr")
lapply(packageList,require,character.only=TRUE)
rm(list=ls())


# Treatment assignment file
matching_columns <- c("communityOrHamlet", "municipality", "department")
ttmAssign <- read.csv("01_data/raw/implementation/ttmAssign.csv", stringsAsFactors = FALSE) %>%
  select(-sellsToIntermediary) %>%
  mutate(across(all_of(matching_columns), ~replace_na(., "")))


# Joining treatment with endline ids
endlineDF <- read.csv("01_data/processed/endline/endlineAgg.csv") %>%
  select(surveyID, all_of(matching_columns))
endlineDF <- endlineDF %>% 
  left_join(ttmAssign, by = c("communityOrHamlet", "municipality", "department")) %>%
  mutate(
    treatment = ifelse(is.na(treatment), "Control", treatment),
    treatment_eng = ifelse(is.na(treatment_eng), "Control", treatment_eng)
  ) %>%
  select(surveyID, treatment, treatment_eng, communityOrHamlet) %>%
  rename(communityOrHamlet_baseline = communityOrHamlet)
endlineDF$attrition <- 0

# Joining treatment with baseline ids
baselineDF <- read.csv("01_data/processed/baseline/baselineAgg.csv")
baselineDF <- left_join(baselineDF, ttmAssign,by = c("communityOrHamlet", "municipality", "department")) %>%
  mutate(
    treatment = ifelse(is.na(treatment), "Control", treatment),
    treatment_eng = ifelse(is.na(treatment_eng), "Control", treatment_eng)
  ) %>% rename(
    bl_treatment = treatment,
    bl_treatment_eng = treatment_eng
  )


# Endline treatment on baseline data
df <- left_join(baselineDF, endlineDF, by="surveyID")
df <- df %>% mutate(
  treatment = case_when(is.na(treatment) ~ bl_treatment,TRUE ~ treatment),
  treatment_eng = case_when(is.na(treatment_eng) ~ bl_treatment_eng,TRUE ~ treatment_eng)
  ) %>% select(-c(bl_treatment,bl_treatment_eng))
rm(list=c("baselineDF","endlineDF","ttmAssign"))
#' summary(df$treatment == df$bl_treatment) #FALSE=129(8%), TRUE=1481(92%)


# Final database for balance tests
df <- df %>%
  mutate(treatment = case_when(
    treatment == "Control" ~ "0",
    treatment == "Evaluacion de calidad" ~ "1",
    treatment == "Asistencia tecnica" ~ "2",
    treatment == "Asistencia tecnica y evaluacion de calidad" ~ "3",
    TRUE ~ treatment  # Keep any other values unchanged if they exist
  ))
colnames(df) <- gsub("\\.", "/", colnames(df))


# Parameters
## Treatment variable
treatment_var <- "treatment"
df[[treatment_var]] <- as.factor(df[[treatment_var]])
## Covariates
covariates <- c(
  "householdHasPipedWater","householdConnectedToElectricity","householdHasMobilePhone",
  "hasInternetAtHome","hasMigratedInThePastYear_pop","yearsExperienceInCoffeeFarming",
  "usesWhatsAppForMessages","registeredWithIHCAFE","adoptedRecommendedTechnologies",
  "totalAreaUsedForAgricultureInHa","coffeeAreaLastHarvestInHa",
  "amountSoldInKgGreen_typ" ,"minimumReceivedPriceForCoffeeInKgGreen_typ","maximumReceivedPriceForCoffeeInKgGreen_typ"
  )


# Missing test
missTest <- df %>%
  group_by(treatment) %>%
  summarise(across(all_of(covariates), ~ mean(is.na(.))))


# Keeping observations without missings
#df <- df[complete.cases(df[, c(treatment_var, covariates)]), ]


# For attrition
df_attrition <- df %>% mutate(attrited = is.na(attrition))


# Function to add stars
add_stars <- function(p) {
  paste0(format(round(p, 3), nsmall = 3),
    ifelse(p < 0.001, "***",
           ifelse(p < 0.01, "**",
                  ifelse(p < 0.05, "*", ""))))
}



#' ------------------------------------------------------------------------
# Per-covariate tests -----------------------------------------------------
#' ------------------------------------------------------------------------

# Datasets
df_main  <- df
df_rc    <- df %>% filter(!communityOrHamlet_baseline %in% c("nyork","zorca"))
datasets <- list(
  main = df_main,
  rc = df_rc
)


# Loop by data
for (name in names(datasets)) {
  
  df <- datasets[[name]]

  ## Aggregate test for all treatments --------------------------------------
  #' All tests through ANOVA. (Chi-squared should be used for categorical)
  balance_rows_agg <- list()
  for (var in covariates) {
    fit   <- aov(df[[var]] ~ df[[treatment_var]])
    p_val <- summary(fit)[[1]][["Pr(>F)"]][1]
    n_obs <- sum(!is.na(df[[var]]))
    balance_rows_agg[[var]] <- data.frame(
      N = n_obs, Variable = var, p_value = add_stars(p_val))
  }
  balance_table_agg <- bind_rows(balance_rows_agg)
  
  
  ## Each treatment compared with control ----------------------------------
  df_bt <- df %>% select(treatment_var, all_of(covariates))
  balance_table_ind <- balance_table(df_bt, treatment_var)
  
  # Adding row for the number of observations
  n_row <- data.frame(
    variables1 = "Number of observations",
    Media_control1 = sum(df[[treatment_var]] == 0, na.rm = TRUE),
    Media_trat1    = sum(df[[treatment_var]] == 1, na.rm = TRUE),
    Media_trat2    = sum(df[[treatment_var]] == 2, na.rm = TRUE),
    Media_trat3    = sum(df[[treatment_var]] == 3, na.rm = TRUE),
    p_value1 = NA,
    p_value2 = NA,
    p_value3 = NA
  )
  balance_table_ind <- rbind(n_row, balance_table_ind)
  
  # Formating 
  ## p values
  p_cols <- grep("^p_value", names(balance_table_ind))
  balance_table_ind[p_cols] <- lapply(balance_table_ind[p_cols], add_stars)
  ## Media
  media_cols <- grep("^Media_", names(balance_table_ind))
  balance_table_ind[media_cols] <- lapply(
    balance_table_ind[media_cols],
    \(x) round(x, 3))
  ## Renaming
  balance_table_ind <- balance_table_ind %>% rename(
    Variable=variables1,
    Control = Media_control1,
    `QE` = Media_trat1,
    `TA` = Media_trat2,
    `QE+TA` = Media_trat3,
    `p (Control vs QE)` = p_value1,
    `p (Control vs TA)` = p_value2,
    `p (Control vs QE+TA)` = p_value3
  )
  
  
  ## Merging and saving ---------------------------------------------------
  # Merging
  balance_table <- full_join(balance_table_ind, balance_table_agg, by="Variable")
  ## Reordering
  balance_table <- balance_table %>% select(Variable, N, everything())

  
  # Saving complete table
  ## HTML
  balance_table %>%
    kbl(caption = "Balance Test Across Treatment Arms") %>%
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
    footnote(general = "*** p<0.01, ** p<0.05, * p<0.10. 
             QE: Quality Evaluation, TA: Technical Assistance.", 
             general_title = "Note:") %>%
    save_kable(paste0("03_tables/baseline/balance_table_app_",name,".html"))

  
  # Saving subtable
  ## HTML
  # balance_table %>%
  #   select(c(Variable, N, Control, QE, TA, `QE+TA`, p_value)) %>%
  #   kbl(caption = "Balance Test Across Treatment Arms") %>%
  #   kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
  #   footnote(general = "*** p<0.01, ** p<0.05, * p<0.10. 
  #            QE: Quality Evaluation, TA: Technical Assistance.", 
  #            general_title = "Note:") %>%
  #   save_kable(paste0("03_tables/baseline/balance_table_",name,".html"))

}


#' ------------------------------------------------------------------------
# Joint test --------------------------------------------------------------
#' ------------------------------------------------------------------------

# formula_str <- paste(treatment_var, "~", paste(covariates, collapse = " + "))
# full_model <- multinom(as.formula(formula_str), data = df, trace = FALSE)
# null_model <- multinom(as.formula(paste(treatment_var, "~ 1")), data = df, trace = FALSE)
# 
# joint_test <- anova(null_model, full_model)
# print(joint_test)
# cat("\nIf this p-value is > 0.05, covariates do not jointly predict",
#     "treatment assignment -- consistent with successful randomization.\n\n")



#' ------------------------------------------------------------------------
# Attrition --------------------------------------------------------------
#' ------------------------------------------------------------------------

# Attrition rates
rates <- df_attrition %>%
  group_by(treatment) %>%
  summarise(
    attrition = mean(attrited),
    n = n(),
    .groups = "drop"
  )

# P-values: each treatment vs control
pvals <- sapply(1:3, function(t) {
  x <- df_attrition$attrited[df_attrition$treatment == t]
  y <- df_attrition$attrited[df_attrition$treatment == 0]
  
  prop.test(
    c(sum(x), sum(y)),
    c(length(x), length(y))
  )$p.value
})

pval_stars <- function(p) {
  ifelse(
    p < 0.01, "***",
    ifelse(p < 0.05, "**",
           ifelse(p < 0.10, "*", ""))
  )
}

attrition_table <- tibble(
  Statistic = c("Share attrition", "p-value (Control vs Treatment)", "Number of observations"),
  Control = c(
    round(rates$attrition[rates$treatment == 0],3),
    "-",
    rates$n[rates$treatment == 0]
  ),
  `Quality Evaluation (QE)` = c(
    round(rates$attrition[rates$treatment == 1],3),
    paste0(sprintf("%.3f", pvals[1]), pval_stars(pvals[1])),
    rates$n[rates$treatment == 1]
  ),
  `Technical Assistance (TA)` = c(
    round(rates$attrition[rates$treatment == 2],3),
    paste0(sprintf("%.3f", pvals[2]), pval_stars(pvals[2])),
    rates$n[rates$treatment == 2]
  ),
  `QE + TA` = c(
    round(rates$attrition[rates$treatment == 3],3),
    paste0(sprintf("%.3f", pvals[3]), pval_stars(pvals[3])),
    rates$n[rates$treatment == 3]
  )
)

kable(
  attrition_table,
  format = "html",
  escape = FALSE,
  caption = "Attrition by Treatment Group"
) |>
  writeLines("03_tables/baseline/attrition_table.html")
