packageList <- c("dplyr","readxl","purrr","tidyr","stringr","tidyverse","fuzzyjoin",
                 "kableExtra","stargazer","haven","fixest","modelsummary","ggplot2")
lapply(packageList,require,character.only=TRUE)
rm(list=ls())

wide_file     <- "01_data/processed/harmonization/wideDF.csv"
data  <- read.csv(wide_file)
colnames(data) <- gsub("\\.", "/", colnames(data))
#data <- wideDF
data <- data %>% 
  mutate(treatment = case_when(
    treatment == "Control" ~ "0",
    treatment == "Evaluacion de calidad" ~ "1",
    treatment == "Asistencia tecnica" ~ "2",
    treatment == "Asistencia tecnica y evaluacion de calidad" ~ "3",
    TRUE ~ treatment  # Keep any other values unchanged if they exist
  ))


#' -----------------------------------------------------------------------------
#  REGRESSIONS  ----------------------------------------------------------------
#' -----------------------------------------------------------------------------

# Setting parameters
covariates <- c(
  "householdHasPipedWater","householdConnectedToElectricity","householdHasMobilePhone",
  "hasInternetAtHome","hasMigratedInThePastYear_pop","yearsExperienceInCoffeeFarming",
  "usesWhatsAppForMessages","registeredWithIHCAFE","adoptedRecommendedTechnologies",
  "totalAreaUsedForAgricultureInHa","coffeeAreaLastHarvestInHa"
  #"amountSoldInKgGreen_typ" ,"minimumReceivedPriceForCoffeeInKgGreen_typ","maximumReceivedPriceForCoffeeInKgGreen_typ"
)
covariates <- c(
  "householdHasPipedWater","householdHasMobilePhone","hasInternetAtHome","yearsExperienceInCoffeeFarming",
  "usesWhatsAppForMessages","registeredWithIHCAFE","coffeeAreaLastHarvestInHa")
controls <- paste0(" + ", paste0(covariates, "_baseline", collapse = " + "))


# Same sample
data <- data[complete.cases(data[, c(paste0(covariates, "_baseline"))]), ]
#' 5 observations deleted for missings on controls
 

# Missing observations
# data$missing_baseline <- !complete.cases(data[, paste0(covariates, "_baseline")])
# data_miss <- data %>% select(surveyID, missing_baseline, treatment) %>%
#   filter(missing_baseline==TRUE)


# Robustness: excluding communities
#data <- data %>% filter(!communityOrHamlet %in% c("nyork","zorca"))


table(data$treatment, data$rankingInterTop3,
      dnn = c("treatment", "trusted"))

length(unique(data$communityOrHamlet_name))
table(unique(data[, c("communityOrHamlet_name", "treatment")])$treatment)


#' -----------------------------------------------------------------------------
## Intermediary outcomes: Market outcomes --------------------------------------

### Full sample ----------------------------------------------------------------
#kg_sold_inter: extensive margin was affected
models1 <- list()
variables_m1_nu <- c(
  "numberDifferentBuyersLastHarvest","probSellInter","percSellInter") #"receivedQualityBonus_typ","receivedQualityDiscount_typ",

#Reg numerical
for (var in variables_m1_nu) {
  ## Controls = No; Interaction = No
  formula <- as.formula(paste0(var, " ~ ", var, "_baseline + treatment"))
  models1[[paste0(var,"_s1")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## Controls = Yes; Interaction = No
  formula <- as.formula(paste0(var, " ~ ", var,  "_baseline + treatment", controls))
  models1[[paste0(var,"_s2")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## Controls = Yes; Interaction = Yes
  formula <- as.formula(paste0(var, " ~ ", var,  "_baseline + treatment + rankingInterTop3 + treatment*rankingInterTop3", controls))
  models1[[paste0(var,"_s3")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
}


models2 <- list()
variables_m2_ln <- c(
  "amountSoldInKgGreen_typ","minimumReceivedPriceForCoffeeInKgGreen_typ",
  "maximumReceivedPriceForCoffeeInKgGreen_typ")
#Reg with ln
for (var in variables_m2_ln) {
  ## Controls = No; Interaction = No
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment"))
  models2[[paste0(var,"_s1")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## Controls = Yes; Interaction = No
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", controls))
  models2[[paste0(var,"_s2")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## Controls = Yes; Interaction = Yes
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment + rankingInterTop3 + treatment*rankingInterTop3", controls))
  models2[[paste0(var,"_s3")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
}


models3 <- list()
variables_m3_ln <- c(
  "amountSoldInKgGreenInter_typ","minimumReceivedPriceForCoffeeInKgGreenInter_typ",
  "maximumReceivedPriceForCoffeeInKgGreenInter_typ")
#Reg with ln
for (var in variables_m3_ln) {
  ## Controls = No; Interaction = No
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment"))
  models3[[paste0(var,"_s1")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## Controls = Yes; Interaction = No
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", controls))
  models3[[paste0(var,"_s2")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## Controls = Yes; Interaction = Yes
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment + rankingInterTop3 + treatment*rankingInterTop3", controls))
  models3[[paste0(var,"_s3")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
}



#' -----------------------------------------------------------------------------
## Intermediary outcomes: Adoption of practices --------------------------------


### Processing times with with always doers ------------------------------------
#' Intensive margin for those who already did it?
#' Not causal
models4 <- list()
# variables_m2 <- c(
#   "timeBetweenHarvestAndDelivery","timeBetweenHarvestAndDeliveryDiff","timeBetweenHarvestAndDeliveryDiffr",
#   "timeBetweenHarvestAndPulping","timeBetweenHarvestAndPulpingDiff","timeBetweenHarvestAndPulpingDiffr",
#   "timeBetweenPulpingAndWashing","timeBetweenPulpingAndWashingDiff","timeBetweenPulpingAndWashingDiffr",
#   "timeFromWashingToDryCoffee","timeFromWashingToDryCoffeeDiff","timeFromWashingToDryCoffeeDiffr")
variables_m4 <- c(
  "timeBetweenHarvestAndDeliveryDiffr","timeBetweenHarvestAndPulpingDiffr","timeBetweenPulpingAndWashingDiffr")
for (var in variables_m4) {
  ## Controls = No; Interaction = No
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment"))
  models4[[paste0(var,"_s1")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## Controls = Yes; Interaction = No
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", controls))
  models4[[paste0(var,"_s2")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## Controls = Yes; Interaction = Yes
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment + rankingInterTop3 + treatment*rankingInterTop3", controls))
  models4[[paste0(var,"_s3")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
}



#' -----------------------------------------------------------------------------
#  SAVING TABLES  ---------------------------------------------------------------
#' -----------------------------------------------------------------------------

# length_cluster <- length(variables_m1)

all_models <- list(
  models1 = models1,
  models2 = models2,
  models3 = models3,
  models4 = models4
  )

for (name in names(all_models)) {
  
  models <- all_models[[name]]
  modelsummary(models,
    output = paste0("03_tables/harmonization/", name, "_top3xTreat.html"),
    stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01),
    gof_map = c("nobs", "adj.r.squared"),
    add_rows = bind_rows(
      tibble(term = "Errors clustered",
             !!!setNames(rep(list("Yes"), length(models)), names(models))),
      tibble(term = "Controls",
             !!!setNames(rep(c("No","Yes","Yes"), length(models)/3), names(models))),
      tibble(term = "Trusted communities",
             !!!setNames(rep(c("No","No","Yes"), length(models)/3), names(models)))
    ),
    notes = c("treatment1 = Quality Evaluation (QE); treatment2 = Technical Assistance (TA); treatment3 = QE + TA"))
}





# =============================================================================
# Plot specification 3 (treatment x trusted community) for ALL THREE outcome
# groups in one pass, reusing models1 / models2 / models3 you already fit:
#   m1  variables_m1_nu   -> LEVELS:  y            ~ y_baseline           + ...
#   m2  variables_m2_ln   -> LOG:     log(y + 1)   ~ log(y_baseline + 1)  + ...
#   m3  variables_m3_ln   -> LOG:     log(y + 1)   ~ log(y_baseline + 1)  + ...
#
# One function (spec3_results()) drives the plotting loop across all three
# groups. Output: 9 PNGs (3 figures x 3 groups) in `figures/`.
# =============================================================================

# ---- 0. Settings --------------------------------------------------------------
trust_var  <- "rankingInterTop3"   # dummy: 1 = trusted community
level      <- 0.95                 # confidence level
as_percent <- FALSE                # TRUE -> for LOG groups only, plot 100*(exp(b)-1)
out_dir    <- "04_plots/harmonization/"
dir.create(out_dir, showWarnings = FALSE)

variables_m1_nu <- c(
  "numberDifferentBuyersLastHarvest", "probSellInter", "percSellInter")
variables_m2_ln <- c(
  "amountSoldInKgGreen_typ", "minimumReceivedPriceForCoffeeInKgGreen_typ",
  "maximumReceivedPriceForCoffeeInKgGreen_typ")
variables_m3_ln <- c(
  "amountSoldInKgGreenInter_typ", "minimumReceivedPriceForCoffeeInKgGreenInter_typ",
  "maximumReceivedPriceForCoffeeInKgGreenInter_typ")

# Optional: nicer facet titles. Anything not listed here falls back to an
# auto-generated label (camelCase -> "Title case words").
outcome_labels <- c(
  numberDifferentBuyersLastHarvest                          = "Number of different buyers (last harvest)",
  probSellInter                                              = "Probability of selling to project intermediary",
  percSellInter                                              = "% sold to project intermediary",
  amountSoldInKgGreen_typ                                    = "Amount sold (kg, green)",
  minimumReceivedPriceForCoffeeInKgGreen_typ                 = "Minimum price received (per kg)",
  maximumReceivedPriceForCoffeeInKgGreen_typ                 = "Maximum price received (per kg)",
  amountSoldInKgGreenInter_typ                               = "Amount sold to project intermediary (kg, green)",
  minimumReceivedPriceForCoffeeInKgGreenInter_typ            = "Minimum price from project intermediary (per kg)",
  maximumReceivedPriceForCoffeeInKgGreenInter_typ            = "Maximum price from project intermediary (per kg)"
)
humanize <- function(v) {
  s <- gsub("_typ$|_baseline$", "", v)
  s <- gsub("([a-z])([A-Z])", "\\1 \\2", s)
  paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
}
label_for <- function(v) if (v %in% names(outcome_labels)) outcome_labels[[v]] else humanize(v)

# One row per outcome group: name, variables, whether it's logged, and the
# models list you already built for it (models1/models2/models3, each keyed
# like models[[paste0(var, "_s3")]], same convention as your models2 loop).
specs <- list(
  m1 = list(vars = variables_m1_nu, log = FALSE, models = models1),
  m2 = list(vars = variables_m2_ln, log = TRUE,  models = models2),
  m3 = list(vars = variables_m3_ln, log = TRUE,  models = models3)
)

# ---- 1. Linear combinations for one spec-3 model ------------------------------
spec3_results <- function(m, ref_arm, trust_var, level) {
  b  <- coef(m)
  V  <- vcov(m)
  nm <- names(b)
  crit <- qt(1 - (1 - level) / 2, degrees_freedom(m, type = "t"))
  
  arm_terms <- nm[grepl("^treatment", nm) & !grepl(":", nm)]
  if (length(arm_terms) == 0)
    stop("No 'treatment<level>' coefficients found. Is `treatment` a factor?")
  if (!trust_var %in% nm)
    stop("Coefficient '", trust_var, "' not found. Is it a numeric 0/1 variable?")
  arms <- sub("^treatment", "", arm_terms)
  
  int_term <- function(t) {
    cand <- c(paste0(t, ":", trust_var), paste0(trust_var, ":", t))
    hit  <- cand[cand %in% nm]
    if (!length(hit)) stop("Interaction term not found for ", t)
    hit[1]
  }
  L <- function(terms = character(0)) {
    l <- setNames(numeric(length(b)), nm); l[terms] <- 1; l
  }
  lincom <- function(l) {
    est <- sum(l * b)
    se  <- sqrt(as.numeric(t(l) %*% V %*% l))
    tibble::tibble(est = est, se = se, lo = est - crit * se, hi = est + crit * se)
  }
  
  levels_df <- bind_rows(
    bind_cols(tibble::tibble(arm = ref_arm, trusted = 0), lincom(L())),
    bind_cols(tibble::tibble(arm = ref_arm, trusted = 1), lincom(L(trust_var))),
    map2_dfr(arm_terms, arms, function(t, a) bind_rows(
      bind_cols(tibble::tibble(arm = a, trusted = 0), lincom(L(t))),
      bind_cols(tibble::tibble(arm = a, trusted = 1), lincom(L(c(t, trust_var, int_term(t)))))
    ))
  )
  effects_df <- map2_dfr(arm_terms, arms, function(t, a) bind_rows(
    bind_cols(tibble::tibble(arm = a, trusted = 0), lincom(L(t))),
    bind_cols(tibble::tibble(arm = a, trusted = 1), lincom(L(c(t, int_term(t)))))
  ))
  interaction_df <- map2_dfr(arm_terms, arms, function(t, a)
    bind_cols(tibble::tibble(arm = a), lincom(L(int_term(t)))))
  
  list(levels = levels_df, effects = effects_df, interaction = interaction_df)
}

# ---- 2. Build the three figures for ONE group ---------------------------------
plot_group <- function(models, vars, group_name, uselog) {
  arm_order <- levels(factor(data$treatment))
  ref_arm   <- arm_order[1]
  
  res <- map(vars, function(v) spec3_results(models[[paste0(v, "_s3")]], ref_arm, trust_var, level))
  names(res) <- vars
  group_as_percent <- as_percent && uselog   # % transform only makes sense for logged outcomes
  trust_lab <- c("Untrusted community", "Trusted community")
  
  collect <- function(name) {
    bind_rows(map(res, name), .id = "outcome") %>%
      mutate(
        outcome = factor(outcome, levels = vars, labels = vapply(vars, label_for, "")),
        arm     = factor(arm, levels = arm_order)
      ) %>%
      { if ("trusted" %in% names(.))
        mutate(., trusted = factor(trusted, levels = c(0, 1), labels = trust_lab))
        else . } %>%
      { if (group_as_percent) mutate(., across(c(est, lo, hi), ~ 100 * (exp(.x) - 1))) else . }
  }
  
  levels_df      <- collect("levels")
  effects_df     <- collect("effects")
  interaction_df <- collect("interaction")
  
  ylab <- if (uselog) {
    if (group_as_percent) "% difference" else "Difference in ln(y + 1)"
  } else "Difference in y (levels)"
  ci_note <- paste0(round(level * 100), "% CIs, SEs clustered by community/hamlet. 0 = Control; 1 = Quality Evaluation (QE); 2 = Technical Assistance (TA); 3 = QE + TA.")
  pal   <- setNames(c("#440154", "#35b779"), trust_lab)
  shp   <- setNames(c(16, 17), trust_lab)
  dodge <- position_dodge(width = 0.55)
  
  base_theme <- theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", legend.title = element_blank(),
          panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          strip.text = element_text(face = "bold"), axis.title.x = element_blank())
  zero_line <- geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50")
  
  p1 <- ggplot(levels_df, aes(arm, est, colour = trusted, shape = trusted)) +
    zero_line +
    geom_pointrange(aes(ymin = lo, ymax = hi), position = dodge, linewidth = 0.6) +
    facet_wrap(~ outcome, nrow = 1, scales = "free_y") +
    scale_colour_manual(values = pal) + scale_shape_manual(values = shp) +
    labs(y = ylab,
         title = paste0("Outcome level by treatment arm and community trust"),
         subtitle = "Relative to untreated x untrusted (omitted category, = 0)",
         caption = ci_note) +
    base_theme
  
  # p2 <- ggplot(effects_df, aes(arm, est, colour = trusted, shape = trusted)) +
  #   zero_line +
  #   geom_pointrange(aes(ymin = lo, ymax = hi), position = dodge, linewidth = 0.6) +
  #   facet_wrap(~ outcome, nrow = 1, scales = "free_y") +
  #   scale_colour_manual(values = pal) + scale_shape_manual(values = shp) +
  #   labs(y = ylab,
  #        title = paste0(group_name, ": treatment effect vs. control, by community trust"),
  #        subtitle = "Each arm compared with the control arm within the same trust group",
  #        caption = ci_note) +
  #   base_theme
  # 
  # p3 <- ggplot(interaction_df, aes(arm, est)) +
  #   zero_line +
  #   geom_pointrange(aes(ymin = lo, ymax = hi), colour = "grey20", linewidth = 0.6) +
  #   facet_wrap(~ outcome, nrow = 1, scales = "free_y") +
  #   labs(y = ylab,
  #        title = paste0(group_name, ": does trust change the treatment effect?"),
  #        subtitle = "Interaction coefficients: effect in trusted minus effect in untrusted communities",
  #        caption = ci_note) +
  #   base_theme
  # 
  # list(levels = p1, effects = p2, interaction = p3)
  list(levels = p1)
}

# ---- 3. Loop over the three groups, print + save all 9 figures ---------------
figs <- list()
for (g in names(specs)) {
  models_g <- specs[[g]]$models
  vars_g   <- specs[[g]]$vars
  uselog_g <- specs[[g]]$log
  
  p <- plot_group(models_g, vars_g, group_name = g, uselog = uselog_g)
  figs[[g]] <- p
  
  print(p$levels); print(p$effects); print(p$interaction)
  
  ggsave(file.path(out_dir, paste0("reg_", g, "_spec3.png")),       p$levels,      width = 11, height = 4.5, dpi = 300)
  # ggsave(file.path(out_dir, paste0(g, "_fig2_effects_by_trust.png")), p$effects,    width = 11, height = 4.5, dpi = 300)
  # ggsave(file.path(out_dir, paste0(g, "_fig3_interaction.png")),      p$interaction, width = 11, height = 4.5, dpi = 300)
}



# =============================================================================
# 4. SENSITIVITY TO THE "TRUSTED COMMUNITY" CUTOFF
#    rankingInterTop1 ... rankingInterTop15
#
# Paste this after section 3 of workingPaperRegressions.R. It reuses:
#   data, controls, specs, spec3_results(), label_for(), level, as_percent, out_dir
#
# For each outcome group (m1, m2, m3), each outcome, and each cutoff k = 1..15,
# it re-estimates specification 3 with rankingInterTop<k> in place of
# rankingInterTop3, keeps the "levels" coefficients (the same numbers as p1),
# and draws one figure per group: x-axis = cutoff k, one line per arm x trust.
# =============================================================================

k_max <- 15

# ---- 4.1 Create the cutoff dummies --------------------------------------------
# rankingInterTop<k> = 1 if rankingInter <= k, 0 otherwise (NA stays NA)
for (k in 1:k_max) {
  data[[paste0("rankingInterTop", k)]] <- as.integer(data$rankingInter <= k)
}

# ---- 4.2 Specification 3 with any trust dummy --------------------------------
fit_spec3 <- function(var, tv, uselog) {
  rhs <- paste0(" + treatment + ", tv, " + treatment*", tv, controls)
  f <- if (uselog) {
    paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1)", rhs)
  } else {
    paste0(var, " ~ ", var, "_baseline", rhs)
  }
  feols(as.formula(f), data = data, cluster = ~communityOrHamlet_baseline)
}

# ---- 4.3 Run all regressions and store the coefficients ----------------------
arm_order <- levels(factor(data$treatment))   # "0" "1" "2" "3"
ref_arm   <- arm_order[1]

sens <- list()
for (g in names(specs)) {
  sens[[g]] <- map_dfr(1:k_max, function(k) {
    tv <- paste0("rankingInterTop", k)
    map_dfr(specs[[g]]$vars, function(v) {
      out <- tryCatch(
        spec3_results(fit_spec3(v, tv, specs[[g]]$log), ref_arm, tv, level)$levels,
        error = function(e) {
          message("Skipped ", g, " / ", v, " / k = ", k, ": ", conditionMessage(e))
          NULL
        })
      if (!is.null(out)) mutate(out, outcome = v, k = k)
    })
  })
}

# Optional: keep the coefficients on disk
# write.csv(bind_rows(sens, .id = "group"), file.path(out_dir, "sensitivity_cutoff_coefs.csv"), row.names = FALSE)

# ---- 4.4 Plot: one figure per group ------------------------------------------
arm_labels <- c("0" = "Control", "1" = "QE", "2" = "TA", "3" = "QE + TA")
arm_pal    <- c("Control" = "grey35", "QE" = "#E69F00", "TA" = "#0072B2", "QE + TA" = "#009E73")

plot_sensitivity <- function(df, vars, uselog) {
  pct <- as_percent && uselog
  
  df <- df %>%
    filter(!(arm == ref_arm & trusted == 0)) %>%   # omitted category, always 0
    mutate(
      outcome = factor(outcome, levels = vars, labels = vapply(vars, label_for, "")),
      arm     = factor(arm_labels[as.character(arm)], levels = arm_labels),
      trusted = factor(trusted, levels = c(0, 1), labels = c("Untrusted", "Trusted"))
    ) %>%
    { if (pct) mutate(., across(c(est, lo, hi), ~ 100 * (exp(.x) - 1))) else . }
  
  ylab <- if (uselog) {
    if (pct) "% difference" else "Difference in ln(y + 1)"
  } else "Difference in y (levels)"
  
  dodge <- position_dodge(width = 0.7)
  
  ggplot(df, aes(k, est, colour = arm, shape = trusted, linetype = trusted,
                 group = interaction(arm, trusted))) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_vline(xintercept = 3, colour = "grey75", linetype = "dotted") +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, alpha = 0.35,
                  linetype = "solid", position = dodge) +
    geom_line(linewidth = 0.4, position = dodge) +
    geom_point(size = 1.7, position = dodge) +
    facet_wrap(~ outcome, nrow = 1, scales = "free_y") +
    scale_x_continuous(breaks = 1:k_max) +
    scale_colour_manual(values = arm_pal) +
    scale_shape_manual(values = c(Untrusted = 1, Trusted = 16)) +
    scale_linetype_manual(values = c(Untrusted = "dashed", Trusted = "solid")) +
    labs(
      x = "Trusted community = intermediary ranked in top k",
      y = ylab,
      title = "Outcome level by treatment arm and community trust, across trust cutoffs",
      subtitle = "Relative to Control x Untrusted (omitted, = 0). Dotted line: main specification (k = 3)",
      caption = paste0(round(level * 100),
                       "% CIs, SEs clustered by community/hamlet. QE = Quality Evaluation; TA = Technical Assistance.")
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", legend.title = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"))
}

figs_sens <- list()
for (g in names(specs)) {
  figs_sens[[g]] <- plot_sensitivity(sens[[g]], specs[[g]]$vars, specs[[g]]$log)
  print(figs_sens[[g]])
  ggsave(file.path(out_dir, paste0("reg_", g, "_spec3_cutoffs.png")),
         figs_sens[[g]], width = 14, height = 5.5, dpi = 300)
}

