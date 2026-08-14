packageList <- c("dplyr","readxl","purrr","tidyr","stringr","tidyverse","fuzzyjoin",
                 "kableExtra","stargazer","haven","fixest","modelsummary")
lapply(packageList,require,character.only=TRUE)
rm(list=ls())

wide_file     <- "01_data/processed/harmonization/wideDF.csv"
data  <- read.csv(wide_file) %>%
  mutate(treatment = case_when(
    treatment == "Control" ~ "0",
    treatment == "Evaluacion de calidad" ~ "1",
    treatment == "Asistencia tecnica" ~ "2",
    treatment == "Asistencia tecnica y evaluacion de calidad" ~ "3",
    TRUE ~ treatment  # Keep any other values unchanged if they exist
  ))
colnames(data) <- gsub("\\.", "/", colnames(data))


#' -----------------------------------------------------------------------------
#  REGRESSIONS  ----------------------------------------------------------------
#' -----------------------------------------------------------------------------

covariates <- c(
  "householdHasPipedWater","householdConnectedToElectricity","householdHasMobilePhone",
  "hasInternetAtHome","hasMigratedInThePastYear_pop","yearsExperienceInCoffeeFarming",
  "usesWhatsAppForMessages","registeredWithIHCAFE","adoptedRecommendedTechnologies",
  "totalAreaUsedForAgricultureInHa","coffeeAreaLastHarvestInHa"
  #"amountSoldInKgGreen_typ" ,"minimumReceivedPriceForCoffeeInKgGreen_typ","maximumReceivedPriceForCoffeeInKgGreen_typ"
)

# Setting parameters
## Controls
# controls <- ""
# controls_text <- "No"
controls <- paste(" + ", paste(covariates, collapse = " + "))
controls_text <- "Yes"
## Fixed effects
# fixed_effects <- ""
# fixed_effects_text <- "No"
fixed_effects <- " | sellsToIntermediary"
fixed_effects_text <- "Yes"

# Robustness: excluding communities
#data <- data %>% filter(!communityOrHamlet %in% c("nyork","zorca"))

#' -----------------------------------------------------------------------------
## Intermediary outcomes: Adoption of practices --------------------------------


### Dummy for adoption ---------------------------------------------------------
#' Considering Always-doers, Adopters, Droppers, Never-doers
#' Extensive margin?
models1 <- list()
variables_m1 <- c(
  "coffeeProcessingDoneOnFarm/N","coffeeProcessingDoneOnFarm/despulpado",
  "coffeeProcessingDoneOnFarm/fermentado","coffeeProcessingDoneOnFarm/lavado")
for (var in variables_m1) {
  formula <- as.formula(paste0("`", var, "` ~ `", var, "_baseline` + treatment", controls, fixed_effects))
  models1[[var]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
}


### Processing times with with always doers ------------------------------------
#' Intensive margin for those who already did it?
#' Not causal
models2 <- list()
variables_m2 <- c(
  "timeBetweenHarvestAndDelivery","timeBetweenHarvestAndDeliveryDiff","timeBetweenHarvestAndDeliveryDiffr",
  "timeBetweenHarvestAndPulping","timeBetweenHarvestAndPulpingDiff","timeBetweenHarvestAndPulpingDiffr",
  "timeBetweenPulpingAndWashing","timeBetweenPulpingAndWashingDiff","timeBetweenPulpingAndWashingDiffr",
  "timeFromWashingToDryCoffee","timeFromWashingToDryCoffeeDiff","timeFromWashingToDryCoffeeDiffr")
for (var in variables_m2) {
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", controls, fixed_effects))
  models2[[var]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
}


#' -----------------------------------------------------------------------------
## Intermediary outcomes: Market outcomes --------------------------------------

### Full sample ----------------------------------------------------------------
#kg_sold_inter: extensive margin was affected
models3 <- list()
variables_m3_ln <- c(
  "amountSoldInKgGreen_typ","minimumReceivedPriceForCoffeeInKgGreen_typ",
  "maximumReceivedPriceForCoffeeInKgGreen_typ","amountSoldInKgGreenInter_typ")
variables_m3_nu <- c(
  "numberDifferentBuyersLastHarvest","receivedQualityBonus_typ","receivedQualityDiscount_typ",
  "probSellInter","percSellInter")
#Reg with ln
for (var in variables_m3_ln) {
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", controls, fixed_effects))
  models3[[var]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
}
#Reg numerical
for (var in variables_m3_nu) {
  formula <- as.formula(paste0(var, " ~ ", var, "_baseline + treatment", controls, fixed_effects))
  models3[[var]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
}


### Reduced sample: only intermediaries ----------------------------------------
#' Not causal?
models4 <- list()
variables_m4_ln <- c(
  "minimumReceivedPriceForCoffeeInKgGreenInter_typ","maximumReceivedPriceForCoffeeInKgGreenInter_typ")
variables_m4_nu <- c(
  "receivedQualityBonusInter_typ","receivedQualityDiscountInter_typ")
#Reg with ln
for (var in variables_m4_ln) {
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", controls, fixed_effects))
  models4[[var]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
}
#Reg numerical
for (var in variables_m4_nu) {
  formula <- as.formula(paste0(var, " ~ ", var, "_baseline + treatment", controls, fixed_effects))
  models4[[var]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
}



#' -----------------------------------------------------------------------------
## Long-term outcomes ----------------------------------------------------------
models5 <- list()
variables_m5 <- c(
  "amountOfCoffeeProducedLastHarvestInKgGreen","amountSoldInKgGreen_typ","yieldKgPerHa")
for (var in variables_m5) {
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", controls, fixed_effects))
  models5[[var]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
}



#' -----------------------------------------------------------------------------
#  SAVING TABLES  ---------------------------------------------------------------
#' -----------------------------------------------------------------------------

length_cluster <- length(variables_m1)

all_models <- list(
  models1 = models1,
  models2 = models2,
  models3 = models3,
  models4 = models4,
  models5 = models5)

for (name in names(all_models)) {
  
  models <- all_models[[name]]
  modelsummary(models,
    output = paste0("03_tables/harmonization/", name, "_Controls", controls_text, "_FE", fixed_effects_text, ".html"),
    stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01),
    gof_map = c("nobs", "adj.r.squared"),
    add_rows = bind_rows(
      tibble(term = "Errors clustered",
             !!!setNames(rep(list("Yes"), length(models)), names(models))),
      tibble(term = "FE by intermediary",
             !!!setNames(rep(list(fixed_effects_text), length(models)), names(models))),
      tibble(term = "Controls",
             !!!setNames(rep(list(controls_text), length(models)), names(models)))),
    notes = c("treatment1 = Quality Evaluation (QE); treatment2 = Technical Assistance (TA); treatment3 = QE + TA "))
}
