packageList <- c("dplyr","readxl","purrr","tidyr","stringr","tidyverse","fuzzyjoin",
                 "kableExtra","stargazer","haven","fixest","modelsummary")
lapply(packageList,require,character.only=TRUE)
#rm(list=ls())

#wide_file     <- "01_data/processed/harmonization/wideDF.csv"
#data  <- read.csv(wide_file)
#colnames(data) <- gsub("\\.", "/", colnames(data))
data <- wideDF
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
  "householdHasPipedWater","householdHasMobilePhone",
  "hasInternetAtHome","yearsExperienceInCoffeeFarming",
  "usesWhatsAppForMessages","registeredWithIHCAFE",
  "coffeeAreaLastHarvestInHa"
  #"amountSoldInKgGreen_typ" ,"minimumReceivedPriceForCoffeeInKgGreen_typ","maximumReceivedPriceForCoffeeInKgGreen_typ"
)
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
  ## FE = No; Controls = No
  formula <- as.formula(paste0("`", var, "` ~ `", var, "_baseline` + treatment"))
  models1[[paste0(var,"_s1")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## FE = Yes; Controls = No
  formula <- as.formula(paste0("`", var, "` ~ `", var, "_baseline` + treatment", " | sellsToIntermediary"))
  models1[[paste0(var,"_s2")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## FE = No; Controls = Yes
  formula <- as.formula(paste0("`", var, "` ~ `", var, "_baseline` + treatment", controls))
  models1[[paste0(var,"_s3")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## FE = Yes; Controls = Yes
  formula <- as.formula(paste0("`", var, "` ~ `", var, "_baseline` + treatment", controls, " | sellsToIntermediary"))
  models1[[paste0(var,"_s4")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
}


### Processing times with with always doers ------------------------------------
#' Intensive margin for those who already did it?
#' Not causal
models2 <- list()
# variables_m2 <- c(
#   "timeBetweenHarvestAndDelivery","timeBetweenHarvestAndDeliveryDiff","timeBetweenHarvestAndDeliveryDiffr",
#   "timeBetweenHarvestAndPulping","timeBetweenHarvestAndPulpingDiff","timeBetweenHarvestAndPulpingDiffr",
#   "timeBetweenPulpingAndWashing","timeBetweenPulpingAndWashingDiff","timeBetweenPulpingAndWashingDiffr",
#   "timeFromWashingToDryCoffee","timeFromWashingToDryCoffeeDiff","timeFromWashingToDryCoffeeDiffr")
variables_m2 <- c(
  "timeBetweenHarvestAndDeliveryDiffr","timeBetweenHarvestAndPulpingDiffr","timeBetweenPulpingAndWashingDiffr")
for (var in variables_m2) {
  ## FE = No; Controls = No
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment"))
  models2[[paste0(var,"_s1")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## FE = Yes; Controls = No
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", " | sellsToIntermediary"))
  models2[[paste0(var,"_s2")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## FE = No; Controls = Yes
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", controls))
  models2[[paste0(var,"_s3")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## FE = Yes; Controls = Yes
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", controls, " | sellsToIntermediary"))
  models2[[paste0(var,"_s4")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  }


#' -----------------------------------------------------------------------------
## Intermediary outcomes: Market outcomes --------------------------------------

### Full sample ----------------------------------------------------------------
#kg_sold_inter: extensive margin was affected
models3a <- list()
models3b <- list()
variables_m3_ln <- c(
  "amountSoldInKgGreen_typ","minimumReceivedPriceForCoffeeInKgGreen_typ",
  "maximumReceivedPriceForCoffeeInKgGreen_typ","amountSoldInKgGreenInter_typ",
  "minimumReceivedPriceForCoffeeInKgGreenInter_typ","maximumReceivedPriceForCoffeeInKgGreenInter_typ")
variables_m3_nu <- c(
  "numberDifferentBuyersLastHarvest","probSellInter","percSellInter") #"receivedQualityBonus_typ","receivedQualityDiscount_typ",
#Reg with ln
for (var in variables_m3_ln) {
  ## FE = No; Controls = No
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment"))
  models3b[[paste0(var,"_s1")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## FE = Yes; Controls = No
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", " | sellsToIntermediary"))
  models3b[[paste0(var,"_s2")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## FE = No; Controls = Yes
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", controls))
  models3b[[paste0(var,"_s3")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## FE = Yes; Controls = Yes
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", controls, " | sellsToIntermediary"))
  models3b[[paste0(var,"_s4")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
}
#Reg numerical
for (var in variables_m3_nu) {
  ## FE = No; Controls = No
  formula <- as.formula(paste0(var, " ~ ", var, "_baseline + treatment"))
  models3a[[paste0(var,"_s1")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## FE = Yes; Controls = No
  formula <- as.formula(paste0(var, " ~ ", var, "_baseline + treatment", " | sellsToIntermediary"))
  models3a[[paste0(var,"_s2")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## FE = No; Controls = Yes
  formula <- as.formula(paste0(var, " ~ ", var,  "_baseline + treatment", controls))
  models3a[[paste0(var,"_s3")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## FE = Yes; Controls = Yes
  formula <- as.formula(paste0(var, " ~ ", var, "_baseline + treatment", controls, " | sellsToIntermediary"))
  models3a[[paste0(var,"_s4")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  }


### Reduced sample: only intermediaries ----------------------------------------
#' Not causal?
models4 <- list()
variables_m4_ln <- c(
  "minimumReceivedPriceForCoffeeInKgGreenInter_typ","maximumReceivedPriceForCoffeeInKgGreenInter_typ")
variables_m4_nu <- c(
  "receivedQualityBonusInter_typ","receivedQualityDiscountInter_typ")
#Reg with ln
# for (var in variables_m4_ln) {
#   ## FE = No; Controls = No
#   formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment"))
#   models4[[paste0(var,"_s1")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
#   ## FE = Yes; Controls = No
#   formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", " | sellsToIntermediary"))
#   models4[[paste0(var,"_s2")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
#   ## FE = No; Controls = Yes
#   formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", controls))
#   models4[[paste0(var,"_s3")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
#   ## FE = Yes; Controls = Yes
#   formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", controls, " | sellsToIntermediary"))
#   models4[[paste0(var,"_s4")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
# }
#Reg numerical
# for (var in variables_m4_nu) {
#   ## FE = No; Controls = No
#   formula <- as.formula(paste0(var, " ~ ", var, "_baseline + treatment"))
#   models4[[paste0(var,"_s1")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
#   ## FE = Yes; Controls = No
#   formula <- as.formula(paste0(var, " ~ ", var, "_baseline + treatment", " | sellsToIntermediary"))
#   models4[[paste0(var,"_s2")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
#   ## FE = No; Controls = Yes
#   formula <- as.formula(paste0(var, " ~ ", var,  "_baseline + treatment", controls))
#   models4[[paste0(var,"_s3")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
#   ## FE = Yes; Controls = Yes
#   formula <- as.formula(paste0(var, " ~ ", var, "_baseline + treatment", controls, " | sellsToIntermediary"))
#   models4[[paste0(var,"_s4")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
#   }



#' -----------------------------------------------------------------------------
## Long-term outcomes ----------------------------------------------------------
models5 <- list()
variables_m5 <- c(
  "amountOfCoffeeProducedLastHarvestInKgGreen","amountSoldInKgGreen_typ","yieldKgPerHa")
for (var in variables_m5) {
  ## FE = No; Controls = No
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment"))
  models5[[paste0(var,"_s1")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## FE = Yes; Controls = No
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", " | sellsToIntermediary"))
  models5[[paste0(var,"_s2")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## FE = No; Controls = Yes
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", controls))
  models5[[paste0(var,"_s3")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  ## FE = Yes; Controls = Yes
  formula <- as.formula(paste0("log(", var, " + 1) ~ log(", var, "_baseline + 1) + treatment", controls, " | sellsToIntermediary"))
  models5[[paste0(var,"_s4")]] <- feols(formula,data = data,cluster = ~communityOrHamlet_baseline, se="cluster")
  }



#' -----------------------------------------------------------------------------
#  SAVING TABLES  ---------------------------------------------------------------
#' -----------------------------------------------------------------------------

length_cluster <- length(variables_m1)

all_models <- list(
  #models1 = models1,
  models2 = models2,
  models3a = models3a,
  models3b = models3b,
  #models5 = models5
  )

for (name in names(all_models)) {
  
  models <- all_models[[name]]
  modelsummary(models,
    output = paste0("03_tables/harmonization/", name, ".html"),
    stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01),
    gof_map = c("nobs", "adj.r.squared"),
    add_rows = bind_rows(
      tibble(term = "Errors clustered",
             !!!setNames(rep(list("Yes"), length(models)), names(models))),
      tibble(term = "FE by intermediary",
             !!!setNames(rep(c("No","Yes","No","Yes"), length(models)/4), names(models))),
      tibble(term = "Controls",
             !!!setNames(rep(c("No","No","Yes","Yes"), length(models)/4), names(models)))),
    notes = c("treatment1 = Quality Evaluation (QE); treatment2 = Technical Assistance (TA); treatment3 = QE + TA "))
}
