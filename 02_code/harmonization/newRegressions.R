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


#' -----------------------------------------------------------------------------
## Intermediary outcomes: Adoption of practices --------------------------------


### Dummy for adoption ---------------------------------------------------------
#' Considering Always-doers, Adopters, Droppers, Never-doers
#' Extensive margin?
models1 <- list()
  
#`coffeeProcessingDoneOnFarm/N`
linear_model_cl <- feglm(
  `coffeeProcessingDoneOnFarm/N` ~ `coffeeProcessingDoneOnFarm/N_baseline` + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models1[["coffeeProcessingDoneOnFarm/N"]] <- linear_model_cl
summary(models1$`coffeeProcessingDoneOnFarm/N`)

#`coffeeProcessingDoneOnFarm/despulpado`
linear_model_cl <- feglm(
  `coffeeProcessingDoneOnFarm/despulpado` ~ `coffeeProcessingDoneOnFarm/despulpado_baseline` + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models1[["coffeeProcessingDoneOnFarm/despulpado"]] <- linear_model_cl
summary(models1$`coffeeProcessingDoneOnFarm/despulpado`)

#`coffeeProcessingDoneOnFarm/fermentado`
linear_model_cl <- feglm(
  `coffeeProcessingDoneOnFarm/fermentado` ~ `coffeeProcessingDoneOnFarm/fermentado_baseline` + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models1[["coffeeProcessingDoneOnFarm/fermentado"]] <- linear_model_cl
summary(models1$`coffeeProcessingDoneOnFarm/fermentado`)

#`coffeeProcessingDoneOnFarm/lavado`
linear_model_cl <- feglm(
  `coffeeProcessingDoneOnFarm/lavado` ~ `coffeeProcessingDoneOnFarm/lavado_baseline` + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models1[["coffeeProcessingDoneOnFarm/lavado"]] <- linear_model_cl
summary(models1$`coffeeProcessingDoneOnFarm/lavado`)


### Processing times with with always doers ------------------------------------
#' Intensive margin for those who already did it?
#' Not causal
models2 <- list()

#timeBetweenHarvestAndDelivery
linear_model_cl <- feols(
  log(timeBetweenHarvestAndDelivery+1) ~ log(timeBetweenHarvestAndDelivery_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models2[["timeBetweenHarvestAndDelivery"]] <- linear_model_cl
linear_model_cl <- feols(
  log(timeBetweenHarvestAndDeliveryDiff+1) ~ log(timeBetweenHarvestAndDeliveryDiff_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models2[["timeBetweenHarvestAndDeliveryDiff"]] <- linear_model_cl
linear_model_cl <- feols(
  log(timeBetweenHarvestAndDeliveryDiffr+1) ~ log(timeBetweenHarvestAndDeliveryDiffr_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models2[["timeBetweenHarvestAndDeliveryDiffr"]] <- linear_model_cl

#timeBetweenHarvestAndPulping
linear_model_cl <- feols(
  log(timeBetweenHarvestAndPulping+1) ~ log(timeBetweenHarvestAndPulping_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models2[["timeBetweenHarvestAndPulping"]] <- linear_model_cl
linear_model_cl <- feols(
  log(timeBetweenHarvestAndPulpingDiff+1) ~ log(timeBetweenHarvestAndPulpingDiff_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models2[["timeBetweenHarvestAndPulpingDiff"]] <- linear_model_cl
linear_model_cl <- feols(
  log(timeBetweenHarvestAndPulpingDiffr+1) ~ log(timeBetweenHarvestAndPulpingDiffr_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models2[["timeBetweenHarvestAndPulpingDiffr"]] <- linear_model_cl

#timeBetweenPulpingAndWashing
linear_model_cl <- feols(
  log(timeBetweenPulpingAndWashing+1) ~ log(timeBetweenPulpingAndWashing_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models2[["timeBetweenPulpingAndWashing"]] <- linear_model_cl
linear_model_cl <- feols(
  log(timeBetweenPulpingAndWashingDiff+1) ~ log(timeBetweenPulpingAndWashingDiff_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models2[["timeBetweenPulpingAndWashingDiff"]] <- linear_model_cl
linear_model_cl <- feols(
  log(timeBetweenPulpingAndWashingDiffr+1) ~ log(timeBetweenPulpingAndWashingDiffr_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models2[["timeBetweenPulpingAndWashingDiffr"]] <- linear_model_cl

#timeFromWashingToDryCoffee
linear_model_cl <- feols(
  log(timeFromWashingToDryCoffee+1) ~ log(timeFromWashingToDryCoffee_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models2[["timeFromWashingToDryCoffee"]] <- linear_model_cl
linear_model_cl <- feols(
  log(timeFromWashingToDryCoffeeDiff+1) ~ log(timeFromWashingToDryCoffeeDiff_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models2[["timeFromWashingToDryCoffeeDiff"]] <- linear_model_cl
linear_model_cl <- feols(
  log(timeFromWashingToDryCoffeeDiffr+1) ~ log(timeFromWashingToDryCoffeeDiffr_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models2[["timeFromWashingToDryCoffeeDiffr"]] <- linear_model_cl


#' -----------------------------------------------------------------------------
## Intermediary outcomes: Market outcomes --------------------------------------

### Full sample ----------------------------------------------------------------
models3 <- list()

##kg_sold_full
linear_model_cl <- feols(
  log(amountSoldInKgGreen_typ+1) ~ log(amountSoldInKgGreen_typ_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models3[["kg_sold_full"]] <- linear_model_cl

##min_price_full
linear_model_cl <- feols(
  log(minimumReceivedPriceForCoffeeInKgGreen_typ+1) ~ log(minimumReceivedPriceForCoffeeInKgGreen_typ_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models3[["min_price_full"]] <- linear_model_cl

##max_price_full
linear_model_cl <- feols(
  log(maximumReceivedPriceForCoffeeInKgGreen_typ+1) ~ log(maximumReceivedPriceForCoffeeInKgGreen_typ_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models3[["max_price_full"]] <- linear_model_cl

##numberDifferentBuyersLastHarvest
logit_model_cl <- feglm(
  numberDifferentBuyersLastHarvest ~ numberDifferentBuyersLastHarvest_baseline + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models3[["numberDifferentBuyersLastHarvest"]] <- logit_model_cl

##receivedQualityBonus_full
logit_model_cl <- feglm(
  receivedQualityBonus_typ ~ receivedQualityBonus_typ_baseline + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models3[["receivedQualityBonus_full"]] <- logit_model_cl

##receivedQualityDiscount_full
logit_model_cl <- feglm(
  receivedQualityDiscount_typ ~ receivedQualityDiscount_typ_baseline + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models3[["receivedQualityDiscount_full"]] <- logit_model_cl

##probSellInter: extensive margin was affected
linear_model_cl <- feols(
  probSellInter ~ probSellInter_baseline + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models3[["probSellInter"]] <- linear_model_cl
summary(models3$prob_sell_inter)

##perc_sell_inter: reallocation of sellings
linear_model_cl <- feols(
  percSellInter ~ percSellInter_baseline + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models3[["percSellInter"]] <- linear_model_cl
summary(models3$perc_sell_inter)

#kg_sold_inter: extensive margin was affected
linear_model_cl <- feols(
  log(amountSoldInKgGreenInter_typ+1) ~ log(amountSoldInKgGreenInter_typ_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models3[["kg_sold_inter"]] <- linear_model_cl
summary(models3$kg_sold_inter)



### Reduced sample: only intermediaries ----------------------------------------
#' Not causal?
models4 <- list()

##min_price_inter
linear_model_cl <- feols(
  log(minimumReceivedPriceForCoffeeInKgGreenInter_typ+1) ~ log(minimumReceivedPriceForCoffeeInKgGreenInter_typ_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models4[["min_price_inter"]] <- linear_model_cl
summary(models4$min_price_inter)

##max_price_inter
linear_model_cl <- feols(
  log(maximumReceivedPriceForCoffeeInKgGreenInter_typ+1) ~ log(maximumReceivedPriceForCoffeeInKgGreenInter_typ_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models4[["max_price_inter"]] <- linear_model_cl
summary(models4$max_price_inter)

##receivedQualityBonus_inter
logit_model_cl <- feglm(
  receivedQualityBonusInter_typ ~ receivedQualityBonusInter_typ_baseline + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models4[["receivedQualityBonus_inter"]] <- logit_model_cl

##receivedQualityDiscount_inter
logit_model_cl <- feglm(
  receivedQualityDiscountInter_typ ~ receivedQualityDiscountInter_typ_baseline + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models4[["receivedQualityDiscount_inter"]] <- logit_model_cl



#' -----------------------------------------------------------------------------
## Long-term outcomes ----------------------------------------------------------
models5 <- list()

#amountOfCoffeeProducedLastHarvestInKgGreen
linear_model_cl <- feols(
  log(amountOfCoffeeProducedLastHarvestInKgGreen+1) ~ log(amountOfCoffeeProducedLastHarvestInKgGreen_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models5[["amountOfCoffeeProducedLastHarvestInKgGreen"]] <- linear_model_cl

#amountSoldInKgGreen_typ
linear_model_cl <- feols(
  log(amountSoldInKgGreen_typ+1) ~ log(amountSoldInKgGreen_typ_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models5[["amountSoldInKgGreen_typ"]] <- linear_model_cl

#yieldKgPerHa
linear_model_cl <- feols(
  log(yieldKgPerHa+1) ~ log(yieldKgPerHa_baseline+1) + treatment,
  data = data, cluster = ~communityOrHamlet_baseline)
models5[["yieldKgPerHa"]] <- linear_model_cl


#' -----------------------------------------------------------------------------
#  SAVING TABLES  ---------------------------------------------------------------
#' -----------------------------------------------------------------------------


all_models <- list(
  models1 = models1,
  models2 = models2,
  models3 = models3,
  models4 = models4,
  models5 = models5)

for (name in names(all_models)) {
  
  models <- all_models[[name]]
  
  modelsummary(
    models,
    output = paste0("03_tables/harmonization/", name, ".html"),
    stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01),
    gof_map = c("nobs", "adj.r.squared"),
    add_rows = bind_rows(
      tibble(
        term = "Errors clustered",
        !!!setNames(rep(list("Yes"), length(models)), names(models))
      ),
      tibble(
        term = "Controls",
        !!!setNames(rep(list("No"), length(models)), names(models))
      )
    ),
    notes = c(
      "treatment1 = Quality Evaluation (QE); treatment2 = Technical Assistance (TA); treatment3 = QE + TA ")
  )
}

