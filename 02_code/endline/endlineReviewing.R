#' -----------------------------------------------------------------------------
#' IMPORTANT!
#' Run the beginning of endlineProcessingCode.R before running this script.
#' End on line 67: base_df <- read_excel(data_path, sheet = "Encuesta de Línea Intermedia...")
#' -----------------------------------------------------------------------------
#' 
#' This script reviews variables in "01_data/raw/endline/endlineAgg.csv", including 
#' missing values, outliers, and violations of expected numeric order relationships.
#' The main input for these corrections is: "03_tables/endline/endlineQualityCheck.RData"


# Loading data
df      <- read.csv("01_data/processed/endline/endlineAgg.csv")
results <- readRDS("03_tables/endline/endlineQualityCheck.RData")



#' ========================================================================
#  Assessing area variables -----------------------------------------------
#' ========================================================================

# Vars on main sheet
base_area <- df %>%
  select(c(surveyID,X_uuid,totalFarmAreaInHa_trr,totalAreaUsedForAgricultureInHa_trr,
           coffeeAreaLastHarvestInHa_trr,totalFarmArea,totalAreaUnit,
           totalAreaUsedForAgriculture,measurementUnitOfArea,
           coffeeAreaLastHarvest,productiveCoffeeAreaUnit))


# Vars on area_terr sheet
lands_area <- lands_df %>%
  select(c('_submission__uuid',totalFarmArea,totalAreaUnit,totalAreaUsedForAgriculture,
           measurementUnitOfArea,coffeeAreaLastHarvest,productiveCoffeeAreaUnit))


# Joining to have a better overview
df_area <- full_join(base_area, lands_area,  by = c("X_uuid"="_submission__uuid"))
rm(lands_area)


#' ------------------------------------------------------------------------
## Outliers ------------------------
# Separating observations with outliers (based on quality check)
vars_check <- c("totalFarmAreaInHa_trr","totalAreaUsedForAgricultureInHa_trr","coffeeAreaLastHarvestInHa_trr")
## Loop by outlier variable
for(v in vars_check){
  df[[paste0(v, "_outlier")]] <-
    !is.na(df[[v]]) & (
      df[[v]] < as.numeric(results$lower_bound[[v]]) |
        df[[v]] > as.numeric(results$upper_bound[[v]])
    )
}
## Keeping only rows with at least 1 outlier
flag             <- rowSums(df[paste0(vars_check, "_outlier")]) > 0
df_area_outliers <- df_area[df_area$surveyID %in% df$surveyID[flag], ]
## Adding dummies to identify which var has the outlier
df_area_outliers <- merge(
  df_area_outliers,
  df[, c("surveyID", paste0(vars_check, "_outlier"))],
  by = "surveyID",  all.x = TRUE
)


# Evaluating cases with any outlier
## Case 1: only coffeeAreaLastHarvestInHa_outlier=TRUE
df_area_outliers_c1 <- df_area_outliers %>% filter(
    coffeeAreaLastHarvestInHa_trr_outlier       == TRUE  &
    totalAreaUsedForAgricultureInHa_trr_outlier == FALSE &
    totalFarmAreaInHa_trr_outlier               == FALSE )
summary(duplicated(df_area_outliers_c1$surveyID))
results[["upper_bound"]][["coffeeAreaLastHarvestInHa_trr"]]

## Case 2: only totalAreaUsedForAgricultureInHa_outlier=TRUE
df_area_outliers_c2 <- df_area_outliers %>% filter(
    coffeeAreaLastHarvestInHa_trr_outlier       == FALSE &
    totalAreaUsedForAgricultureInHa_trr_outlier == TRUE  &
    totalFarmAreaInHa_trr_outlier               == FALSE )
summary(duplicated(df_area_outliers_c2$surveyID))
results[["upper_bound"]][["totalAreaUsedForAgricultureInHa_trr"]]

## Case 3: only totalFarmAreaInHa_trr_outlier=TRUE
df_area_outliers_c3 <- df_area_outliers %>% filter(
    coffeeAreaLastHarvestInHa_trr_outlier       == FALSE &
    totalAreaUsedForAgricultureInHa_trr_outlier == FALSE &
    totalFarmAreaInHa_trr_outlier               == TRUE  )
summary(duplicated(df_area_outliers_c3$surveyID))
results[["upper_bound"]][["totalFarmAreaInHa_trr"]]
corrected_outliers <- c("2023-10-27-T-6888-X-5826","2023-11-02-T-4733-X-8087")

## Case 4: coffeeAreaLastHarvestInHa_outlier=TRUE & totalAreaUsedForAgricultureInHa_outlier=TRUE
df_area_outliers_c4 <- df_area_outliers %>% filter(
    coffeeAreaLastHarvestInHa_trr_outlier       == TRUE &
    totalAreaUsedForAgricultureInHa_trr_outlier == TRUE &
    totalFarmAreaInHa_trr_outlier               == FALSE)
summary(duplicated(df_area_outliers_c4$surveyID))

## Case 5: coffeeAreaLastHarvestInHa_outlier=TRUE & totalFarmAreaInHa_trr_outlier=TRUE
df_area_outliers_c5 <- df_area_outliers %>% filter(
    coffeeAreaLastHarvestInHa_trr_outlier       == TRUE  &
    totalAreaUsedForAgricultureInHa_trr_outlier == FALSE &
    totalFarmAreaInHa_trr_outlier               == TRUE )
summary(duplicated(df_area_outliers_c5$surveyID))

## Case 6: totalAreaUsedForAgricultureInHa_outlier=TRUE & totalFarmAreaInHa_trr_outlier=TRUE
df_area_outliers_c6 <- df_area_outliers %>% filter(
    coffeeAreaLastHarvestInHa_trr_outlier       == FALSE &
    totalAreaUsedForAgricultureInHa_trr_outlier == TRUE  &
    totalFarmAreaInHa_trr_outlier               == TRUE )
summary(duplicated(df_area_outliers_c6$surveyID))

## Case 7: all=TRUE
df_area_outliers_c7 <- df_area_outliers %>% filter(
    coffeeAreaLastHarvestInHa_trr_outlier       == TRUE  &
    totalAreaUsedForAgricultureInHa_trr_outlier == TRUE &
    totalFarmAreaInHa_trr_outlier               == TRUE )
summary(duplicated(df_area_outliers_c7$surveyID))
corrected_outliers <- c("2023-10-30-T-942-X-8394","2023-10-05-T-129-X-990")

## Deleting variables
rm(vars_check,flag,df_area_outliers,df_area_outliers_c1,df_area_outliers_c2,
   df_area_outliers_c3,df_area_outliers_c4,df_area_outliers_c5,df_area_outliers_c6,
   df_area_outliers_c7,corrected_outliers)


#' ------------------------------------------------------------------------
## Comparing areas ------------------------

# Case 1: coffeeAreaLastHarvestInHa > totalAreaUsedForAgricultureInHa
df_area_comparing_c1 <- df_area %>% filter(
  coffeeAreaLastHarvestInHa_trr > totalAreaUsedForAgricultureInHa_trr)
summary(duplicated(df_area_comparing_c1$surveyID))

# Case 2: coffeeAreaLastHarvestInHa > totalFarmAreaInHa_trr
df_area_comparing_c2 <- df_area %>% filter(
  coffeeAreaLastHarvestInHa_trr > totalFarmAreaInHa_trr)
summary(duplicated(df_area_comparing_c2$surveyID))

# Case 3: totalAreaUsedForAgricultureInHa > totalFarmAreaInHa_trr
df_area_comparing_c3 <- df_area %>% filter(
  totalAreaUsedForAgricultureInHa_trr > totalFarmAreaInHa_trr)
summary(duplicated(df_area_comparing_c3$surveyID))

#Removing variables
rm(df_area_comparing_c1,df_area_comparing_c2,df_area_comparing_c3,df_area)



#' ========================================================================
#  Assessing quantity variables -------------------------------------------
#' ========================================================================

#' ------------------------------------------------------------------------
## Organizing data ------------------------

# Vars on main sheet
base_quan <- df %>%
  select(c(surveyID,X_uuid,typeOfCoffeeProduced,coffeeProductionMeasurementUnit,
           amountOfCoffeeProducedLastHarvest))
base_quan <- standardize_weights_prod(base_quan,dictionary)


# Vars on sales_proces_repeat_typ sheet
sales_process_repeat_df  <- sales_process_repeat_df %>% rename(
  typeCoffeeSoldToBuyer = form1,amountSold = saleAmountInUnits)
sales_proces_repeat_quan <- sales_process_repeat_df %>%
  select(c('_submission__uuid',typeCoffeeSoldToBuyer,amountSold,salesMeasurementUnit))
sales_proces_repeat_quan <- standardize_weights(sales_proces_repeat_quan,sales_process_repeat_dict)
names(sales_proces_repeat_quan)


# Joining to have a better overview
df_quan <- full_join(base_quan, sales_proces_repeat_quan,  by = c("X_uuid"="_submission__uuid"))
names(df_quan)
df_quan <- df_quan %>%
  group_by(surveyID) %>%
  mutate(totalSoldKgGreen = sum(amountSoldInKgGreen, na.rm = TRUE)) %>%
  ungroup()

# Removing variables
rm(base_quan,sales_proces_repeat_quan)



#' ------------------------------------------------------------------------
## Comparing quantities ------------------------

# Checking condition fulfillment
df_quan_comp_flag <- df_quan %>% filter(
  amountOfCoffeeProducedLastHarvestInKgGreen < (totalSoldKgGreen-0.01))
cortd_comparing <- c(
  "2023-10-03-T-332","2023-10-03-T-36","2023-10-03-T-997","2023-10-04-T-410","2023-10-04-T-661","2023-10-04-T-915",
  "2023-10-05-T-23-X-957","2023-10-05-T-462-X-647","2023-10-05-T-484-X-543","2023-10-05-T-643-X-367","2023-10-06-T-559-X-800","2023-10-06-T-593-X-673",
  "2023-10-06-T-797-X-251","2023-10-07-T-224-X-253","2023-10-07-T-769-X-887","2023-10-09-T-111-X-294","2023-10-09-T-160-X-518","2023-10-09-T-463-X-227",
  "2023-10-09-T-599-X-661","2023-10-09-T-926-X-561","2023-10-11-T-22-X-720","2023-10-11-T-844-X-198","2023-10-14-T-235-X-140","2023-10-14-T-286-X-989",
  "2023-10-17-T-193-X-397","2023-10-17-T-478-X-65","2023-10-18-T-301-X-12","2023-10-18-T-350-X-135","2023-10-18-T-402-X-931","2023-10-19-T-382-X-599",
  "2023-10-20-T-305-X-223","2023-10-26-T-211-X-4417","2023-10-27-T-2014-X-9914","2023-10-27-T-8218-X-2768","2023-11-02-T-7561-X-4341","2023-11-04-T-7525-X-65",
  "2023-11-04-T-882-X-208","2023-11-06-T-32-X-21","2023-11-06-T-340-X-234","2023-11-06-T-353-X-657","2023-11-06-T-673-X-892","2023-11-06-T-774-X-154",
  "2023-11-06-T-867-X-395","2023-11-07-T-987-X-731","2023-11-09-T-645-X-796","2023-11-08-T-104-X-401","2023-10-17-T-659-X-960","2023-11-09-T-499-X-407")
df_quan_comp_flag <- df_quan_comp_flag[!(df_quan_comp_flag$surveyID %in% cortd_comparing), ]

# Removing variables
rm(df_quan_comp_flag,cortd_comparing)


#' ------------------------------------------------------------------------
## Outliers ------------------------

# Separating observations with outliers (based on quality check)
df_quan_out <- df %>% #base_df
  select(c(surveyID,X_uuid,amountOfCoffeeProducedLastHarvestInKgGreen,amountSoldInKgGreen_typ))

vars_check <- c("amountOfCoffeeProducedLastHarvestInKgGreen","amountSoldInKgGreen_typ")
## Loop by outlier variable
for(v in vars_check){
  df_quan_out[[paste0(v, "_outlier")]] <-
    !is.na(df_quan_out[[v]]) & (
      df_quan_out[[v]] < as.numeric(results$lower_bound[[v]]) |
        df_quan_out[[v]] > as.numeric(results$upper_bound[[v]])
    )
}
## Keeping only rows with at least 1 outlier
flag             <- rowSums(df_quan_out[paste0(vars_check, "_outlier")]) > 0
df_quan_outliers <- df_quan_out[df_quan_out$surveyID %in% df_quan_out$surveyID[flag], ]

# Evaluating cases with any outlier
results[["upper_bound"]][["amountOfCoffeeProducedLastHarvestInKgGreen"]]
results[["upper_bound"]][["amountSoldInKgGreen_typ"]]
corrected_outliers <- c(
  "2023-10-18-T-790-X-517","2023-10-28-T-5456-X-4396","2023-10-19-T-72-X-967",
  "2023-10-18-T-190-X-580","2023-10-17-T-662-X-771","2023-10-18-T-722-X-397",
  "2023-10-17-T-973-X-368")

## Deleting variables
rm(vars_check,flag,df_area_outliers,corrected_outliers)



#' ========================================================================
#  Assessing number of plants variables -----------------------------------
#' ========================================================================

#' ------------------------------------------------------------------------
## Outliers ------------------------
#' The number of plants per hectare was checked to ensure it fell between
#' 2,334 (2 m × 1.5 m) and 7,000 (1 m × 1 m), and adjustments were made in the
#' Excel. Virtually, all outliers correspond to large coffee-growing areas.

# Separating observations with outliers (based on quality check)
df_plant <- df %>% #base_df
  select(c(
    surveyID,X_uuid,totalCoffeePlantsOnFarm,numberProductivePlants,
    coffeeAreaLastHarvestInHa_trr,spacingBetweenPlantsInMt,spacingBetweenRowsInMt,
    spacingBetweenRows,spacingBetweenRowsUnit,spacingBetweenPlants,spacingBetweenPlantsUnit
  ))

vars_check <- c("totalCoffeePlantsOnFarm","numberProductivePlants")
# Loop by outlier variable
for(v in vars_check){
  ## Numbers
  IQR_value <- IQR(df_plant[[v]], na.rm = TRUE)
  Q1        <- quantile(df_plant[[v]], 0.25, na.rm = TRUE)
  Q3        <- quantile(df_plant[[v]], 0.75, na.rm = TRUE)
  lower_bound <- Q1 - 3 * IQR_value
  upper_bound <- Q3 + 3 * IQR_value

  ## Flagging
  df_plant[[paste0(v, "_outlier")]] <-
    !is.na(df_plant[[v]]) & (
      df_plant[[v]] < lower_bound | df_plant[[v]] > upper_bound)
}
# Keeping only rows with at least 1 outlier
flag         <- rowSums(df_plant[paste0(vars_check, "_outlier")]) > 0
df_plant_out <- df_plant[df_plant$surveyID %in% df_plant$surveyID[flag], ]

# Deleting variables
rm(df_plant,vars_check,v,IQR_value,Q1,Q3,lower_bound,upper_bound,flag,df_plant_out)



#' ========================================================================
#  Assessing density of plants --------------------------------------------
#' ========================================================================

#' ------------------------------------------------------------------------
## Outliers ------------------------

#Separating observations with outliers (based on quality check)
df_dens_out <- df %>% select(c(
  surveyID,X_uuid,densityPlantsPerHa,totalCoffeePlantsOnFarm,numberProductivePlants,
  coffeeAreaLastHarvestInHa_trr,spacingBetweenPlantsInMt,spacingBetweenRowsInMt,
  spacingBetweenRows,spacingBetweenRowsUnit,spacingBetweenPlants,spacingBetweenPlantsUnit,
  amountOfCoffeeProducedLastHarvestInKgGreen,amountSoldInKgGreen_typ))

vars_check <- c("densityPlantsPerHa")
## Loop by outlier variable
for(v in vars_check){
  df_dens_out[[paste0(v, "_outlier")]] <-
    !is.na(df_dens_out[[v]]) & (
      df_dens_out[[v]] < as.numeric(results$lower_bound[[v]]) |
        df_dens_out[[v]] > as.numeric(results$upper_bound[[v]])
    )
}
## Keeping only rows with at least 1 outlier
flag             <- rowSums(df_dens_out[paste0(vars_check, "_outlier")]) > 0
df_dens_outliers <- df_dens_out[df_dens_out$surveyID %in% df_dens_out$surveyID[flag], ]


# Evaluating cases with any outlier
results[["upper_bound"]][["densityPlantsPerHa"]]

## Deleting variables
rm(df_dens_out,df_dens_outliers,flag,v,vars_check)



#' ========================================================================
#  Assessing yield per ha -------------------------------------------------
#' ========================================================================

#' ------------------------------------------------------------------------
## Outliers ------------------------

#Separating observations with outliers (based on quality check)
df_yield_out <- df %>% select(c(
  surveyID,X_uuid,yieldKgPerHa,coffeeAreaLastHarvestInHa_trr,
  totalCoffeePlantsOnFarm,numberProductivePlants,
  amountOfCoffeeProducedLastHarvestInKgGreen,amountSoldInKgGreen_typ))

vars_check <- c("yieldKgPerHa")
## Loop by outlier variable
for(v in vars_check){
  df_yield_out[[paste0(v, "_outlier")]] <-
    !is.na(df_yield_out[[v]]) & (
      df_yield_out[[v]] < as.numeric(results$lower_bound[[v]]) |
        df_yield_out[[v]] > as.numeric(results$upper_bound[[v]])
    )
}
## Keeping only rows with at least 1 outlier
flag             <- rowSums(df_yield_out[paste0(vars_check, "_outlier")]) > 0
df_yield_outliers <- df_yield_out[df_yield_out$surveyID %in% df_yield_out$surveyID[flag], ]


# Evaluating cases with any outlier
results[["upper_bound"]][["yieldKgPerHa"]]

## Deleting variables
rm(df_yield_out,df_yield_outliers,corrected_outliers,coherent_outliers,flag,v,vars_check)



#' ========================================================================
#  Assessing min and max price --------------------------------------------
#' ========================================================================

#' ------------------------------------------------------------------------
## Comparing prices ------------------------

df_price_comp <- df %>% select(c(
  surveyID,minimumReceivedPriceForCoffeeInKgGreen_typ,maximumReceivedPriceForCoffeeInKgGreen_typ)) %>%
  filter(minimumReceivedPriceForCoffeeInKgGreen_typ>maximumReceivedPriceForCoffeeInKgGreen_typ)

rm(df_price_comp)


#' ------------------------------------------------------------------------
## Outliers ------------------------

#Separating observations with outliers (based on quality check)
df_price_out <- df %>% select(c(
  surveyID,X_uuid,minimumReceivedPriceForCoffeeInKgGreen_typ,maximumReceivedPriceForCoffeeInKgGreen_typ,
  amountOfCoffeeProducedLastHarvestInKgGreen,amountSoldInKgGreen_typ))

vars_check <- c("minimumReceivedPriceForCoffeeInKgGreen_typ","maximumReceivedPriceForCoffeeInKgGreen_typ")
## Loop by outlier variable
for(v in vars_check){
  df_price_out[[paste0(v, "_outlier")]] <-
    !is.na(df_price_out[[v]]) & (
      df_price_out[[v]] < as.numeric(results$lower_bound[[v]]) |
        df_price_out[[v]] > as.numeric(results$upper_bound[[v]])
    )
}
## Keeping only rows with at least 1 outlier
flag              <- rowSums(df_price_out[paste0(vars_check, "_outlier")]) > 0
df_price_outliers <- df_price_out[df_price_out$surveyID %in% df_price_out$surveyID[flag], ]

# ## sales_process_repeat
# sales_process_repeat_df    <- convert_numeric_columns(sales_process_repeat_df,sales_process_repeat_dict)
# sales_process_repeat_price <- standardize_prices(sales_process_repeat_df,sales_process_repeat_dict)
# sales_process_repeat_price$surveyID <- sales_process_repeat_price$surveyID
# 
# ## Joining original prices
# sales_process_repeat_price <- sales_process_repeat_price %>% select(
#   surveyID,methodOfCoffeeSale,amountCoffeeSold,salesMeasurementUnit,
#   minimumReceivedPriceForCoffee,priceUnitForMinimumPrice,minimumReceivedPriceForCoffeeInKgGreen,
#   maximumReceivedPriceForCoffee,priceUnitForMaximumPrice,maximumReceivedPriceForCoffeeInKgGreen
#   )
# df_price_outliers <- left_join(df_price_outliers,sales_process_repeat_price, by="surveyID")
# 
# 
# # Evaluating cases with any outlier
# results[["upper_bound"]][["minimumReceivedPriceForCoffeeInKgGreen_typ"]]
# results[["upper_bound"]][["maximumReceivedPriceForCoffeeInKgGreen_typ"]]
# 
# ## Deleting variables
# rm(df_price_out,df_price_outliers,corrected_outliers,coherent_outliers,flag,v,vars_check)

