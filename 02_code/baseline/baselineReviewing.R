#' -----------------------------------------------------------------------------
#' IMPORTANT!
#' Run the beginning of baselineProcessingCode.R before running this script.
#' End on line 61: base_df <- read_excel(data_path, sheet = "mod_combined")
#' -----------------------------------------------------------------------------
#' 
#' This script reviews variables in "01_data/raw/baseline/baselineAgg.csv", including 
#' missing values, outliers, and violations of expected numeric order relationships.
#' The main input for these corrections is: "03_tables/baseline/baselineQualityCheck.RData"


# Loading data
df             <- read.csv("01_data/processed/baseline/baselineAgg.csv")
df$ID_ENCUESTA <- df$surveyID
results        <- readRDS("03_tables/baseline/baselineQualityCheck.RData")



#' ========================================================================
#  Assessing area variables -----------------------------------------------
#' ========================================================================

# Vars on main sheet
base_area <- df %>%
  select(c(ID_ENCUESTA, totalParcelAreaInHa_trr,
           totalAreaUsedForAgriculture,measurementUnitOfArea,totalAreaUsedForAgricultureInHa,
           coffeeAreaLastHarvest,coffeeAreaMeasurementUnit,coffeeAreaLastHarvestInHa,
           productiveCoffeeAreaLastHarvest,productiveCoffeeAreaUnit))


# Vars on area_terr sheet
area_terr_area <- area_terr_df %>%
  select(c(ID_ENCUESTA,totalParcelArea,parcelAreaMeasurementUnit))


# Joining to have a better overview
df_area <- full_join(base_area, area_terr_area,  by = "ID_ENCUESTA")
df_area <- df_area %>%
  dplyr::select(ID_ENCUESTA,
                totalParcelArea,parcelAreaMeasurementUnit,totalParcelAreaInHa_trr,
                totalAreaUsedForAgriculture,measurementUnitOfArea,totalAreaUsedForAgricultureInHa,
                coffeeAreaLastHarvest,coffeeAreaMeasurementUnit,coffeeAreaLastHarvestInHa,
                productiveCoffeeAreaLastHarvest,productiveCoffeeAreaUnit)
df_area$totalParcelArea <- as.numeric(df_area$totalParcelArea)
rm(area_terr_area)


#' ------------------------------------------------------------------------
## Missing values ------------------

df_area_missing <- df_area %>%
  dplyr::filter(if_any(all_of(names(df_area)), is.na))
colSums(is.na(df_area_missing))
#' There are 265 observations without values for any area variable.
colSums(is.na(base_area))
#' The rest of observations have values in all area variables. There is no need
#' to impute missing values between variables.
rm(df_area_missing,base_area)


#' ------------------------------------------------------------------------
## Outliers ------------------------
# Separating observations with outliers (based on quality check)
vars_check <- c("totalParcelAreaInHa_trr","totalAreaUsedForAgricultureInHa","coffeeAreaLastHarvestInHa")
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
df_area_outliers <- df_area[df_area$ID_ENCUESTA %in% df$ID_ENCUESTA[flag], ]
## Adding dummies to identify which var has the outlier
df_area_outliers <- merge(
  df_area_outliers,
  df[, c("ID_ENCUESTA", paste0(vars_check, "_outlier"))],
  by = "ID_ENCUESTA",  all.x = TRUE
)


# Evaluating cases with any outlier
## Case 1: only coffeeAreaLastHarvestInHa_outlier=TRUE
df_area_outliers_c1 <- df_area_outliers %>% filter(
    coffeeAreaLastHarvestInHa_outlier       == TRUE  &
    totalAreaUsedForAgricultureInHa_outlier == FALSE &
    totalParcelAreaInHa_trr_outlier         == FALSE )
summary(duplicated(df_area_outliers_c1$ID_ENCUESTA))
results[["upper_bound"]][["coffeeAreaLastHarvestInHa"]]
corrected_outliers <- c("2023-11-08-T-336-X-669","2023-10-05-T-778-X-313","2023-10-30-T-2455-X-2066",
                        "2023-10-21-T-188-X-319")
coherent_outliers <- c(
  "2023-10-06-T-29-X-262","2023-10-07-T-224-X-253","2023-10-14-T-863-X-221","2023-10-18-T-402-X-931","2023-10-23-T-285-X-42","2023-10-23-T-543-X-913",
  "2023-10-27-T-8218-X-2768","2023-10-30-T-1899-X-6021","2023-11-02-T-5957-X-7411","2023-11-09-T-499-X-407","2023-10-06-T-294-X-685","2023-10-18-T-350-X-135",
  "2023-10-18-T-551-X-811","2023-10-19-T-520-X-976","2023-10-19-T-925-X-541","2023-10-26-T-5986-X-6703","2023-10-26-T-6452-X-3280","2023-10-27-T-5219-X-3944",
  "2023-10-28-T-5456-X-4396","2023-10-28-T-590-X-1900","2023-10-28-T-8301-X-2441","2023-10-30-T-1351-X-3126","2023-10-30-T-1529-X-5250","2023-10-30-T-5657-X-2691",
  "2023-11-04-T-312-X-272","2023-11-04-T-5605-X-5944","2023-11-09-T-925-X-316")
df_area_outliers_c1 <- df_area_outliers_c1[!(df_area_outliers_c1$ID_ENCUESTA %in% c(corrected_outliers,coherent_outliers)), ]

## Case 2: only totalAreaUsedForAgricultureInHa_outlier=TRUE
df_area_outliers_c2 <- df_area_outliers %>% filter(
    coffeeAreaLastHarvestInHa_outlier       == FALSE &
    totalAreaUsedForAgricultureInHa_outlier == TRUE  &
    totalParcelAreaInHa_trr_outlier         == FALSE )
summary(duplicated(df_area_outliers_c2$ID_ENCUESTA))
results[["upper_bound"]][["totalAreaUsedForAgricultureInHa"]]
corrected_outliers <- c("2023-10-16-T-694-X-362","2023-10-19-T-347-X-954","2023-10-27-T-7525-X-2130",
                        "2023-10-28-T-5335-X-8873")
df_area_outliers_c2 <- df_area_outliers_c2[!(df_area_outliers_c2$ID_ENCUESTA %in% c(corrected_outliers,coherent_outliers)), ]

## Case 3: only totalParcelAreaInHa_trr_outlier=TRUE
df_area_outliers_c3 <- df_area_outliers %>% filter(
    coffeeAreaLastHarvestInHa_outlier       == FALSE &
    totalAreaUsedForAgricultureInHa_outlier == FALSE &
    totalParcelAreaInHa_trr_outlier         == TRUE  )
summary(duplicated(df_area_outliers_c3$ID_ENCUESTA))
results[["upper_bound"]][["totalParcelAreaInHa_trr"]]
corrected_outliers <- c("2023-10-11-T-812-X-70","2023-11-08-T-864-X-666")
coherent_outliers  <- c("2023-10-30-T-5368-X-952","2023-10-30-T-7439-X-2588","2023-11-06-T-507-X-86",
                        "2023-11-08-T-257-X-451")
df_area_outliers_c3 <- df_area_outliers_c3[!(df_area_outliers_c3$ID_ENCUESTA %in% c(corrected_outliers,coherent_outliers)), ]

## Case 4: coffeeAreaLastHarvestInHa_outlier=TRUE & totalAreaUsedForAgricultureInHa_outlier=TRUE
df_area_outliers_c4 <- df_area_outliers %>% filter(
    coffeeAreaLastHarvestInHa_outlier       == TRUE &
    totalAreaUsedForAgricultureInHa_outlier == TRUE &
    totalParcelAreaInHa_trr_outlier         == FALSE)
#' No observations

## Case 5: coffeeAreaLastHarvestInHa_outlier=TRUE & totalParcelAreaInHa_trr_outlier=TRUE
df_area_outliers_c5 <- df_area_outliers %>% filter(
    coffeeAreaLastHarvestInHa_outlier       == TRUE  &
    totalAreaUsedForAgricultureInHa_outlier == FALSE &
    totalParcelAreaInHa_trr_outlier         == TRUE )
summary(duplicated(df_area_outliers_c5$ID_ENCUESTA))
coherent_outliers  <- c("2023-10-30-T-7901-X-9723","2023-11-04-T-79-X-153","2023-11-07-T-196-X-464")
df_area_outliers_c5 <- df_area_outliers_c5[!(df_area_outliers_c5$ID_ENCUESTA %in% c(corrected_outliers,coherent_outliers)), ]

## Case 6: totalAreaUsedForAgricultureInHa_outlier=TRUE & totalParcelAreaInHa_trr_outlier=TRUE
df_area_outliers_c6 <- df_area_outliers %>% filter(
    coffeeAreaLastHarvestInHa_outlier       == FALSE &
    totalAreaUsedForAgricultureInHa_outlier == TRUE  &
    totalParcelAreaInHa_trr_outlier         == TRUE )
summary(duplicated(df_area_outliers_c6$ID_ENCUESTA))
corrected_outliers <- c("2023-11-02-T-1127-X-9406","2023-10-06-T-787-X-371")
coherent_outliers  <- c(
  "2023-10-11-T-338-X-589","2023-10-30-T-8181-X-7369","2023-10-12-T-973-X-976","2023-10-30-T-9979-X-7324","2023-10-04-T-781","2023-10-11-T-281-X-96",
  "2023-10-21-T-340-X-609","2023-10-21-T-224-X-560","2023-10-18-T-107-X-220","2023-10-27-T-9424-X-4126","2023-10-28-T-9976-X-4731","2023-10-05-T-102-X-917",
  "2023-10-12-T-881-X-411","2023-10-05-T-484-X-543","2023-10-16-T-296-X-906","2023-10-04-T-161","2023-10-17-T-609-X-335","2023-10-28-T-6375-X-7226",
  "2023-11-07-T-238-X-192","2023-11-09-T-623-X-239","2023-11-07-T-26-X-315","2023-10-19-T-892-X-246","2023-10-19-T-835-X-616","2023-10-05-T-295-X-88",
  "2023-10-20-T-617-X-618","2023-10-06-T-891-X-140","2023-10-30-T-997-X-8093","2023-10-26-T-3027-X-3937","2023-10-16-T-42-X-554","2023-10-05-T-245-X-367")
df_area_outliers_c6 <- df_area_outliers_c6[!(df_area_outliers_c6$ID_ENCUESTA %in% c(corrected_outliers,coherent_outliers)), ]

## Case 7: all=TRUE
df_area_outliers_c7 <- df_area_outliers %>% filter(
    coffeeAreaLastHarvestInHa_outlier       == TRUE  &
    totalAreaUsedForAgricultureInHa_outlier == TRUE &
    totalParcelAreaInHa_trr_outlier         == TRUE )
summary(duplicated(df_area_outliers_c7$ID_ENCUESTA))
corrected_outliers <- c("2023-10-21-T-871-X-106","2023-10-06-T-375-X-292","2023-10-11-T-30-X-574")
coherent_outliers  <- c(
  "2023-11-02-T-5494-X-2657","2023-10-27-T-849-X-249","2023-10-21-T-508-X-455","2023-10-27-T-276-X-8288","2023-10-27-T-3443-X-5030","2023-10-30-T-892-X-5350",
  "2023-10-26-T-4117-X-622","2023-10-28-T-4882-X-1578","2023-10-26-T-404-X-1619","2023-10-18-T-365-X-326","2023-10-07-T-986-X-326","2023-10-27-T-2212-X-6213",
  "2023-10-23-T-353-X-716","2023-11-07-T-559-X-736","2023-11-07-T-245-X-63","2023-10-06-T-880-X-873","2023-10-30-T-5140-X-330","2023-10-11-T-347-X-936",
  "2023-11-06-T-157-X-625","2023-10-26-T-9490-X-4301","2023-10-23-T-406-X-207","2023-10-19-T-665-X-248","2023-11-09-T-645-X-796","2023-11-06-T-655-X-804",
  "2023-10-28-T-596-X-5729","2023-10-23-T-513-X-947","2023-10-27-T-1181-X-1961","2023-10-07-T-335-X-365","2023-10-20-T-375-X-397","2023-11-02-T-2012-X-6961",
  "2023-11-04-T-428-X-512","2023-10-26-T-8115-X-3427","2023-10-30-T-1841-X-1411","2023-11-08-T-514-X-232","2023-11-09-T-656-X-642","2023-10-26-T-2590-X-8500",
  "2023-10-18-T-322-X-965","2023-11-07-T-146-X-994","2023-10-28-T-7752-X-8204","2023-10-26-T-650-X-4386","2023-10-19-T-104-X-957","2023-10-06-T-504-X-978",
  "2023-11-02-T-4070-X-6073","2023-10-09-T-125-X-176","2023-10-28-T-3559-X-6172","2023-10-26-T-7095-X-9140","2023-10-21-T-913-X-279","2023-10-19-T-49-X-782",
  "2023-10-17-T-902-X-123","2023-10-17-T-889-X-836","2023-10-14-T-134-X-280","2023-10-28-T-3129-X-6097","2023-10-26-T-5798-X-3347","2023-10-16-T-667-X-326",
  "2023-10-07-T-211-X-688","2023-10-11-T-304-X-496","2023-10-18-T-46-X-810","2023-10-26-T-9700-X-1198","2023-10-07-T-769-X-887","2023-11-02-T-5189-X-1159")
df_area_outliers_c7 <- df_area_outliers_c7[!(df_area_outliers_c7$ID_ENCUESTA %in% c(corrected_outliers,coherent_outliers)), ]

## Deleting variables
rm(vars_check,flag,df_area_outliers,df_area_outliers_c1,df_area_outliers_c2,
   df_area_outliers_c3,df_area_outliers_c4,df_area_outliers_c5,df_area_outliers_c6,
   df_area_outliers_c7,corrected_outliers,coherent_outliers)


#' ------------------------------------------------------------------------
## Comparing areas ------------------------

# Case 1: coffeeAreaLastHarvestInHa > totalAreaUsedForAgricultureInHa
df_area_comparing_c1 <- df_area %>% filter(
  coffeeAreaLastHarvestInHa > totalAreaUsedForAgricultureInHa)
summary(duplicated(df_area_comparing_c1$ID_ENCUESTA))
false_comparing <- c("2023-11-04-T-410-X-157")
cortd_comparing <- c("2023-10-03-T-104","2023-10-03-T-68","2023-10-06-T-504-X-978",
                     "2023-10-11-T-30-X-574","2023-10-17-T-303-X-191","2023-11-06-T-507-X-86",
                     "2023-11-08-T-336-X-669","2023-10-30-T-2455-X-2066")
df_area_comparing_c1 <- df_area_comparing_c1[!(df_area_comparing_c1$ID_ENCUESTA %in% c(cortd_comparing,false_comparing)), ]

# Case 2: coffeeAreaLastHarvestInHa > totalParcelAreaInHa_trr
df_area_comparing_c2 <- df_area %>% filter(
  coffeeAreaLastHarvestInHa > totalParcelAreaInHa_trr)
summary(duplicated(df_area_comparing_c2$ID_ENCUESTA))
cortd_comparing <- c("2023-11-08-T-336-X-669","2023-10-11-T-30-X-574","2023-10-06-T-134-X-681",
                     "2023-10-09-T-118-X-697","2023-10-30-T-2455-X-2066","2023-10-05-T-363-X-749",
                     "2023-10-06-T-170-X-101","2023-10-03-T-368","2023-10-03-T-99",
                     "2023-10-04-T-661","2023-10-03-T-643")
df_area_comparing_c2 <- df_area_comparing_c2[!(df_area_comparing_c2$ID_ENCUESTA %in% cortd_comparing), ]

# Case 3: totalAreaUsedForAgricultureInHa > totalParcelAreaInHa_trr
df_area_comparing_c3 <- df_area %>% filter(
  totalAreaUsedForAgricultureInHa > totalParcelAreaInHa_trr)
summary(duplicated(df_area_comparing_c3$ID_ENCUESTA))
false_comparing <- c()
cortd_comparing <- c(
  "2023-10-03-T-368","2023-10-03-T-372","2023-10-03-T-389","2023-10-03-T-525","2023-10-03-T-643","2023-10-03-T-708",
  "2023-10-03-T-99","2023-10-04-T-583","2023-10-04-T-625","2023-10-03-T-497","2023-10-04-T-661","2023-10-05-T-363-X-749",
  "2023-10-05-T-778-X-313","2023-10-05-T-888-X-179","2023-10-06-T-134-X-681","2023-10-06-T-170-X-101","2023-10-06-T-180-X-981","2023-10-06-T-316-X-491",
  "2023-10-06-T-375-X-292","2023-10-06-T-787-X-371","2023-10-07-T-722-X-380","2023-10-07-T-950-X-608","2023-10-09-T-118-X-697","2023-10-09-T-178-X-346",
  "2023-10-09-T-181-X-688","2023-10-09-T-270-X-331","2023-11-09-T-574-X-215","2023-11-08-T-197-X-859","2023-10-28-T-7560-X-6247","2023-10-28-T-5335-X-8873",
  "2023-10-28-T-485-X-4910","2023-10-27-T-7525-X-2130","2023-10-23-T-543-X-913","2023-10-21-T-871-X-106","2023-10-21-T-587-X-156","2023-10-21-T-188-X-319",
  "2023-10-21-T-106-X-837","2023-10-20-T-787-X-110","2023-10-20-T-464-X-62","2023-11-09-T-306-X-828","2023-10-20-T-442-X-214","2023-10-20-T-341-X-157",
  "2023-10-19-T-347-X-954","2023-10-18-T-984-X-562","2023-10-18-T-804-X-504","2023-10-18-T-714-X-17","2023-10-17-T-899-X-318","2023-10-17-T-675-X-673",
  "2023-10-17-T-496-X-166","2023-10-17-T-452-X-770","2023-10-16-T-694-X-362","2023-10-11-T-830-X-837","2023-10-11-T-387-X-366","2023-10-07-T-892-X-832",
  "2023-10-09-T-587-X-865","2023-10-11-T-490-X-800","2023-10-14-T-40-X-353")
df_area_comparing_c3 <- df_area_comparing_c3[!(df_area_comparing_c3$ID_ENCUESTA %in% c(cortd_comparing,false_comparing)), ]

#Removing variables
rm(df_area_comparing_c1,df_area_comparing_c2,df_area_comparing_c3,area_terr_df,
   area_terr_dict,df_area,cortd_comparing)



#' ========================================================================
#  Assessing quantity variables -------------------------------------------
#' ========================================================================

#' ------------------------------------------------------------------------
## Organizing data ------------------------

# Standardizing quantity to green kg
df_quan_comparing <- standardize_weights(df_quan_missing,sales_process_repeat_dict)
names(df_quan_comparing)
df_quan_comp_agg <- df_quan_comparing %>%
  group_by(
    ID_ENCUESTA,typeOfCoffeeProduced,coffeeProductionMeasurementUnit,amountOfCoffeeProducedLastHarvest) %>%
  summarise(
    amountSoldInKgGreen = sum(amountSoldInKgGreen, na.rm = TRUE),
    .groups = "drop"
  )
df_quan_comp_agg <- standardize_weights_prod(df_quan_comparing,dictionary)
names(df_quan_comp_agg)


#' ------------------------------------------------------------------------
## Comparing quantities ------------------------

# Checking condition fulfillment
df_quan_comp_flag <- df_quan_comp_agg %>% filter(
  amountOfCoffeeProducedLastHarvestInKgGreen < amountSoldInKgGreen)
false_comparing <- c("2023-10-21-T-508-X-455")
cortd_comparing <- c("2023-10-17-T-742-X-913","2023-10-18-T-676-X-692","2023-10-26-T-1302-X-9320",
                     "2023-10-26-T-8427-X-8270","2023-10-26-T-9216-X-5786","2023-10-03-T-332",
                     "2023-10-06-T-155-X-2","2023-10-06-T-797-X-251","2023-10-17-T-193-X-397",
                     "2023-10-17-T-792-X-372","2023-10-26-T-211-X-4417","2023-10-06-T-375-X-292")
df_quan_comp_flag <- df_quan_comp_flag[!(df_quan_comp_flag$ID_ENCUESTA %in% c(cortd_comparing,false_comparing)), ]

# Removing variables
rm(df_quan_comp_flag,false_comparing,cortd_comparing,df_quan_comparing)


#' ------------------------------------------------------------------------
## Outliers ------------------------

# Separating observations with outliers (based on quality check)
df_quan_out <- df %>% #base_df
  select(c(surveyID,amountOfCoffeeProducedLastHarvestInKgGreen,amountSoldInKgGreen_typ))

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
  "2023-10-06-T-375-X-292","2023-10-06-T-880-X-873","2023-10-06-T-504-X-978",
  "2023-10-17-T-193-X-397","2023-10-06-T-797-X-251","2023-10-17-T-681-X-44",
  "2023-10-18-T-46-X-810")
coherent_outliers <- c(
  "2023-10-26-T-4117-X-622","2023-10-26-T-404-X-1619","2023-10-27-T-1181-X-1961","2023-10-30-T-1841-X-1411","2023-11-02-T-5189-X-1159","2023-11-07-T-245-X-63",
  "2023-10-27-T-3443-X-5030","2023-10-20-T-375-X-397","2023-10-28-T-596-X-5729","2023-10-26-T-9490-X-4301","2023-10-26-T-650-X-4386","2023-10-28-T-3129-X-6097",
  "2023-10-21-T-508-X-455","2023-10-30-T-892-X-5350","2023-10-28-T-3559-X-6172","2023-11-07-T-559-X-736","2023-10-27-T-2212-X-6213","2023-10-28-T-4882-X-1578",
  "2023-10-23-T-513-X-947","2023-10-28-T-7752-X-8204","2023-10-18-T-365-X-326","2023-10-19-T-49-X-782","2023-10-27-T-8218-X-2768","2023-11-09-T-656-X-642",
  "2023-10-27-T-276-X-8288","2023-10-05-T-129-X-990","2023-10-30-T-5140-X-330","2023-10-07-T-211-X-688","2023-10-23-T-353-X-716","2023-11-07-T-994-X-600",
  "2023-11-02-T-5494-X-2657","2023-10-07-T-335-X-365","2023-10-30-T-5657-X-2691","2023-10-23-T-543-X-913","2023-10-23-T-285-X-42","2023-10-27-T-6069-X-8842",
  "2023-10-11-T-30-X-574","2023-10-06-T-134-X-681","2023-11-08-T-514-X-232","2023-10-17-T-902-X-123","2023-10-26-T-3027-X-3937","2023-10-26-T-5986-X-6703",
  "2023-10-26-T-7095-X-9140","2023-10-27-T-849-X-249","2023-10-28-T-590-X-1900","2023-10-30-T-3183-X-3566","2023-11-02-T-4070-X-6073","2023-10-06-T-29-X-262",
  "2023-10-23-T-406-X-207","2023-10-17-T-496-X-166","2023-10-11-T-304-X-496","2023-10-28-T-1723-X-9760","2023-10-28-T-5456-X-4396","2023-10-21-T-323-X-214",
  "2023-10-11-T-338-X-589","2023-11-04-T-23-X-4662","2023-10-26-T-2590-X-8500","2023-10-19-T-136-X-862","2023-10-06-T-457-X-755","2023-10-16-T-139-X-588",
  "2023-11-04-T-886-X-182","2023-10-30-T-7901-X-9723","2023-10-27-T-1783-X-2185","2023-10-05-T-778-X-313","2023-10-16-T-667-X-326","2023-10-19-T-178-X-800",
  "2023-11-02-T-9881-X-549","2023-10-23-T-921-X-511","2023-10-19-T-665-X-248","2023-10-30-T-1351-X-3126","2023-11-04-T-428-X-512","2023-11-06-T-157-X-625",
  "2023-11-09-T-645-X-796","2023-10-26-T-5798-X-3347","2023-10-26-T-6452-X-3280","2023-10-26-T-7265-X-4121","2023-10-28-T-5335-X-8873","2023-10-30-T-1899-X-6021",
  "2023-10-28-T-3402-X-6197","2023-11-02-T-1127-X-9406","2023-10-26-T-3540-X-2649","2023-10-21-T-871-X-106","2023-10-26-T-5662-X-1743","2023-10-20-T-978-X-436",
  "2023-11-04-T-312-X-272","2023-10-26-T-6854-X-8114","2023-10-28-T-1620-X-3230","2023-10-28-T-3671-X-2734","2023-11-04-T-5605-X-5944","2023-10-27-T-5219-X-3944",
  "2023-11-07-T-196-X-464","2023-11-02-T-2012-X-6961","2023-10-18-T-402-X-931","2023-10-21-T-340-X-609","2023-10-27-T-668-X-5678","2023-10-28-T-9976-X-4731",
  "2023-10-30-T-5368-X-952")
df_quan_outliers <- df_quan_outliers[!(df_quan_outliers$surveyID %in% c(corrected_outliers,coherent_outliers)), ]

## Deleting variables
rm(vars_check,flag,df_area_outliers,df_area_outliers_c1,df_area_outliers_c2,
   df_area_outliers_c3,df_area_outliers_c4,df_area_outliers_c5,df_area_outliers_c6,
   df_area_outliers_c7,corrected_outliers,coherent_outliers)



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
    surveyID,coffeeAreaLastHarvest,coffeeAreaMeasurementUnit,
    productiveCoffeeAreaLastHarvest,productiveCoffeeAreaUnit,
    totalCoffeePlantsOnFarm,spacingBetweenPlantsInMt,spacingBetweenRowsInMt,
    numberProductivePlants))

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
    surveyID,densityPlantsPerHa,coffeeAreaLastHarvest,coffeeAreaMeasurementUnit,
    productiveCoffeeAreaLastHarvest,productiveCoffeeAreaUnit,
    totalCoffeePlantsOnFarm,spacingBetweenPlantsInMt,spacingBetweenRowsInMt,
    numberProductivePlants,coffeeAreaLastHarvestInHa,
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
results[["lower_bound"]][["densityPlantsPerHa"]]
corrected_outliers <- c(
  "2023-10-09-T-270-X-331","2023-10-09-T-554-X-358","2023-11-02-T-1774-X-8638","2023-10-09-T-80-X-215","2023-10-18-T-366-X-173","2023-10-27-T-43-X-8082",
  "2023-10-09-T-565-X-572","2023-10-14-T-296-X-487","2023-11-06-T-607-X-93","2023-10-07-T-234-X-268","2023-10-09-T-992-X-945","2023-10-04-T-509",
  "2023-10-04-T-987","2023-10-04-T-75","2023-10-06-T-559-X-800","2023-10-28-T-6333-X-7426","2023-10-11-T-387-X-366","2023-11-04-T-87-X-817",
  "2023-10-03-T-740","2023-10-07-T-944-X-196","2023-10-09-T-118-X-697","2023-10-16-T-865-X-338","2023-10-18-T-676-X-692","2023-11-08-T-525-X-205",
  "2023-11-09-T-264-X-25","2023-10-09-T-808-X-300","2023-10-09-T-972-X-965","2023-10-14-T-495-X-373","2023-11-08-T-168-X-592","2023-10-09-T-882-X-371",
  "2023-10-03-T-708","2023-10-03-T-980","2023-10-19-T-746-X-460","2023-11-07-T-705-X-64","2023-10-03-T-547","2023-11-07-T-215-X-908",
  "2023-11-06-T-677-X-658","2023-10-07-T-299-X-207","2023-10-11-T-721-X-118","2023-11-07-T-882-X-779","2023-11-08-T-795-X-61","2023-10-03-T-615",
  "2023-10-04-T-483","2023-10-04-T-959","2023-10-05-T-71-X-879","2023-10-09-T-461-X-687","2023-10-09-T-680-X-847","2023-10-12-T-604-X-737",
  "2023-10-12-T-619-X-516","2023-10-16-T-65-X-318","2023-10-18-T-359-X-175","2023-10-18-T-641-X-968","2023-10-20-T-840-X-71","2023-10-23-T-94-X-770",
  "2023-11-08-T-780-X-176","2023-10-06-T-222-X-989","2023-10-09-T-747-X-960","2023-10-09-T-268-X-457","2023-10-09-T-984-X-845","2023-10-16-T-907-X-382",
  "2023-10-21-T-871-X-106","2023-11-04-T-410-X-157","2023-10-04-T-273","2023-11-02-T-5609-X-834","2023-10-16-T-553-X-916","2023-11-04-T-832-X-618",
  "2023-10-06-T-488-X-51","2023-11-06-T-507-X-86","2023-10-26-T-8067-X-5054","2023-11-04-T-368-X-144","2023-11-04-T-784-X-198","2023-11-06-T-762-X-80",
  "2023-10-05-T-295-X-88","2023-11-04-T-813-X-160","2023-10-09-T-599-X-661","2023-10-11-T-170-X-673","2023-10-14-T-173-X-909","2023-11-08-T-601-X-662",
  "2023-10-07-T-381-X-374","2023-10-07-T-722-X-380","2023-11-04-T-979-X-38","2023-11-07-T-33-X-520","2023-10-26-T-4578-X-4807","2023-10-27-T-8315-X-422",
  "2023-10-11-T-347-X-936")
coherent_outliers <- c(
  "2023-10-03-T-622","2023-10-05-T-531-X-978","2023-10-06-T-316-X-491","2023-10-09-T-928-X-517","2023-10-09-T-990-X-243","2023-10-03-T-628",
  "2023-10-04-T-618","2023-10-04-T-812","2023-10-05-T-499-X-918","2023-10-09-T-181-X-688","2023-10-20-T-893-X-354","2023-10-03-T-114",
  "2023-10-05-T-476-X-712","2023-10-09-T-434-X-426","2023-10-09-T-853-X-689","2023-10-17-T-350-X-817","2023-10-18-T-523-X-872","2023-10-12-T-462-X-6",
  "2023-10-04-T-224","2023-10-09-T-277-X-222","2023-10-20-T-527-X-561","2023-10-16-T-771-X-790","2023-10-30-T-947-X-8969","2023-10-19-T-675-X-577",
  "2023-10-16-T-575-X-948","2023-10-05-T-960-X-16","2023-10-03-T-102","2023-10-03-T-329","2023-10-03-T-525","2023-10-03-T-627",
  "2023-10-04-T-583","2023-10-04-T-765","2023-10-05-T-392-X-217","2023-10-11-T-490-X-800","2023-10-12-T-57-X-109","2023-10-16-T-950-X-612",
  "2023-10-20-T-24-X-890","2023-10-23-T-929-X-97","2023-11-04-T-835-X-754","2023-10-27-T-9688-X-2920","2023-11-02-T-6250-X-6402","2023-11-08-T-212-X-503",
  "2023-11-08-T-604-X-331","2023-11-06-T-410-X-904","2023-11-09-T-310-X-477","2023-10-05-T-778-X-533","2023-11-06-T-226-X-995","2023-10-30-T-6842-X-7600"
  )
df_dens_outliers <- df_dens_outliers[!(df_dens_outliers$surveyID %in% c(corrected_outliers,coherent_outliers)), ]

## Deleting variables
rm(df_dens_out,df_dens_outliers,corrected_outliers,coherent_outliers,flag,v,vars_check)


#' ========================================================================
#  Assessing yield per ha -------------------------------------------------
#' ========================================================================

#' ------------------------------------------------------------------------
## Outliers ------------------------

#Separating observations with outliers (based on quality check)
df_yield_out <- df %>% select(c(
  surveyID,yieldKgPerHa,coffeeAreaLastHarvest,coffeeAreaMeasurementUnit,
  productiveCoffeeAreaLastHarvest,productiveCoffeeAreaUnit,
  totalCoffeePlantsOnFarm,numberProductivePlants,coffeeAreaLastHarvestInHa,
  amountOfCoffeeProducedLastHarvest,coffeeProductionMeasurementUnit,typeOfCoffeeProduced,
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
corrected_outliers <- c("2023-11-04-T-301-X-286","2023-10-09-T-270-X-331",
                        "2023-10-09-T-554-X-358","2023-10-27-T-43-X-8082")
coherent_outliers <- c(
  "2023-10-16-T-950-X-612","2023-10-16-T-139-X-588","2023-11-04-T-299-X-353","2023-10-21-T-910-X-216","2023-10-03-T-104","2023-10-20-T-978-X-436",
  "2023-10-26-T-5786-X-1107","2023-10-04-T-666","2023-10-28-T-2535-X-2227","2023-10-17-T-792-X-372","2023-10-17-T-681-X-44","2023-11-08-T-143-X-217")
df_yield_outliers <- df_yield_outliers[!(df_yield_outliers$surveyID %in% c(corrected_outliers,coherent_outliers)), ]

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
  surveyID,minimumReceivedPriceForCoffeeInKgGreen_typ,maximumReceivedPriceForCoffeeInKgGreen_typ,
  amountOfCoffeeProducedLastHarvest,coffeeProductionMeasurementUnit,typeOfCoffeeProduced,
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

## sales_process_repeat
sales_process_repeat_df    <- convert_numeric_columns(sales_process_repeat_df,sales_process_repeat_dict)
sales_process_repeat_price <- standardize_prices(sales_process_repeat_df,sales_process_repeat_dict)
sales_process_repeat_price$surveyID <- sales_process_repeat_price$ID_ENCUESTA

## Joining original prices
sales_process_repeat_price <- sales_process_repeat_price %>% select(
  surveyID,methodOfCoffeeSale,amountCoffeeSold,salesMeasurementUnit,
  minimumReceivedPriceForCoffee,priceUnitForMinimumPrice,minimumReceivedPriceForCoffeeInKgGreen,
  maximumReceivedPriceForCoffee,priceUnitForMaximumPrice,maximumReceivedPriceForCoffeeInKgGreen
  )
df_price_outliers <- left_join(df_price_outliers,sales_process_repeat_price, by="surveyID")


# Evaluating cases with any outlier
results[["upper_bound"]][["minimumReceivedPriceForCoffeeInKgGreen_typ"]]
results[["upper_bound"]][["maximumReceivedPriceForCoffeeInKgGreen_typ"]]

## Deleting variables
rm(df_price_out,df_price_outliers,corrected_outliers,coherent_outliers,flag,v,vars_check)

