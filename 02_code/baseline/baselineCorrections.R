#' Se hizo desde el raw, con criterios para outliers de IQR ...
#' Se debe rehacer si se va a cambiar el criterio
#' Las correcciones son hechas a manito y se deja la trazabilidad en este script


#' ------------------------------------------------------------------------
# Main sheet --------------------------------------------------------------
#' ------------------------------------------------------------------------

# Reading Excel
#changes_df  <- read_excel(changes_path, sheet = "mod_combined")

# Calling data
#' Si se va a correr lo que está comentado, se debe llamar el agg para ver las 
#' variables agregadas. Si no, se corre desde baselineProcessingCode.
#base_df      <- read.csv("01_data/processed/baseline/baselineAgg.csv")
#base_df$ID_ENCUESTA <- base_df$surveyID
#results <- readRDS("03_tables/baseline/baselineQualityCheck.RData")


#' ### Production (temporal) ------------------------

# Nuevos valores
updates <- data.frame(
  ID_ENCUESTA = c(
    "2023-10-03-T-332","2023-10-06-T-155-X-2","2023-10-06-T-375-X-292","2023-10-06-T-797-X-251",
    "2023-10-17-T-193-X-397","2023-10-17-T-742-X-913","2023-10-17-T-792-X-372","2023-10-18-T-676-X-692",
    "2023-10-26-T-1302-X-9320","2023-10-26-T-211-X-4417","2023-10-26-T-8427-X-8270","2023-10-26-T-9216-X-5786"),
  new_amount = c(
    118.75,24.75,791.36,1098.44,
    2559.01,11.61,1542.12,58.001,
    371.23,545.48,382.84,87.01))
# Actualizar variable
base_df <- base_df %>%
  left_join(updates, by = "ID_ENCUESTA") %>%
  mutate(
    amountOfCoffeeProducedLastHarvest =
      ifelse(!is.na(new_amount),new_amount,amountOfCoffeeProducedLastHarvest)) %>%
  select(-new_amount)


## Assessing area variables -----------------------------------------------
#' # Vars on main sheet
#' base_area <- df %>% #base_df
#'   select(c(ID_ENCUESTA, totalParcelAreaInHa_trr, 
#'            totalAreaUsedForAgriculture,measurementUnitOfArea,totalAreaUsedForAgricultureInHa,
#'            coffeeAreaLastHarvest,coffeeAreaMeasurementUnit,coffeeAreaLastHarvestInHa,
#'            productiveCoffeeAreaLastHarvest,productiveCoffeeAreaUnit))
#' 
#' # Vars on area_terr sheet
#' area_terr_area <- area_terr_df %>%
#'   select(c(ID_ENCUESTA,totalParcelArea,parcelAreaMeasurementUnit))
#' 
#' # Joining to have a better overview
#' df_area <- full_join(base_area, area_terr_area,  by = "ID_ENCUESTA")
#' df_area <- df_area %>% 
#'   dplyr::select(ID_ENCUESTA,
#'                 totalParcelArea,parcelAreaMeasurementUnit,totalParcelAreaInHa_trr,
#'                 totalAreaUsedForAgriculture,measurementUnitOfArea,totalAreaUsedForAgricultureInHa,
#'                 coffeeAreaLastHarvest,coffeeAreaMeasurementUnit,coffeeAreaLastHarvestInHa,
#'                 productiveCoffeeAreaLastHarvest,productiveCoffeeAreaUnit)
#' df_area$totalParcelArea <- as.numeric(df_area$totalParcelArea)
#' rm(area_terr_area)
#' 
#' ### Missing values ------------------
#' df_area_missing <- df_area %>%
#'   dplyr::filter(if_any(all_of(names(df_area)), is.na))
#' colSums(is.na(df_area_missing))
#' #' There are 265 observations without values for any area variable.
#' colSums(is.na(base_area))
#' #' The rest of observations have values in all area variables. There is no need
#' #' to impute missing values between variables.
#' rm(df_area_missing,base_area)
#' 
#' ### Outliers ------------------------
#' # Separating observations with outliers (based on quality check)
#' vars_check <- c("totalParcelAreaInHa_trr","totalAreaUsedForAgricultureInHa","coffeeAreaLastHarvestInHa")
#' ## Loop by outlier variable
#' for(v in vars_check){
#'   df[[paste0(v, "_outlier")]] <-
#'     !is.na(df[[v]]) & (
#'       df[[v]] < as.numeric(results$lower_bound[[v]]) |
#'         df[[v]] > as.numeric(results$upper_bound[[v]])
#'     )
#' }
#' ## Keeping only rows with at least 1 outlier
#' flag             <- rowSums(df[paste0(vars_check, "_outlier")]) > 0
#' df_area_outliers <- df_area[df_area$ID_ENCUESTA %in% df$ID_ENCUESTA[flag], ]
#' ## Adding dummies to identify which var has the outlier
#' df_area_outliers <- merge(
#'   df_area_outliers,
#'   df[, c("ID_ENCUESTA", paste0(vars_check, "_outlier"))],
#'   by = "ID_ENCUESTA",  all.x = TRUE
#' )
#' 
#' 
#' # Evaluating cases with any outlier
#' ## Case 1: only coffeeAreaLastHarvestInHa_outlier=TRUE
#' df_area_outliers_c1 <- df_area_outliers %>% filter(
#'     coffeeAreaLastHarvestInHa_outlier       == TRUE  &
#'     totalAreaUsedForAgricultureInHa_outlier == FALSE &
#'     totalParcelAreaInHa_trr_outlier         == FALSE )
#' summary(duplicated(df_area_outliers_c1$ID_ENCUESTA))
#' results[["upper_bound"]][["coffeeAreaLastHarvestInHa"]]
#' corrected_outliers <- c("2023-11-08-T-336-X-669","2023-10-05-T-778-X-313","2023-10-30-T-2455-X-2066",
#'                         "2023-10-21-T-188-X-319")
#' coherent_outliers <- c("2023-10-06-T-29-X-262","2023-10-07-T-224-X-253","2023-10-14-T-863-X-221",
#'                        "2023-10-18-T-402-X-931","2023-10-23-T-285-X-42","2023-10-23-T-543-X-913",
#'                        "2023-10-27-T-8218-X-2768","2023-10-30-T-1899-X-6021","2023-11-02-T-5957-X-7411",
#'                        "2023-11-09-T-499-X-407","2023-10-06-T-294-X-685","2023-10-18-T-350-X-135",
#'                        "2023-10-18-T-551-X-811","2023-10-19-T-520-X-976","2023-10-19-T-925-X-541",
#'                        "2023-10-26-T-5986-X-6703","2023-10-26-T-6452-X-3280","2023-10-27-T-5219-X-3944",
#'                        "2023-10-28-T-5456-X-4396","2023-10-28-T-590-X-1900","2023-10-28-T-8301-X-2441",
#'                        "2023-10-30-T-1351-X-3126","2023-10-30-T-1529-X-5250","2023-10-30-T-5657-X-2691",
#'                        "2023-11-04-T-312-X-272","2023-11-04-T-5605-X-5944","2023-11-09-T-925-X-316")
#' df_area_outliers_c1 <- df_area_outliers_c1[!(df_area_outliers_c1$ID_ENCUESTA %in% c(corrected_outliers,coherent_outliers)), ]
#' 
#' ## Case 2: only totalAreaUsedForAgricultureInHa_outlier=TRUE
#' df_area_outliers_c2 <- df_area_outliers %>% filter(
#'     coffeeAreaLastHarvestInHa_outlier       == FALSE &
#'     totalAreaUsedForAgricultureInHa_outlier == TRUE  &
#'     totalParcelAreaInHa_trr_outlier         == FALSE )
#' summary(duplicated(df_area_outliers_c2$ID_ENCUESTA))
#' results[["upper_bound"]][["totalAreaUsedForAgricultureInHa"]]
#' corrected_outliers <- c("2023-10-16-T-694-X-362","2023-10-19-T-347-X-954","2023-10-27-T-7525-X-2130",
#'                         "2023-10-28-T-5335-X-8873")
#' df_area_outliers_c2 <- df_area_outliers_c2[!(df_area_outliers_c2$ID_ENCUESTA %in% c(corrected_outliers,coherent_outliers)), ]
#' 
#' ## Case 3: only totalParcelAreaInHa_trr_outlier=TRUE
#' df_area_outliers_c3 <- df_area_outliers %>% filter(
#'     coffeeAreaLastHarvestInHa_outlier       == FALSE &
#'     totalAreaUsedForAgricultureInHa_outlier == FALSE &
#'     totalParcelAreaInHa_trr_outlier         == TRUE  )
#' summary(duplicated(df_area_outliers_c3$ID_ENCUESTA))
#' results[["upper_bound"]][["totalParcelAreaInHa_trr"]]
#' corrected_outliers <- c("2023-10-11-T-812-X-70","2023-11-08-T-864-X-666")
#' coherent_outliers  <- c("2023-10-30-T-5368-X-952","2023-10-30-T-7439-X-2588","2023-11-06-T-507-X-86",
#'                         "2023-11-08-T-257-X-451")
#' df_area_outliers_c3 <- df_area_outliers_c3[!(df_area_outliers_c3$ID_ENCUESTA %in% c(corrected_outliers,coherent_outliers)), ]
#' 
#' ## Case 4: coffeeAreaLastHarvestInHa_outlier=TRUE & totalAreaUsedForAgricultureInHa_outlier=TRUE
#' df_area_outliers_c4 <- df_area_outliers %>% filter(
#'     coffeeAreaLastHarvestInHa_outlier       == TRUE &
#'     totalAreaUsedForAgricultureInHa_outlier == TRUE &
#'     totalParcelAreaInHa_trr_outlier         == FALSE)
#' #' No observations
#' 
#' ## Case 5: coffeeAreaLastHarvestInHa_outlier=TRUE & totalParcelAreaInHa_trr_outlier=TRUE
#' df_area_outliers_c5 <- df_area_outliers %>% filter(
#'     coffeeAreaLastHarvestInHa_outlier       == TRUE  &
#'     totalAreaUsedForAgricultureInHa_outlier == FALSE &
#'     totalParcelAreaInHa_trr_outlier         == TRUE )
#' summary(duplicated(df_area_outliers_c5$ID_ENCUESTA))
#' coherent_outliers  <- c("2023-10-30-T-7901-X-9723","2023-11-04-T-79-X-153","2023-11-07-T-196-X-464")
#' df_area_outliers_c5 <- df_area_outliers_c5[!(df_area_outliers_c5$ID_ENCUESTA %in% c(corrected_outliers,coherent_outliers)), ]
#' 
#' ## Case 6: totalAreaUsedForAgricultureInHa_outlier=TRUE & totalParcelAreaInHa_trr_outlier=TRUE
#' df_area_outliers_c6 <- df_area_outliers %>% filter(
#'     coffeeAreaLastHarvestInHa_outlier       == FALSE &
#'     totalAreaUsedForAgricultureInHa_outlier == TRUE  &
#'     totalParcelAreaInHa_trr_outlier         == TRUE )
#' summary(duplicated(df_area_outliers_c6$ID_ENCUESTA))
#' corrected_outliers <- c("2023-11-02-T-1127-X-9406","2023-10-06-T-787-X-371")
#' coherent_outliers  <- c("2023-10-11-T-338-X-589","2023-10-30-T-8181-X-7369","2023-10-12-T-973-X-976",
#'                         "2023-10-30-T-9979-X-7324","2023-10-04-T-781","2023-10-11-T-281-X-96",
#'                         "2023-10-21-T-340-X-609","2023-10-21-T-224-X-560","2023-10-18-T-107-X-220",
#'                         "2023-10-27-T-9424-X-4126","2023-10-28-T-9976-X-4731","2023-10-05-T-102-X-917",
#'                         "2023-10-12-T-881-X-411","2023-10-05-T-484-X-543","2023-10-16-T-296-X-906",
#'                         "2023-10-04-T-161","2023-10-17-T-609-X-335","2023-10-28-T-6375-X-7226",
#'                         "2023-11-07-T-238-X-192","2023-11-09-T-623-X-239","2023-11-07-T-26-X-315",
#'                         "2023-10-19-T-892-X-246","2023-10-19-T-835-X-616","2023-10-05-T-295-X-88",
#'                         "2023-10-20-T-617-X-618","2023-10-06-T-891-X-140","2023-10-30-T-997-X-8093",
#'                         "2023-10-26-T-3027-X-3937","2023-10-16-T-42-X-554","2023-10-05-T-245-X-367")
#' df_area_outliers_c6 <- df_area_outliers_c6[!(df_area_outliers_c6$ID_ENCUESTA %in% c(corrected_outliers,coherent_outliers)), ]
#' 
#' ## Case 7: all=TRUE
#' df_area_outliers_c7 <- df_area_outliers %>% filter(
#'     coffeeAreaLastHarvestInHa_outlier       == TRUE  &
#'     totalAreaUsedForAgricultureInHa_outlier == TRUE &
#'     totalParcelAreaInHa_trr_outlier         == TRUE )
#' summary(duplicated(df_area_outliers_c7$ID_ENCUESTA))
#' corrected_outliers <- c("2023-10-21-T-871-X-106","2023-10-06-T-375-X-292","2023-10-11-T-30-X-574")
#' coherent_outliers  <- c("2023-11-02-T-5494-X-2657","2023-10-27-T-849-X-249","2023-10-21-T-508-X-455",
#'                         "2023-10-27-T-276-X-8288","2023-10-27-T-3443-X-5030","2023-10-30-T-892-X-5350",
#'                         "2023-10-26-T-4117-X-622","2023-10-28-T-4882-X-1578","2023-10-26-T-404-X-1619",
#'                         "2023-10-18-T-365-X-326","2023-10-07-T-986-X-326","2023-10-27-T-2212-X-6213",
#'                         "2023-10-23-T-353-X-716","2023-11-07-T-559-X-736","2023-11-07-T-245-X-63",
#'                         "2023-10-06-T-880-X-873","2023-10-30-T-5140-X-330","2023-10-11-T-347-X-936",
#'                         "2023-11-06-T-157-X-625","2023-10-26-T-9490-X-4301","2023-10-23-T-406-X-207",
#'                         "2023-10-19-T-665-X-248","2023-11-09-T-645-X-796","2023-11-06-T-655-X-804",
#'                         "2023-10-28-T-596-X-5729","2023-10-23-T-513-X-947","2023-10-27-T-1181-X-1961",
#'                         "2023-10-07-T-335-X-365","2023-10-20-T-375-X-397","2023-11-02-T-2012-X-6961",
#'                         "2023-11-04-T-428-X-512","2023-10-26-T-8115-X-3427","2023-10-30-T-1841-X-1411",
#'                         "2023-11-08-T-514-X-232","2023-11-09-T-656-X-642","2023-10-26-T-2590-X-8500",
#'                         "2023-10-18-T-322-X-965","2023-11-07-T-146-X-994","2023-10-28-T-7752-X-8204",
#'                         "2023-10-26-T-650-X-4386","2023-10-19-T-104-X-957","2023-10-06-T-504-X-978",
#'                         "2023-11-02-T-4070-X-6073","2023-10-09-T-125-X-176","2023-10-28-T-3559-X-6172",
#'                         "2023-10-26-T-7095-X-9140","2023-10-21-T-913-X-279","2023-10-19-T-49-X-782",
#'                         "2023-10-17-T-902-X-123","2023-10-17-T-889-X-836","2023-10-14-T-134-X-280",
#'                         "2023-10-28-T-3129-X-6097","2023-10-26-T-5798-X-3347","2023-10-16-T-667-X-326",
#'                         "2023-10-07-T-211-X-688","2023-10-11-T-304-X-496","2023-10-18-T-46-X-810",
#'                         "2023-10-26-T-9700-X-1198","2023-10-07-T-769-X-887","2023-11-02-T-5189-X-1159")
#' df_area_outliers_c7 <- df_area_outliers_c7[!(df_area_outliers_c7$ID_ENCUESTA %in% c(corrected_outliers,coherent_outliers)), ]
#' 
#' ## Deleting variables
#' rm(vars_check,flag,df_area_outliers,df_area_outliers_c1,df_area_outliers_c2,
#'    df_area_outliers_c3,df_area_outliers_c4,df_area_outliers_c5,df_area_outliers_c6,
#'    df_area_outliers_c7,corrected_outliers,coherent_outliers)
#' 
#' 
#' ### Comparing areas ------------------------
#' 
#' ## Case 1: coffeeAreaLastHarvestInHa > totalAreaUsedForAgricultureInHa
#' df_area_comparing_c1 <- df_area %>% filter(
#'   coffeeAreaLastHarvestInHa > totalAreaUsedForAgricultureInHa)
#' summary(duplicated(df_area_comparing_c1$ID_ENCUESTA))
#' false_comparing <- c("2023-11-04-T-410-X-157")
#' cortd_comparing <- c("2023-10-03-T-104","2023-10-03-T-68","2023-10-06-T-504-X-978",
#'                      "2023-10-11-T-30-X-574","2023-10-17-T-303-X-191","2023-11-06-T-507-X-86",
#'                      "2023-11-08-T-336-X-669","2023-10-30-T-2455-X-2066")
#' df_area_comparing_c1 <- df_area_comparing_c1[!(df_area_comparing_c1$ID_ENCUESTA %in% c(cortd_comparing,false_comparing)), ]
#' 
#' ## Case 2: coffeeAreaLastHarvestInHa > totalParcelAreaInHa_trr
#' df_area_comparing_c2 <- df_area %>% filter(
#'   coffeeAreaLastHarvestInHa > totalParcelAreaInHa_trr)
#' summary(duplicated(df_area_comparing_c2$ID_ENCUESTA))
#' cortd_comparing <- c("2023-11-08-T-336-X-669","2023-10-11-T-30-X-574","2023-10-06-T-134-X-681",
#'                      "2023-10-09-T-118-X-697","2023-10-30-T-2455-X-2066","2023-10-05-T-363-X-749",
#'                      "2023-10-06-T-170-X-101","2023-10-03-T-368","2023-10-03-T-99",
#'                      "2023-10-04-T-661","2023-10-03-T-643")
#' df_area_comparing_c2 <- df_area_comparing_c2[!(df_area_comparing_c2$ID_ENCUESTA %in% cortd_comparing), ]
#' 
#' ## Case 3: totalAreaUsedForAgricultureInHa > totalParcelAreaInHa_trr
#' df_area_comparing_c3 <- df_area %>% filter(
#'   totalAreaUsedForAgricultureInHa > totalParcelAreaInHa_trr)
#' summary(duplicated(df_area_comparing_c3$ID_ENCUESTA))
#' false_comparing <- c()
#' cortd_comparing <- c("2023-10-03-T-368","2023-10-03-T-372","2023-10-03-T-389",
#'                      "2023-10-03-T-525","2023-10-03-T-643","2023-10-03-T-708",
#'                      "2023-10-03-T-99","2023-10-04-T-583","2023-10-04-T-625",
#'                      "2023-10-03-T-497","2023-10-04-T-661","2023-10-05-T-363-X-749",
#'                      "2023-10-05-T-778-X-313","2023-10-05-T-888-X-179","2023-10-06-T-134-X-681",
#'                      "2023-10-06-T-170-X-101","2023-10-06-T-180-X-981","2023-10-06-T-316-X-491",
#'                      "2023-10-06-T-375-X-292","2023-10-06-T-787-X-371","2023-10-07-T-722-X-380",
#'                      "2023-10-07-T-950-X-608","2023-10-09-T-118-X-697","2023-10-09-T-178-X-346",
#'                      "2023-10-09-T-181-X-688","2023-10-09-T-270-X-331","2023-11-09-T-574-X-215",
#'                      "2023-11-08-T-197-X-859","2023-10-28-T-7560-X-6247","2023-10-28-T-5335-X-8873",
#'                      "2023-10-28-T-485-X-4910","2023-10-27-T-7525-X-2130","2023-10-23-T-543-X-913",
#'                      "2023-10-21-T-871-X-106","2023-10-21-T-587-X-156","2023-10-21-T-188-X-319",
#'                      "2023-10-21-T-106-X-837","2023-10-20-T-787-X-110","2023-10-20-T-464-X-62",
#'                      "2023-11-09-T-306-X-828","2023-10-20-T-442-X-214","2023-10-20-T-341-X-157",
#'                      "2023-10-19-T-347-X-954","2023-10-18-T-984-X-562","2023-10-18-T-804-X-504",
#'                      "2023-10-18-T-714-X-17","2023-10-17-T-899-X-318","2023-10-17-T-675-X-673",
#'                      "2023-10-17-T-496-X-166","2023-10-17-T-452-X-770","2023-10-16-T-694-X-362",
#'                      "2023-10-11-T-830-X-837","2023-10-11-T-387-X-366","2023-10-07-T-892-X-832",
#'                      "2023-10-09-T-587-X-865","2023-10-11-T-490-X-800","2023-10-14-T-40-X-353")
#' df_area_comparing_c3 <- df_area_comparing_c3[!(df_area_comparing_c3$ID_ENCUESTA %in% c(cortd_comparing,false_comparing)), ]
#' 
#' #Removing variables
#' rm(df_area_comparing_c1,df_area_comparing_c2,df_area_comparing_c3,area_terr_df,
#'    area_terr_dict,df_area,cortd_comparing)


## Assessing coffee quantity variables -------------------------------------
# Vars on main sheet
base_quan <- base_df %>%
  select(c(ID_ENCUESTA,typeOfCoffeeProduced,coffeeProductionMeasurementUnit,
           amountOfCoffeeProducedLastHarvest))
base_quan$amountOfCoffeeProducedLastHarvest <- as.numeric(base_quan$amountOfCoffeeProducedLastHarvest)

# Vars on sales_repeat sheet
sales_repeat_quan <- sales_repeat_df %>%
  select(c(ID_ENCUESTA,SALES_REPEAT_ROWID,formInWhichCoffeeWasSold,amountCoffeeSold))
sales_repeat_quan$amountCoffeeSold <- as.numeric(sales_repeat_quan$amountCoffeeSold)

# Vars on sales_process_repeat sheet
sales_process_repeat_quan <- sales_process_repeat_df %>%
  select(c(ID_ENCUESTA,SALES_REPEAT_ROWID,SALES_PROCESS_REPEAT_ROWID,
           methodOfCoffeeSale,salesMeasurementUnit,amountSold))
sales_process_repeat_quan$amountSold <- as.numeric(sales_process_repeat_quan$amountSold)

# Joining to have a better overview
df_quan_aux <- full_join(sales_repeat_quan, sales_process_repeat_quan,  by = c("ID_ENCUESTA","SALES_REPEAT_ROWID"))
df_quan     <- full_join(base_quan, df_quan_aux,  by = "ID_ENCUESTA")
rm(base_quan,sales_repeat_quan,sales_process_repeat_quan,df_quan_aux)


### Missing values ------------------
#' General rule: when in doubt, take the production's word for it
df_quan_missing <- df_quan %>%
  #dplyr::filter(if_any(all_of(names(df_quan)), is.na)) %>%
  dplyr::group_by(ID_ENCUESTA) %>%
  dplyr::ungroup()
colSums(is.na(df_quan_missing))
#' There are 265 observations without values for any area variable.
#' For the variable "amountCoffeeSold" there are 318-265=53 extra observations

# Creating auxiliar variables
df_quan_missing <- df_quan_missing %>%
  dplyr::group_by(ID_ENCUESTA) %>%
  ## Number of rows
  dplyr::mutate(aux_nrows_id = dplyr::n()) %>%
  ## Total sales
  mutate(aux_tot_amountCoffeeSold = sum(amountCoffeeSold, na.rm = TRUE)) %>%
  dplyr::ungroup()
## Distances to detect outliers
df_quan_missing$aux_distance <-  df_quan_missing$amountOfCoffeeProducedLastHarvest - df_quan_missing$aux_tot_amountCoffeeSold
df_quan_missing$aux_distPerc <- (df_quan_missing$amountOfCoffeeProducedLastHarvest - df_quan_missing$aux_tot_amountCoffeeSold)/df_quan_missing$amountOfCoffeeProducedLastHarvest
## Organizing missings
df_quan_missing$aux_nrows_id[is.na(df_quan_missing$typeOfCoffeeProduced)] <- NA
df_quan_missing$aux_tot_amountCoffeeSold[is.na(df_quan_missing$typeOfCoffeeProduced)] <- NA

# Deleting rows without information
## From case 1: zero sales
df_quan_missing <- df_quan_missing %>% filter(ID_ENCUESTA != "2023-10-06-T-593-X-673")
## From case 3: zero sales or no sale information
df_quan_missing <- df_quan_missing %>%
  dplyr::filter(!(ID_ENCUESTA %in% c(
    "2023-10-06-T-802-X-693","2023-10-09-T-928-X-517","2023-10-23-T-212-X-861",
    "2023-11-07-T-987-X-731","2023-10-28-T-590-X-1900","2023-10-30-T-3183-X-3566",
    "2023-10-27-T-1468-X-5761","2023-10-28-T-485-X-4910","2023-10-09-T-463-X-227",
    "2023-10-26-T-3545-X-8227","2023-10-18-T-787-X-813","2023-10-03-T-630") &
      is.na(amountCoffeeSold)))
## From case 4: zero sales or no sale information
df_quan_missing <- df_quan_missing %>%
  dplyr::filter(!(ID_ENCUESTA %in% c(
    "2023-10-21-T-224-X-560","2023-10-06-T-400-X-508","2023-10-11-T-511-X-393",
    "2023-11-08-T-69-X-166","2023-10-30-T-6842-X-7600","2023-11-08-T-514-X-232") &
      is.na(amountCoffeeSold)))

# Case 1: One buyer and one form. Same form
#' The units are reviewed to ensure they are consistent with those in the production
#' or can be rescaled to an equivalent unit.
#' If no equivalence is found, the production reported is prioritized, assuming production=sales
#' Once the units have been organized, replaces are:
#' amountSold           <- amountCoffeeSold
#' salesMeasurementUnit <- coffeeProductionMeasurementUnit

## 1.1. Corresponding proportions for other units of measurement
#' Letting sales number, changing unit unit manually
df_quan_missing$salesMeasurementUnit[
    df_quan_missing$ID_ENCUESTA=="2023-10-19-T-440-X-497" & df_quan_missing$amountCoffeeSold==24925 |
    df_quan_missing$ID_ENCUESTA=="2023-10-03-T-997"       & df_quan_missing$amountCoffeeSold==1275
] <- "lb"

## 1.2. Same number but in different proportions -> Assigning production number and units
#' Assigning production number and units
cases <- data.frame(
  ID_ENCUESTA = c("2023-10-11-T-841-X-664","2023-10-03-T-980","2023-10-30-T-8117-X-3374","2023-10-26-T-5986-X-6703"),
  amountCoffeeSold = c(975, 425, 15,50)
)
for(i in 1:nrow(cases)){
  condition <- df_quan_missing$ID_ENCUESTA == cases$ID_ENCUESTA[i] & df_quan_missing$amountCoffeeSold == cases$amountCoffeeSold[i]
  df_quan_missing$amountSold[condition]           <- df_quan_missing$amountOfCoffeeProducedLastHarvest[condition]
  df_quan_missing$salesMeasurementUnit[condition] <- df_quan_missing$coffeeProductionMeasurementUnit[condition]
}

## 1.3. No equivalent units of measurement were found
#' Assigning production number and units
#' Assumption: production = sales
cases <- data.frame(
  ID_ENCUESTA = c("2023-11-04-T-2048-X-8712","2023-10-21-T-106-X-837","2023-11-07-T-655-X-404",
                  "2023-10-16-T-553-X-916","2023-10-20-T-438-X-812","2023-10-04-T-765",
                  "2023-10-16-T-667-X-326","2023-10-18-T-790-X-517","2023-10-11-T-30-X-574",
                  "2023-10-26-T-5798-X-3347","2023-10-26-T-5662-X-1743","2023-10-23-T-321-X-18",
                  "2023-10-16-T-230-X-831","2023-11-08-T-197-X-859","2023-11-08-T-712-X-211"),
  amountCoffeeSold = c(540,125,48,
                       30,498,5,
                       190,113,202,
                       20,35,90,
                       1,6,5)
)
for(i in 1:nrow(cases)){
  condition <- df_quan_missing$ID_ENCUESTA == cases$ID_ENCUESTA[i] & df_quan_missing$amountCoffeeSold == cases$amountCoffeeSold[i]
  df_quan_missing$amountSold[condition]           <- df_quan_missing$amountOfCoffeeProducedLastHarvest[condition]
  df_quan_missing$salesMeasurementUnit[condition] <- df_quan_missing$coffeeProductionMeasurementUnit[condition]
}

## 1.0.  The rest
#' Letting sales number, assigning production units
condition <- 
  !is.na(df_quan_missing$aux_nrows_id) & df_quan_missing$aux_nrows_id == 1 &
  df_quan_missing$typeOfCoffeeProduced == df_quan_missing$formInWhichCoffeeWasSold
### Quantity
df_quan_missing$amountSold[condition & is.na(df_quan_missing$amountSold)] <- 
  df_quan_missing$amountCoffeeSold[condition & is.na(df_quan_missing$amountSold)]
### Unit
df_quan_missing$salesMeasurementUnit[condition & is.na(df_quan_missing$salesMeasurementUnit)] <- 
  df_quan_missing$coffeeProductionMeasurementUnit[condition & is.na(df_quan_missing$salesMeasurementUnit)]


# Case 2: One buyer and one form. Different forms
#' #' The forms are reviewed to ensure they are consistent with those in the production
#' or can be changed to an equivalent form.
#' If no equivalence is found, the production reported is prioritized, assuming production=sales
#' Once the forms have been organized, replaces are:
#' amountSold               <- amountCoffeeSold
#' salesMeasurementUnit     <- coffeeProductionMeasurementUnit
#' formInWhichCoffeeWasSold <- typeOfCoffeeProduced
#' methodOfCoffeeSale       <- typeOfCoffeeProduced

## 2.1. Proportions match for other units
#' Letting sales number and form, assigning production units
cases <- data.frame(
  ID_ENCUESTA = c("2023-10-21-T-508-X-455","2023-10-17-T-742-X-913","2023-10-18-T-676-X-692",
                  "2023-10-26-T-1302-X-9320","2023-10-26-T-8427-X-8270","2023-10-26-T-9216-X-5786",
                  "2023-10-28-T-2370-X-4177","2023-10-06-T-312-X-275","2023-10-27-T-4098-X-9738",
                  "2023-10-27-T-668-X-5678","2023-10-28-T-3129-X-6097"),
  amountCoffeeSold = c(500,2,10,
                       64,66,15,
                       30,40,80,
                       300,972)
)
for(i in 1:nrow(cases)){
  condition <-
    df_quan_missing$ID_ENCUESTA == cases$ID_ENCUESTA[i] & df_quan_missing$amountCoffeeSold == cases$amountCoffeeSold[i]
  df_quan_missing$amountSold[condition]           <- df_quan_missing$amountCoffeeSold[condition]
  df_quan_missing$salesMeasurementUnit[condition] <- df_quan_missing$coffeeProductionMeasurementUnit[condition]
}

## 2.2. No equivalent forms were found
#' Assigning production number, units and form
#' Assumption: production = sales
cases <- data.frame(
  ID_ENCUESTA = c("2023-10-14-T-860-X-222","2023-10-18-T-58-X-527","2023-10-27-T-4758-X-7063",
                  "2023-10-18-T-930-X-339","2023-10-16-T-139-X-588","2023-10-09-T-596-X-833",
                  "2023-10-27-T-627-X-3154","2023-10-28-T-4920-X-984",
                  "2023-11-08-T-42-X-155","2023-10-28-T-6683-X-9908"),
  amountCoffeeSold = c(4000,80,740,
                       6,40,15,
                       300,70,
                       5,154)
)
for(i in 1:nrow(cases)){
  condition <-
    df_quan_missing$ID_ENCUESTA == cases$ID_ENCUESTA[i] & df_quan_missing$amountCoffeeSold == cases$amountCoffeeSold[i]
  df_quan_missing$amountSold[condition]               <- df_quan_missing$amountOfCoffeeProducedLastHarvest[condition]
  df_quan_missing$salesMeasurementUnit[condition]     <- df_quan_missing$coffeeProductionMeasurementUnit[condition]
  df_quan_missing$formInWhichCoffeeWasSold[condition] <- df_quan_missing$typeOfCoffeeProduced[condition]
  df_quan_missing$methodOfCoffeeSale[condition]       <- df_quan_missing$typeOfCoffeeProduced[condition]
}

## 2.0.  The rest
#' Letting sales number, assigning production units and form
condition <- 
  !is.na(df_quan_missing$aux_nrows_id) & df_quan_missing$aux_nrows_id == 1 &
  df_quan_missing$typeOfCoffeeProduced != df_quan_missing$formInWhichCoffeeWasSold
### Form
df_quan_missing$formInWhichCoffeeWasSold[condition & is.na(df_quan_missing$amountSold)] <- 
  df_quan_missing$typeOfCoffeeProduced[condition & is.na(df_quan_missing$amountSold)]
df_quan_missing$methodOfCoffeeSale[condition & is.na(df_quan_missing$amountSold)]       <- 
  df_quan_missing$typeOfCoffeeProduced[condition & is.na(df_quan_missing$amountSold)]
### Quantity
df_quan_missing$amountSold[condition & is.na(df_quan_missing$amountSold)] <- 
  df_quan_missing$amountCoffeeSold[condition & is.na(df_quan_missing$amountSold)]
### Unit
df_quan_missing$salesMeasurementUnit[condition & is.na(df_quan_missing$salesMeasurementUnit)] <- 
  df_quan_missing$coffeeProductionMeasurementUnit[condition & is.na(df_quan_missing$salesMeasurementUnit)]


# Case 3: 2 buyers
#' Assumption: The units of measurement are equivalent between production and sales
#' unless the opposite is clearly evident. This assumption is justified by the fact 
#' that it is preferable to assume production=sales rather than introducing a lot
#' of noise into the variables by simultaneously changing the unit of weight and the
#' form of sale.
#' The forms are reviewed to ensure they are consistent with those in the production
#' or can be changed to an equivalent form.
#' Once the forms have been organized, replaces are:
#' amountSold               <- amountCoffeeSold
#' salesMeasurementUnit     <- coffeeProductionMeasurementUnit

## 3.1. Consistent values but different production and sales methods
#' Letting sales number, assigning production units and form
condition <-
  df_quan_missing$ID_ENCUESTA=="2023-10-03-T-621"         | df_quan_missing$ID_ENCUESTA=="2023-10-04-T-273"         |
  df_quan_missing$ID_ENCUESTA=="2023-10-05-T-262-X-940"   | df_quan_missing$ID_ENCUESTA=="2023-10-05-T-628-X-554"   |
  df_quan_missing$ID_ENCUESTA=="2023-10-09-T-111-X-294"   | df_quan_missing$ID_ENCUESTA=="2023-10-09-T-554-X-358"   |
  df_quan_missing$ID_ENCUESTA=="2023-10-09-T-587-X-865"   | df_quan_missing$ID_ENCUESTA=="2023-10-14-T-91-X-97"     |
  df_quan_missing$ID_ENCUESTA=="2023-10-17-T-589-X-73"    | df_quan_missing$ID_ENCUESTA=="2023-10-18-T-359-X-175"   |
  df_quan_missing$ID_ENCUESTA=="2023-10-20-T-401-X-862"   | df_quan_missing$ID_ENCUESTA=="2023-11-09-T-613-X-227"   |
  df_quan_missing$ID_ENCUESTA=="2023-10-26-T-1166-X-5614" | df_quan_missing$ID_ENCUESTA=="2023-10-26-T-4587-X-9982" |
  df_quan_missing$ID_ENCUESTA=="2023-10-26-T-4064-X-3491" | df_quan_missing$ID_ENCUESTA=="2023-10-20-T-103-X-7"     |
  df_quan_missing$ID_ENCUESTA=="2023-10-14-T-865-X-422"   | df_quan_missing$ID_ENCUESTA=="2023-10-18-T-641-X-968"   |
  df_quan_missing$ID_ENCUESTA=="2023-10-14-T-741-X-189"   | df_quan_missing$ID_ENCUESTA=="2023-10-03-T-349"         |
  df_quan_missing$ID_ENCUESTA=="2023-10-14-T-370-X-677"   | df_quan_missing$ID_ENCUESTA=="2023-10-03-T-114"         |
  df_quan_missing$ID_ENCUESTA=="2023-10-03-T-643"         | df_quan_missing$ID_ENCUESTA=="2023-10-06-T-514-X-622"   |
  df_quan_missing$ID_ENCUESTA=="2023-10-04-T-625"         | df_quan_missing$ID_ENCUESTA=="2023-10-05-T-778-X-313"   |
  df_quan_missing$ID_ENCUESTA=="2023-10-11-T-812-X-70"    | df_quan_missing$ID_ENCUESTA=="2023-10-07-T-892-X-832"   |
  df_quan_missing$ID_ENCUESTA=="2023-10-04-T-915"         | df_quan_missing$ID_ENCUESTA=="2023-10-04-T-959"         |
  df_quan_missing$ID_ENCUESTA=="2023-10-09-T-785-X-748"   | df_quan_missing$ID_ENCUESTA=="2023-10-06-T-341-X-462"   |
  df_quan_missing$ID_ENCUESTA=="2023-11-07-T-641-X-777"   | df_quan_missing$ID_ENCUESTA=="2023-10-07-T-6-X-573"     |
  df_quan_missing$ID_ENCUESTA=="2023-10-14-T-648-X-290"   | df_quan_missing$ID_ENCUESTA=="2023-10-06-T-457-X-755"   |
  df_quan_missing$ID_ENCUESTA=="2023-10-04-T-803"         | df_quan_missing$ID_ENCUESTA=="2023-11-07-T-987-X-731"
df_quan_missing$formInWhichCoffeeWasSold[condition] <- df_quan_missing$typeOfCoffeeProduced[condition]
df_quan_missing$methodOfCoffeeSale[condition]       <- df_quan_missing$typeOfCoffeeProduced[condition]

## 3.0. The rest
#' Letting sales number and form, assigning production units
condition <- 
  !is.na(df_quan_missing$aux_nrows_id) & df_quan_missing$aux_nrows_id == 2
### Quantity
df_quan_missing$amountSold[condition & is.na(df_quan_missing$amountSold)] <- 
  df_quan_missing$amountCoffeeSold[condition & is.na(df_quan_missing$amountSold)]
### Unit
df_quan_missing$salesMeasurementUnit[condition & is.na(df_quan_missing$salesMeasurementUnit)] <- 
  df_quan_missing$coffeeProductionMeasurementUnit[condition & is.na(df_quan_missing$salesMeasurementUnit)]


# Case 4: >2 buyers
#' Assumption: The units of measurement are equivalent between production and sales
#' unless the opposite is clearly evident. This assumption is justified by the fact 
#' that it is preferable to assume production=sales rather than introducing a lot
#' of noise into the variables by simultaneously changing the unit of weight and the
#' form of sale.
#' The forms are reviewed to ensure they are consistent with those in the production
#' or can be changed to an equivalent form.
#' Once the forms have been organized, replaces are:
#' amountSold               <- amountCoffeeSold
#' salesMeasurementUnit     <- coffeeProductionMeasurementUnit

## 4.1. Consistent values but different production and sales methods, and ALL sales are equal
#' Letting sales number, assigning production units and form
condition <-
  df_quan_missing$ID_ENCUESTA=="2023-10-05-T-985-X-718"   | df_quan_missing$ID_ENCUESTA=="2023-10-14-T-726-X-131" |
  df_quan_missing$ID_ENCUESTA=="2023-10-26-T-6599-X-1772" | df_quan_missing$ID_ENCUESTA=="2023-10-09-T-461-X-687" |
  df_quan_missing$ID_ENCUESTA=="2023-10-06-T-880-X-873"
df_quan_missing$formInWhichCoffeeWasSold[condition] <- df_quan_missing$typeOfCoffeeProduced[condition]
df_quan_missing$methodOfCoffeeSale[condition]       <- df_quan_missing$typeOfCoffeeProduced[condition]

## 4.0. The rest
#' Letting sales number and form, assigning production units
condition <- 
  !is.na(df_quan_missing$aux_nrows_id) & df_quan_missing$aux_nrows_id > 2
### Quantity
df_quan_missing$amountSold[condition & is.na(df_quan_missing$amountSold)] <- 
  df_quan_missing$amountCoffeeSold[condition & is.na(df_quan_missing$amountSold)]
### Unit
df_quan_missing$salesMeasurementUnit[condition & is.na(df_quan_missing$salesMeasurementUnit)] <- 
  df_quan_missing$coffeeProductionMeasurementUnit[condition & is.na(df_quan_missing$salesMeasurementUnit)]

# Manually fixing some information
condition <- df_quan_missing$ID_ENCUESTA=="2023-10-18-T-930-X-339" & df_quan_missing$amountCoffeeSold==6
df_quan_missing$amountSold[condition] <- df_quan_missing$amountCoffeeSold[condition]

# Assigning values to sales_process_repeat_df
sales_process_repeat_df <- sales_process_repeat_df %>%
  select(-c(methodOfCoffeeSale,amountSold,salesMeasurementUnit))
df_quan_spr <- df_quan_missing %>%
  select(c(ID_ENCUESTA,SALES_REPEAT_ROWID,SALES_PROCESS_REPEAT_ROWID,
           methodOfCoffeeSale,salesMeasurementUnit,amountSold))
sales_process_repeat_df <- left_join(
  sales_process_repeat_df, df_quan_spr, by = c("ID_ENCUESTA","SALES_REPEAT_ROWID","SALES_PROCESS_REPEAT_ROWID"))

# Assigning values to sales_repeat_df
df_quan_sr <- df_quan_missing %>%
  select(c(ID_ENCUESTA,SALES_REPEAT_ROWID,formInWhichCoffeeWasSold,amountCoffeeSold)) %>%
  group_by(ID_ENCUESTA,SALES_REPEAT_ROWID,formInWhichCoffeeWasSold,amountCoffeeSold)
sales_repeat_df <- sales_repeat_df %>%
  select(-c(amountCoffeeSold,formInWhichCoffeeWasSold))
sales_repeat_df <- left_join(
  sales_repeat_df, df_quan_sr, by = c("ID_ENCUESTA","SALES_REPEAT_ROWID"))

# Removing variables
rm(cases,condition,df_quan_spr,df_quan_sr,df_quan_missing)

### Comparing quantity of coffee ------------------------
# 
# # Standardizing quantity to green kg
# df_quan_comparing <- standardize_weights(df_quan_missing,sales_process_repeat_dict)
# names(df_quan_comparing)
# df_quan_comp_agg <- df_quan_comparing %>%
#   group_by(
#     ID_ENCUESTA,typeOfCoffeeProduced,coffeeProductionMeasurementUnit,amountOfCoffeeProducedLastHarvest) %>%
#   summarise(
#     amountSoldInKgGreen = sum(amountSoldInKgGreen, na.rm = TRUE),
#     .groups = "drop"
#   )
# df_quan_comp_agg <- standardize_weights_prod(df_quan_comparing,dictionary)
# names(df_quan_comp_agg)
# 
# # Checking condition fulfillment
# df_quan_comp_flag <- df_quan_comp_agg %>% filter(
#   amountOfCoffeeProducedLastHarvestInKgGreen < amountSoldInKgGreen)
# false_comparing <- c("2023-10-21-T-508-X-455")
# cortd_comparing <- c("2023-10-17-T-742-X-913","2023-10-18-T-676-X-692","2023-10-26-T-1302-X-9320",
#                      "2023-10-26-T-8427-X-8270","2023-10-26-T-9216-X-5786","2023-10-03-T-332",
#                      "2023-10-06-T-155-X-2","2023-10-06-T-797-X-251","2023-10-17-T-193-X-397",
#                      "2023-10-17-T-792-X-372","2023-10-26-T-211-X-4417","2023-10-06-T-375-X-292")
# df_quan_comp_flag <- df_quan_comp_flag[!(df_quan_comp_flag$ID_ENCUESTA %in% c(cortd_comparing,false_comparing)), ]
# 
# # Removing variables
# rm(df_quan_comp_flag,false_comparing,cortd_comparing,df_quan_comparing)


## Plants -----------------------------------------------------------------



