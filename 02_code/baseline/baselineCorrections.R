#' This script corrects multiple variables in "01_data/raw/baseline/baselineAgg.csv", 
#' including missing values, outliers, and violations of expected numeric order relationships.
#' 
#' The main input for these corrections is: "03_tables/baseline/baselineQualityCheck.RData"
#' 
#' Workflow:
#' 1. Critical variables are identified using baselineQualityCheck.RData and 
#' baselineAgg.csv
#' 2. Missing values, outliers, and order violations are detected
#' 3. Corrections are manually implemented in "baselineCorrections.xlsx"
#' 4. Detection code (steps 1–3) is commented out
#' 5. baselineCorrections.xlsx is read and loaded into the corresponding datasets 
#' for each aggregation level
#' Only step 5 remains active during regular execution.
#' 
#' This structure allows the main script ("baselineProcessingCode.R") to load imputations
#' immediately after reading the raw data and before any further processing, ensuring 
#' that corrections are applied from the beginning of the pipeline.
#' 
#' Notes:
#' 1. The commented detection code requires an existing baselineAgg.csv file, since the
#' quality checks are run after this dataset has been created. If a new review of
#' variables is needed, baselineAgg.csv must first be rebuilt.
#' Also note that the quality check used for detection will differ from the one 
#' obtained after corrections are loaded.
#' 
#' 2. An exception to the Excel-based correction workflow is the sales_process_repeat 
#' loop (coffee sold to each buyer by sales format). Due to issues during baseline 
#' survey programming, the variables related to coffee quantity sold and sales units
#' are missing for most observations in this loop.
#' This issue is addressed directly in this script using other available variables 
#' (e.g. quantity sold to each buyer). As a result, the sales_process_repeat_df dataset
#' is rebuilt here and the corrected version is the one used in "baselineProcessingCode.R".
#' Corrections for this loop are therefore not implemented through "baselineCorrections.xlsx".


#' ========================================================================
#  Reading baselineCorrections.xlsx ---------------------------------------
#' ========================================================================

#' ------------------------------------------------------------------------
## Main sheet -------------------------------------------------------------

# Reading Excel
base_chg <- read_excel(changes_path, sheet = "mod_combined")


# Changing values
## Variables to change
vars_corregidas <- names(base_chg)[
  !(names(base_chg) %in% c("SURVEYID", "ID_ENCUESTA", "commentsOnChanges"))]

## Loop by var
for (var in vars_corregidas) {
  ### Matching name
  var_cor <- paste0(var, "_mapvar")
  ### Map for observations with changes
  map_var <- base_chg %>%
    filter(!is.na(.data[[var]])) %>%
    select("SURVEYID", "ID_ENCUESTA", !!sym(var))
  ### Changing df
  base_df <- base_df %>%
    left_join(map_var,
              by = c("SURVEYID", "ID_ENCUESTA"), 
              suffix = c("", "_mapvar")) %>%
    mutate(
      !!sym(var) := ifelse(!is.na(.data[[var_cor]]),
                           .data[[var_cor]],
                           .data[[var]])
    )
  ### Cleaning auxiliary columns
  base_df <- base_df %>%
    select(-all_of(var_cor), -matches("_mapvar$"))
}



#' ------------------------------------------------------------------------
## area_terr sheet --------------------------------------------------------

# Reading Excel
area_terr_chg <- read_excel(changes_path, sheet = "area_terr")


# Deleting observations
area_terr_df <- area_terr_df %>%
  anti_join(area_terr_chg %>% filter(delete == 1),by = c("ID_ENCUESTA", "ROWUUID"))


# Changing values
## Variables to change
vars_corregidas <- names(area_terr_chg)[
  !(names(area_terr_chg) %in% c("ID_ENCUESTA","ROWUUID","delete","commentsOnChanges"))]

## Loop by var
for (var in vars_corregidas) {
  ### Matching name
  var_cor <- paste0(var, "_mapvar")
  ### Map for observations with changes
  map_var <- area_terr_chg %>%
    filter(!is.na(.data[[var]])) %>%
    select("ID_ENCUESTA","ROWUUID", !!sym(var))
  ### Changing df
  area_terr_df <- area_terr_df %>%
    left_join(map_var,
              by = c("ID_ENCUESTA","ROWUUID"), 
              suffix = c("", "_mapvar")) %>%
    mutate(
      !!sym(var) := ifelse(!is.na(.data[[var_cor]]),
                           .data[[var_cor]],
                           .data[[var]])
    )
  ### Cleaning auxiliary columns
  area_terr_df <- area_terr_df %>%
    select(-all_of(var_cor), -matches("_mapvar$"))
}

rm("base_chg","area_terr_chg","map_var","vars_corregidas","var","var_cor")



#' ------------------------------------------------------------------------
## sales_process and sales_process_repeat sheets' -------------------------

### Creating dataset with production and sales ----------------------------

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


### Assessing outliers and missing values ---------------------------------
#' General rule: when in doubt, take the production's word for it.

# Creating auxiliar variables
df_quan <- df_quan %>%
  dplyr::group_by(ID_ENCUESTA) %>%
  ## Number of rows
  dplyr::mutate(aux_nrows_id = dplyr::n()) %>%
  ## Total sales
  mutate(aux_tot_amountCoffeeSold = sum(amountCoffeeSold, na.rm = TRUE)) %>%
  dplyr::ungroup()
## Distances to detect outliers
df_quan$aux_distance <-  df_quan$amountOfCoffeeProducedLastHarvest - df_quan$aux_tot_amountCoffeeSold
df_quan$aux_distPerc <- (df_quan$amountOfCoffeeProducedLastHarvest - df_quan$aux_tot_amountCoffeeSold)/df_quan$amountOfCoffeeProducedLastHarvest
## Organizing missings
df_quan$aux_nrows_id[is.na(df_quan$typeOfCoffeeProduced)] <- NA
df_quan$aux_tot_amountCoffeeSold[is.na(df_quan$typeOfCoffeeProduced)] <- NA


# Deleting rows without information
## From case 1: zero sales
df_quan <- df_quan %>% filter(ID_ENCUESTA != "2023-10-06-T-593-X-673")
## From case 3: zero sales or no sale information
df_quan <- df_quan %>%
  dplyr::filter(!(ID_ENCUESTA %in% c(
    "2023-10-06-T-802-X-693","2023-10-09-T-928-X-517","2023-10-23-T-212-X-861",
    "2023-11-07-T-987-X-731","2023-10-28-T-590-X-1900","2023-10-30-T-3183-X-3566",
    "2023-10-27-T-1468-X-5761","2023-10-28-T-485-X-4910","2023-10-09-T-463-X-227",
    "2023-10-26-T-3545-X-8227","2023-10-18-T-787-X-813","2023-10-03-T-630") &
      is.na(amountCoffeeSold)))
## From case 4: zero sales or no sale information
df_quan <- df_quan %>%
  dplyr::filter(!(ID_ENCUESTA %in% c(
    "2023-10-21-T-224-X-560","2023-10-06-T-400-X-508","2023-10-11-T-511-X-393",
    "2023-11-08-T-69-X-166","2023-10-30-T-6842-X-7600","2023-11-08-T-514-X-232") &
      is.na(amountCoffeeSold)))


# Case 1: One buyer and one form, same form
#' The units are reviewed to ensure they are consistent with those in the production
#' or can be re-scaled to an equivalent unit.
#' If no equivalence is found, the production reported is prioritized, assuming production=sales
#' Once the units have been organized, replaces are:
#' amountSold           <- amountCoffeeSold
#' salesMeasurementUnit <- coffeeProductionMeasurementUnit

## 1.1. Corresponding proportions for other units of measurement
#' Letting sales number, changing unit unit manually
df_quan$salesMeasurementUnit[
    df_quan$ID_ENCUESTA=="2023-10-19-T-440-X-497" & df_quan$amountCoffeeSold==24925 |
    df_quan$ID_ENCUESTA=="2023-10-03-T-997"       & df_quan$amountCoffeeSold==1275
] <- "lb"

## 1.2. Same number but in different proportions
#' Assigning production number and units
cases <- data.frame(
  ID_ENCUESTA = c("2023-10-11-T-841-X-664","2023-10-03-T-980","2023-10-30-T-8117-X-3374","2023-10-26-T-5986-X-6703"),
  amountCoffeeSold = c(975, 425, 15,50)
)
for(i in 1:nrow(cases)){
  condition <- df_quan$ID_ENCUESTA == cases$ID_ENCUESTA[i] & df_quan$amountCoffeeSold == cases$amountCoffeeSold[i]
  df_quan$amountSold[condition]           <- df_quan$amountOfCoffeeProducedLastHarvest[condition]
  df_quan$salesMeasurementUnit[condition] <- df_quan$coffeeProductionMeasurementUnit[condition]
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
  condition <- df_quan$ID_ENCUESTA == cases$ID_ENCUESTA[i] & df_quan$amountCoffeeSold == cases$amountCoffeeSold[i]
  df_quan$amountSold[condition]           <- df_quan$amountOfCoffeeProducedLastHarvest[condition]
  df_quan$salesMeasurementUnit[condition] <- df_quan$coffeeProductionMeasurementUnit[condition]
}

## 1.0.  The rest
#' Letting sales number, assigning production units
condition <- 
  !is.na(df_quan$aux_nrows_id) & df_quan$aux_nrows_id == 1 &
  df_quan$typeOfCoffeeProduced == df_quan$formInWhichCoffeeWasSold
### Quantity
df_quan$amountSold[condition & is.na(df_quan$amountSold)] <- 
  df_quan$amountCoffeeSold[condition & is.na(df_quan$amountSold)]
### Unit
df_quan$salesMeasurementUnit[condition & is.na(df_quan$salesMeasurementUnit)] <- 
  df_quan$coffeeProductionMeasurementUnit[condition & is.na(df_quan$salesMeasurementUnit)]


# Case 2: One buyer and one form, different forms
#' The forms are reviewed to ensure they are consistent with those in the production
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
    df_quan$ID_ENCUESTA == cases$ID_ENCUESTA[i] & df_quan$amountCoffeeSold == cases$amountCoffeeSold[i]
  df_quan$amountSold[condition]           <- df_quan$amountCoffeeSold[condition]
  df_quan$salesMeasurementUnit[condition] <- df_quan$coffeeProductionMeasurementUnit[condition]
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
    df_quan$ID_ENCUESTA == cases$ID_ENCUESTA[i] & df_quan$amountCoffeeSold == cases$amountCoffeeSold[i]
  df_quan$amountSold[condition]               <- df_quan$amountOfCoffeeProducedLastHarvest[condition]
  df_quan$salesMeasurementUnit[condition]     <- df_quan$coffeeProductionMeasurementUnit[condition]
  df_quan$formInWhichCoffeeWasSold[condition] <- df_quan$typeOfCoffeeProduced[condition]
  df_quan$methodOfCoffeeSale[condition]       <- df_quan$typeOfCoffeeProduced[condition]
}

## 2.0.  The rest
#' Letting sales number, assigning production units and form
condition <- 
  !is.na(df_quan$aux_nrows_id) & df_quan$aux_nrows_id == 1 &
  df_quan$typeOfCoffeeProduced != df_quan$formInWhichCoffeeWasSold
### Form
df_quan$formInWhichCoffeeWasSold[condition & is.na(df_quan$amountSold)] <- 
  df_quan$typeOfCoffeeProduced[condition & is.na(df_quan$amountSold)]
df_quan$methodOfCoffeeSale[condition & is.na(df_quan$amountSold)]       <- 
  df_quan$typeOfCoffeeProduced[condition & is.na(df_quan$amountSold)]
### Quantity
df_quan$amountSold[condition & is.na(df_quan$amountSold)] <- 
  df_quan$amountCoffeeSold[condition & is.na(df_quan$amountSold)]
### Unit
df_quan$salesMeasurementUnit[condition & is.na(df_quan$salesMeasurementUnit)] <- 
  df_quan$coffeeProductionMeasurementUnit[condition & is.na(df_quan$salesMeasurementUnit)]


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
condition <- df_quan$ID_ENCUESTA %in% c(
  "2023-10-03-T-621","2023-10-04-T-273","2023-10-05-T-262-X-940","2023-10-05-T-628-X-554","2023-10-09-T-111-X-294","2023-10-09-T-554-X-358",
  "2023-10-09-T-587-X-865","2023-10-14-T-91-X-97","2023-10-17-T-589-X-73","2023-10-18-T-359-X-175","2023-10-20-T-401-X-862","2023-11-09-T-613-X-227",
  "2023-10-26-T-1166-X-5614","2023-10-26-T-4587-X-9982","2023-10-26-T-4064-X-3491","2023-10-20-T-103-X-7","2023-10-14-T-865-X-422","2023-10-18-T-641-X-968",
  "2023-10-14-T-741-X-189","2023-10-03-T-349","2023-10-14-T-370-X-677","2023-10-03-T-114","2023-10-03-T-643","2023-10-06-T-514-X-622",
  "2023-10-04-T-625","2023-10-05-T-778-X-313","2023-10-11-T-812-X-70","2023-10-07-T-892-X-832","2023-10-04-T-915","2023-10-04-T-959",
  "2023-10-09-T-785-X-748","2023-10-06-T-341-X-462","2023-11-07-T-641-X-777","2023-10-07-T-6-X-573","2023-10-14-T-648-X-290","2023-10-06-T-457-X-755",
  "2023-10-04-T-803","2023-11-07-T-987-X-731")
df_quan$formInWhichCoffeeWasSold[condition] <- df_quan$typeOfCoffeeProduced[condition]
df_quan$methodOfCoffeeSale[condition]       <- df_quan$typeOfCoffeeProduced[condition]

## 3.0. The rest
#' Letting sales number and form, assigning production units
condition <- !is.na(df_quan$aux_nrows_id) & df_quan$aux_nrows_id == 2
### Quantity
df_quan$amountSold[condition & is.na(df_quan$amountSold)] <- 
  df_quan$amountCoffeeSold[condition & is.na(df_quan$amountSold)]
### Unit
df_quan$salesMeasurementUnit[condition & is.na(df_quan$salesMeasurementUnit)] <- 
  df_quan$coffeeProductionMeasurementUnit[condition & is.na(df_quan$salesMeasurementUnit)]


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

## 4.1. Consistent values but different production and sales methods, and ALL sales forms are equal
#' Letting sales number, assigning production units and form
condition <- df_quan$ID_ENCUESTA %in% c(
  "2023-10-05-T-985-X-718","2023-10-14-T-726-X-131","2023-10-26-T-6599-X-1772","2023-10-09-T-461-X-687")
df_quan$formInWhichCoffeeWasSold[condition] <- df_quan$typeOfCoffeeProduced[condition]
df_quan$methodOfCoffeeSale[condition]       <- df_quan$typeOfCoffeeProduced[condition]

## 4.0. The rest
#' Letting sales number and form, assigning production units
condition <- !is.na(df_quan$aux_nrows_id) & df_quan$aux_nrows_id > 2
### Quantity
df_quan$amountSold[condition & is.na(df_quan$amountSold)] <- 
  df_quan$amountCoffeeSold[condition & is.na(df_quan$amountSold)]
### Unit
df_quan$salesMeasurementUnit[condition & is.na(df_quan$salesMeasurementUnit)] <- 
  df_quan$coffeeProductionMeasurementUnit[condition & is.na(df_quan$salesMeasurementUnit)]

# Manually fixing some information
## One observation
condition <- df_quan$ID_ENCUESTA=="2023-10-18-T-930-X-339" & df_quan$amountCoffeeSold==6
df_quan$amountSold[condition] <- df_quan$amountCoffeeSold[condition]
## Organizing outliers
#' Outliers in production, sales, and yield. These values were compared with the number of
#' hectares of coffee, and it was decided to accept the reported production values.
#' The sales values. were scaled proportionally.
df_quan$amountSold[df_quan$ID_ENCUESTA=="2023-10-06-T-375-X-292"] <- 
  (270/791.36)*(df_quan$amountSold[df_quan$ID_ENCUESTA=="2023-10-06-T-375-X-292"])
df_quan$amountSold[df_quan$ID_ENCUESTA=="2023-10-17-T-193-X-397"] <- 
  (107.75/441.172)*(df_quan$amountSold[df_quan$ID_ENCUESTA=="2023-10-17-T-193-X-397"])
df_quan$amountSold[df_quan$ID_ENCUESTA=="2023-10-06-T-797-X-251"] <- 
  (209.28/421.8)*(df_quan$amountSold[df_quan$ID_ENCUESTA=="2023-10-06-T-797-X-251"])


### Keeping datasets for sales --------------------------------------------

# Assigning values to sales_process_repeat_df
df_quan_spr <- df_quan %>%
  select(c(ID_ENCUESTA,SALES_REPEAT_ROWID,SALES_PROCESS_REPEAT_ROWID,
           methodOfCoffeeSale,salesMeasurementUnit,amountSold))
sales_process_repeat_df <- sales_process_repeat_df %>%
  select(-c(methodOfCoffeeSale,amountSold,salesMeasurementUnit))
sales_process_repeat_df <- left_join(
  sales_process_repeat_df, df_quan, by = c("ID_ENCUESTA","SALES_REPEAT_ROWID","SALES_PROCESS_REPEAT_ROWID"))

# Assigning values to sales_repeat_df
df_quan_sr <- df_quan %>%
  select(c(ID_ENCUESTA,SALES_REPEAT_ROWID,formInWhichCoffeeWasSold,amountCoffeeSold)) %>%
  group_by(ID_ENCUESTA,SALES_REPEAT_ROWID,formInWhichCoffeeWasSold,amountCoffeeSold)
sales_repeat_df <- sales_repeat_df %>%
  select(-c(amountCoffeeSold,formInWhichCoffeeWasSold))
sales_repeat_df <- left_join(
  sales_repeat_df, df_quan_sr, by = c("ID_ENCUESTA","SALES_REPEAT_ROWID"))

# Removing variables
rm(cases,condition,df_quan,df_quan_spr,df_quan_sr,i)












#' #' ========================================================================
#' #  Checking variables -----------------------------------------------------
#' #' ========================================================================
#' 
#' # Calling data
#' df             <- read.csv("01_data/processed/baseline/baselineAgg.csv")
#' df$ID_ENCUESTA <- base_df$surveyID
#' results        <- readRDS("03_tables/baseline/baselineQualityCheck.RData")
#' 
#' 
#' #' ------------------------------------------------------------------------
#' ## Assessing area variables -----------------------------------------------
#' 
#' # Vars on main sheet
#' base_area <- df %>%
#'   select(c(ID_ENCUESTA, totalParcelAreaInHa_trr,
#'            totalAreaUsedForAgriculture,measurementUnitOfArea,totalAreaUsedForAgricultureInHa,
#'            coffeeAreaLastHarvest,coffeeAreaMeasurementUnit,coffeeAreaLastHarvestInHa,
#'            productiveCoffeeAreaLastHarvest,productiveCoffeeAreaUnit))
#' 
#' 
#' # Vars on area_terr sheet
#' area_terr_area <- area_terr_df %>%
#'   select(c(ID_ENCUESTA,totalParcelArea,parcelAreaMeasurementUnit))
#' 
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
#' coherent_outliers <- c(
#'   "2023-10-06-T-29-X-262","2023-10-07-T-224-X-253","2023-10-14-T-863-X-221","2023-10-18-T-402-X-931","2023-10-23-T-285-X-42","2023-10-23-T-543-X-913",
#'   "2023-10-27-T-8218-X-2768","2023-10-30-T-1899-X-6021","2023-11-02-T-5957-X-7411","2023-11-09-T-499-X-407","2023-10-06-T-294-X-685","2023-10-18-T-350-X-135",
#'   "2023-10-18-T-551-X-811","2023-10-19-T-520-X-976","2023-10-19-T-925-X-541","2023-10-26-T-5986-X-6703","2023-10-26-T-6452-X-3280","2023-10-27-T-5219-X-3944",
#'   "2023-10-28-T-5456-X-4396","2023-10-28-T-590-X-1900","2023-10-28-T-8301-X-2441","2023-10-30-T-1351-X-3126","2023-10-30-T-1529-X-5250","2023-10-30-T-5657-X-2691",
#'   "2023-11-04-T-312-X-272","2023-11-04-T-5605-X-5944","2023-11-09-T-925-X-316")
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
#' coherent_outliers  <- c(
#'   "2023-10-11-T-338-X-589","2023-10-30-T-8181-X-7369","2023-10-12-T-973-X-976","2023-10-30-T-9979-X-7324","2023-10-04-T-781","2023-10-11-T-281-X-96",
#'   "2023-10-21-T-340-X-609","2023-10-21-T-224-X-560","2023-10-18-T-107-X-220","2023-10-27-T-9424-X-4126","2023-10-28-T-9976-X-4731","2023-10-05-T-102-X-917",
#'   "2023-10-12-T-881-X-411","2023-10-05-T-484-X-543","2023-10-16-T-296-X-906","2023-10-04-T-161","2023-10-17-T-609-X-335","2023-10-28-T-6375-X-7226",
#'   "2023-11-07-T-238-X-192","2023-11-09-T-623-X-239","2023-11-07-T-26-X-315","2023-10-19-T-892-X-246","2023-10-19-T-835-X-616","2023-10-05-T-295-X-88",
#'   "2023-10-20-T-617-X-618","2023-10-06-T-891-X-140","2023-10-30-T-997-X-8093","2023-10-26-T-3027-X-3937","2023-10-16-T-42-X-554","2023-10-05-T-245-X-367")
#' df_area_outliers_c6 <- df_area_outliers_c6[!(df_area_outliers_c6$ID_ENCUESTA %in% c(corrected_outliers,coherent_outliers)), ]
#' 
#' ## Case 7: all=TRUE
#' df_area_outliers_c7 <- df_area_outliers %>% filter(
#'     coffeeAreaLastHarvestInHa_outlier       == TRUE  &
#'     totalAreaUsedForAgricultureInHa_outlier == TRUE &
#'     totalParcelAreaInHa_trr_outlier         == TRUE )
#' summary(duplicated(df_area_outliers_c7$ID_ENCUESTA))
#' corrected_outliers <- c("2023-10-21-T-871-X-106","2023-10-06-T-375-X-292","2023-10-11-T-30-X-574")
#' coherent_outliers  <- c(
#'   "2023-11-02-T-5494-X-2657","2023-10-27-T-849-X-249","2023-10-21-T-508-X-455","2023-10-27-T-276-X-8288","2023-10-27-T-3443-X-5030","2023-10-30-T-892-X-5350",
#'   "2023-10-26-T-4117-X-622","2023-10-28-T-4882-X-1578","2023-10-26-T-404-X-1619","2023-10-18-T-365-X-326","2023-10-07-T-986-X-326","2023-10-27-T-2212-X-6213",
#'   "2023-10-23-T-353-X-716","2023-11-07-T-559-X-736","2023-11-07-T-245-X-63","2023-10-06-T-880-X-873","2023-10-30-T-5140-X-330","2023-10-11-T-347-X-936",
#'   "2023-11-06-T-157-X-625","2023-10-26-T-9490-X-4301","2023-10-23-T-406-X-207","2023-10-19-T-665-X-248","2023-11-09-T-645-X-796","2023-11-06-T-655-X-804",
#'   "2023-10-28-T-596-X-5729","2023-10-23-T-513-X-947","2023-10-27-T-1181-X-1961","2023-10-07-T-335-X-365","2023-10-20-T-375-X-397","2023-11-02-T-2012-X-6961",
#'   "2023-11-04-T-428-X-512","2023-10-26-T-8115-X-3427","2023-10-30-T-1841-X-1411","2023-11-08-T-514-X-232","2023-11-09-T-656-X-642","2023-10-26-T-2590-X-8500",
#'   "2023-10-18-T-322-X-965","2023-11-07-T-146-X-994","2023-10-28-T-7752-X-8204","2023-10-26-T-650-X-4386","2023-10-19-T-104-X-957","2023-10-06-T-504-X-978",
#'   "2023-11-02-T-4070-X-6073","2023-10-09-T-125-X-176","2023-10-28-T-3559-X-6172","2023-10-26-T-7095-X-9140","2023-10-21-T-913-X-279","2023-10-19-T-49-X-782",
#'   "2023-10-17-T-902-X-123","2023-10-17-T-889-X-836","2023-10-14-T-134-X-280","2023-10-28-T-3129-X-6097","2023-10-26-T-5798-X-3347","2023-10-16-T-667-X-326",
#'   "2023-10-07-T-211-X-688","2023-10-11-T-304-X-496","2023-10-18-T-46-X-810","2023-10-26-T-9700-X-1198","2023-10-07-T-769-X-887","2023-11-02-T-5189-X-1159")
#' df_area_outliers_c7 <- df_area_outliers_c7[!(df_area_outliers_c7$ID_ENCUESTA %in% c(corrected_outliers,coherent_outliers)), ]
#' 
#' ## Deleting variables
#' rm(vars_check,flag,df_area_outliers,df_area_outliers_c1,df_area_outliers_c2,
#'    df_area_outliers_c3,df_area_outliers_c4,df_area_outliers_c5,df_area_outliers_c6,
#'    df_area_outliers_c7,corrected_outliers,coherent_outliers)
#' 
#' 
#' ### Comparing areas ------------------------
#' # Case 1: coffeeAreaLastHarvestInHa > totalAreaUsedForAgricultureInHa
#' df_area_comparing_c1 <- df_area %>% filter(
#'   coffeeAreaLastHarvestInHa > totalAreaUsedForAgricultureInHa)
#' summary(duplicated(df_area_comparing_c1$ID_ENCUESTA))
#' false_comparing <- c("2023-11-04-T-410-X-157")
#' cortd_comparing <- c("2023-10-03-T-104","2023-10-03-T-68","2023-10-06-T-504-X-978",
#'                      "2023-10-11-T-30-X-574","2023-10-17-T-303-X-191","2023-11-06-T-507-X-86",
#'                      "2023-11-08-T-336-X-669","2023-10-30-T-2455-X-2066")
#' df_area_comparing_c1 <- df_area_comparing_c1[!(df_area_comparing_c1$ID_ENCUESTA %in% c(cortd_comparing,false_comparing)), ]
#' 
#' # Case 2: coffeeAreaLastHarvestInHa > totalParcelAreaInHa_trr
#' df_area_comparing_c2 <- df_area %>% filter(
#'   coffeeAreaLastHarvestInHa > totalParcelAreaInHa_trr)
#' summary(duplicated(df_area_comparing_c2$ID_ENCUESTA))
#' cortd_comparing <- c("2023-11-08-T-336-X-669","2023-10-11-T-30-X-574","2023-10-06-T-134-X-681",
#'                      "2023-10-09-T-118-X-697","2023-10-30-T-2455-X-2066","2023-10-05-T-363-X-749",
#'                      "2023-10-06-T-170-X-101","2023-10-03-T-368","2023-10-03-T-99",
#'                      "2023-10-04-T-661","2023-10-03-T-643")
#' df_area_comparing_c2 <- df_area_comparing_c2[!(df_area_comparing_c2$ID_ENCUESTA %in% cortd_comparing), ]
#' 
#' # Case 3: totalAreaUsedForAgricultureInHa > totalParcelAreaInHa_trr
#' df_area_comparing_c3 <- df_area %>% filter(
#'   totalAreaUsedForAgricultureInHa > totalParcelAreaInHa_trr)
#' summary(duplicated(df_area_comparing_c3$ID_ENCUESTA))
#' false_comparing <- c()
#' cortd_comparing <- c(
#'   "2023-10-03-T-368","2023-10-03-T-372","2023-10-03-T-389","2023-10-03-T-525","2023-10-03-T-643","2023-10-03-T-708",
#'   "2023-10-03-T-99","2023-10-04-T-583","2023-10-04-T-625","2023-10-03-T-497","2023-10-04-T-661","2023-10-05-T-363-X-749",
#'   "2023-10-05-T-778-X-313","2023-10-05-T-888-X-179","2023-10-06-T-134-X-681","2023-10-06-T-170-X-101","2023-10-06-T-180-X-981","2023-10-06-T-316-X-491",
#'   "2023-10-06-T-375-X-292","2023-10-06-T-787-X-371","2023-10-07-T-722-X-380","2023-10-07-T-950-X-608","2023-10-09-T-118-X-697","2023-10-09-T-178-X-346",
#'   "2023-10-09-T-181-X-688","2023-10-09-T-270-X-331","2023-11-09-T-574-X-215","2023-11-08-T-197-X-859","2023-10-28-T-7560-X-6247","2023-10-28-T-5335-X-8873",
#'   "2023-10-28-T-485-X-4910","2023-10-27-T-7525-X-2130","2023-10-23-T-543-X-913","2023-10-21-T-871-X-106","2023-10-21-T-587-X-156","2023-10-21-T-188-X-319",
#'   "2023-10-21-T-106-X-837","2023-10-20-T-787-X-110","2023-10-20-T-464-X-62","2023-11-09-T-306-X-828","2023-10-20-T-442-X-214","2023-10-20-T-341-X-157",
#'   "2023-10-19-T-347-X-954","2023-10-18-T-984-X-562","2023-10-18-T-804-X-504","2023-10-18-T-714-X-17","2023-10-17-T-899-X-318","2023-10-17-T-675-X-673",
#'   "2023-10-17-T-496-X-166","2023-10-17-T-452-X-770","2023-10-16-T-694-X-362","2023-10-11-T-830-X-837","2023-10-11-T-387-X-366","2023-10-07-T-892-X-832",
#'   "2023-10-09-T-587-X-865","2023-10-11-T-490-X-800","2023-10-14-T-40-X-353")
#' df_area_comparing_c3 <- df_area_comparing_c3[!(df_area_comparing_c3$ID_ENCUESTA %in% c(cortd_comparing,false_comparing)), ]
#' 
#' #Removing variables
#' rm(df_area_comparing_c1,df_area_comparing_c2,df_area_comparing_c3,area_terr_df,
#'    area_terr_dict,df_area,cortd_comparing)
#' 
#' 
#' #' ------------------------------------------------------------------------
#' ## Assessing quantity variables -------------------------------------------
#' 
#' ### Organizing data ------------------------
#' 
#' # Standardizing quantity to green kg
#' df_quan_comparing <- standardize_weights(df_quan_missing,sales_process_repeat_dict)
#' names(df_quan_comparing)
#' df_quan_comp_agg <- df_quan_comparing %>%
#'   group_by(
#'     ID_ENCUESTA,typeOfCoffeeProduced,coffeeProductionMeasurementUnit,amountOfCoffeeProducedLastHarvest) %>%
#'   summarise(
#'     amountSoldInKgGreen = sum(amountSoldInKgGreen, na.rm = TRUE),
#'     .groups = "drop"
#'   )
#' df_quan_comp_agg <- standardize_weights_prod(df_quan_comparing,dictionary)
#' names(df_quan_comp_agg)
#' 
#' 
#' ### Comparing quantities ------------------------
#' # Checking condition fulfillment
#' df_quan_comp_flag <- df_quan_comp_agg %>% filter(
#'   amountOfCoffeeProducedLastHarvestInKgGreen < amountSoldInKgGreen)
#' false_comparing <- c("2023-10-21-T-508-X-455")
#' cortd_comparing <- c("2023-10-17-T-742-X-913","2023-10-18-T-676-X-692","2023-10-26-T-1302-X-9320",
#'                      "2023-10-26-T-8427-X-8270","2023-10-26-T-9216-X-5786","2023-10-03-T-332",
#'                      "2023-10-06-T-155-X-2","2023-10-06-T-797-X-251","2023-10-17-T-193-X-397",
#'                      "2023-10-17-T-792-X-372","2023-10-26-T-211-X-4417","2023-10-06-T-375-X-292")
#' df_quan_comp_flag <- df_quan_comp_flag[!(df_quan_comp_flag$ID_ENCUESTA %in% c(cortd_comparing,false_comparing)), ]
#' 
#' # Removing variables
#' rm(df_quan_comp_flag,false_comparing,cortd_comparing,df_quan_comparing)
#' 
#' 
#' ### Outliers ------------------------
#' # Separating observations with outliers (based on quality check)
#' df_quan_out <- df %>% #base_df
#'   select(c(surveyID,amountOfCoffeeProducedLastHarvestInKgGreen,amountSoldInKgGreen_typ))
#' 
#' vars_check <- c("amountOfCoffeeProducedLastHarvestInKgGreen","amountSoldInKgGreen_typ")
#' ## Loop by outlier variable
#' for(v in vars_check){
#'   df_quan_out[[paste0(v, "_outlier")]] <-
#'     !is.na(df_quan_out[[v]]) & (
#'       df_quan_out[[v]] < as.numeric(results$lower_bound[[v]]) |
#'         df_quan_out[[v]] > as.numeric(results$upper_bound[[v]])
#'     )
#' }
#' ## Keeping only rows with at least 1 outlier
#' flag             <- rowSums(df_quan_out[paste0(vars_check, "_outlier")]) > 0
#' df_quan_outliers <- df_quan_out[df_quan_out$surveyID %in% df_quan_out$surveyID[flag], ]
#' 
#' # Evaluating cases with any outlier
#' results[["upper_bound"]][["amountOfCoffeeProducedLastHarvestInKgGreen"]]
#' results[["upper_bound"]][["amountSoldInKgGreen_typ"]]
#' corrected_outliers <- c(
#'   "2023-10-06-T-375-X-292","2023-10-06-T-880-X-873","2023-10-06-T-504-X-978",
#'   "2023-10-17-T-193-X-397","2023-10-06-T-797-X-251","2023-10-17-T-681-X-44",
#'   "2023-10-18-T-46-X-810")
#' coherent_outliers <- c(
#'   "2023-10-26-T-4117-X-622","2023-10-26-T-404-X-1619","2023-10-27-T-1181-X-1961","2023-10-30-T-1841-X-1411","2023-11-02-T-5189-X-1159","2023-11-07-T-245-X-63",
#'   "2023-10-27-T-3443-X-5030","2023-10-20-T-375-X-397","2023-10-28-T-596-X-5729","2023-10-26-T-9490-X-4301","2023-10-26-T-650-X-4386","2023-10-28-T-3129-X-6097",
#'   "2023-10-21-T-508-X-455","2023-10-30-T-892-X-5350","2023-10-28-T-3559-X-6172","2023-11-07-T-559-X-736","2023-10-27-T-2212-X-6213","2023-10-28-T-4882-X-1578",
#'   "2023-10-23-T-513-X-947","2023-10-28-T-7752-X-8204","2023-10-18-T-365-X-326","2023-10-19-T-49-X-782","2023-10-27-T-8218-X-2768","2023-11-09-T-656-X-642",
#'   "2023-10-27-T-276-X-8288","2023-10-05-T-129-X-990","2023-10-30-T-5140-X-330","2023-10-07-T-211-X-688","2023-10-23-T-353-X-716","2023-11-07-T-994-X-600",
#'   "2023-11-02-T-5494-X-2657","2023-10-07-T-335-X-365","2023-10-30-T-5657-X-2691","2023-10-23-T-543-X-913","2023-10-23-T-285-X-42","2023-10-27-T-6069-X-8842",
#'   "2023-10-11-T-30-X-574","2023-10-06-T-134-X-681","2023-11-08-T-514-X-232","2023-10-17-T-902-X-123","2023-10-26-T-3027-X-3937","2023-10-26-T-5986-X-6703",
#'   "2023-10-26-T-7095-X-9140","2023-10-27-T-849-X-249","2023-10-28-T-590-X-1900","2023-10-30-T-3183-X-3566","2023-11-02-T-4070-X-6073","2023-10-06-T-29-X-262",
#'   "2023-10-23-T-406-X-207","2023-10-17-T-496-X-166","2023-10-11-T-304-X-496","2023-10-28-T-1723-X-9760","2023-10-28-T-5456-X-4396","2023-10-21-T-323-X-214",
#'   "2023-10-11-T-338-X-589","2023-11-04-T-23-X-4662","2023-10-26-T-2590-X-8500","2023-10-19-T-136-X-862","2023-10-06-T-457-X-755","2023-10-16-T-139-X-588",
#'   "2023-11-04-T-886-X-182","2023-10-30-T-7901-X-9723","2023-10-27-T-1783-X-2185","2023-10-05-T-778-X-313","2023-10-16-T-667-X-326","2023-10-19-T-178-X-800",
#'   "2023-11-02-T-9881-X-549","2023-10-23-T-921-X-511","2023-10-19-T-665-X-248","2023-10-30-T-1351-X-3126","2023-11-04-T-428-X-512","2023-11-06-T-157-X-625",
#'   "2023-11-09-T-645-X-796","2023-10-26-T-5798-X-3347","2023-10-26-T-6452-X-3280","2023-10-26-T-7265-X-4121","2023-10-28-T-5335-X-8873","2023-10-30-T-1899-X-6021",
#'   "2023-10-28-T-3402-X-6197","2023-11-02-T-1127-X-9406","2023-10-26-T-3540-X-2649","2023-10-21-T-871-X-106","2023-10-26-T-5662-X-1743","2023-10-20-T-978-X-436",
#'   "2023-11-04-T-312-X-272","2023-10-26-T-6854-X-8114","2023-10-28-T-1620-X-3230","2023-10-28-T-3671-X-2734","2023-11-04-T-5605-X-5944","2023-10-27-T-5219-X-3944",
#'   "2023-11-07-T-196-X-464","2023-11-02-T-2012-X-6961","2023-10-18-T-402-X-931","2023-10-21-T-340-X-609","2023-10-27-T-668-X-5678","2023-10-28-T-9976-X-4731",
#'   "2023-10-30-T-5368-X-952")
#' df_quan_outliers <- df_quan_outliers[!(df_quan_outliers$surveyID %in% c(corrected_outliers,coherent_outliers)), ]
#' 
#' ## Deleting variables
#' rm(vars_check,flag,df_area_outliers,df_area_outliers_c1,df_area_outliers_c2,
#'    df_area_outliers_c3,df_area_outliers_c4,df_area_outliers_c5,df_area_outliers_c6,
#'    df_area_outliers_c7,corrected_outliers,coherent_outliers)
#' 
#' 
#' 
#' #' ------------------------------------------------------------------------
#' ## Assessing number of plants variables -----------------------------------
#' 
#' ### Outliers ------------------------
#' #' The number of plants per hectare was checked to ensure it fell between 
#' #' 2,334 (2 m × 1.5 m) and 7,000 (1 m × 1 m), and adjustments were made in the 
#' #' Excel. Virtually, all outliers correspond to large coffee-growing areas.
#' 
#' # Separating observations with outliers (based on quality check)
#' df_plant <- df %>% #base_df
#'   select(c(
#'     surveyID,coffeeAreaLastHarvest,coffeeAreaMeasurementUnit,
#'     productiveCoffeeAreaLastHarvest,productiveCoffeeAreaUnit,
#'     totalCoffeePlantsOnFarm,spacingBetweenPlantsInMt,spacingBetweenRowsInMt,
#'     numberProductivePlants))
#' 
#' vars_check <- c("totalCoffeePlantsOnFarm","numberProductivePlants")
#' # Loop by outlier variable
#' for(v in vars_check){
#'   ## Numbers
#'   IQR_value <- IQR(df_plant[[v]], na.rm = TRUE)
#'   Q1        <- quantile(df_plant[[v]], 0.25, na.rm = TRUE)
#'   Q3        <- quantile(df_plant[[v]], 0.75, na.rm = TRUE)
#'   lower_bound <- Q1 - 3 * IQR_value
#'   upper_bound <- Q3 + 3 * IQR_value
#'   
#'   ## Flagging
#'   df_plant[[paste0(v, "_outlier")]] <-
#'     !is.na(df_plant[[v]]) & (
#'       df_plant[[v]] < lower_bound | df_plant[[v]] > upper_bound)
#' }
#' # Keeping only rows with at least 1 outlier
#' flag         <- rowSums(df_plant[paste0(vars_check, "_outlier")]) > 0
#' df_plant_out <- df_plant[df_plant$surveyID %in% df_plant$surveyID[flag], ]
#' 
#' # Deleting variables
#' rm(df_plant,vars_check,v,IQR_value,Q1,Q3,lower_bound,upper_bound,flag,df_plant_out)
#' 
#' 
#' 
#' #' ------------------------------------------------------------------------
#' ## Assessing density of plants --------------------------------------------
#' 
#' ### Outliers ------------------------
#' # Separating observations with outliers (based on quality check)
#' # df_dens_out <- df %>% #base_df
#' #   select(c(surveyID,amountOfCoffeeProducedLastHarvestInKgGreen,amountSoldInKgGreen_typ))
#' # 
#' # vars_check <- c("amountOfCoffeeProducedLastHarvestInKgGreen","amountSoldInKgGreen_typ")
#' # ## Loop by outlier variable
#' # for(v in vars_check){
#' #   df_quan_out[[paste0(v, "_outlier")]] <-
#' #     !is.na(df_quan_out[[v]]) & (
#' #       df_quan_out[[v]] < as.numeric(results$lower_bound[[v]]) |
#' #         df_quan_out[[v]] > as.numeric(results$upper_bound[[v]])
#' #     )
#' # }
#' # ## Keeping only rows with at least 1 outlier
#' # flag             <- rowSums(df_quan_out[paste0(vars_check, "_outlier")]) > 0
#' # df_quan_outliers <- df_quan_out[df_quan_out$surveyID %in% df_quan_out$surveyID[flag], ]
#' # 
#' # # Evaluating cases with any outlier
#' # results[["upper_bound"]][["amountOfCoffeeProducedLastHarvestInKgGreen"]]
#' # results[["upper_bound"]][["amountSoldInKgGreen_typ"]]
#' # corrected_outliers <- c(
#' #   )
#' # coherent_outliers <- c(
#' #   )
#' # df_quan_outliers <- df_quan_outliers[!(df_quan_outliers$surveyID %in% c(corrected_outliers,coherent_outliers)), ]
#' # 
#' # ## Deleting variables
#' # rm()
#' 
