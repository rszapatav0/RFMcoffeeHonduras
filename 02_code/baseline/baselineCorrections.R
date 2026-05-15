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



#' ------------------------------------------------------------------------
## sales_process and sales_process_repeat sheets' -------------------------

### Excel corrections to sales_process_repeat -----------------------------

# Reading Excel
sales_process_repeat_chg <- read_excel(changes_path, sheet = "sales_process_repeat")


# Changing values
## Variables to change
vars_corregidas <- names(sales_process_repeat_chg)[
  !(names(sales_process_repeat_chg) %in% c(
    "ID_ENCUESTA","SALES_REPEAT_ROWID","SALES_PROCESS_REPEAT_ROWID","ROWUUID","commentsOnChanges"))]

## Loop by var
for (var in vars_corregidas) {
  ### Matching name
  var_cor <- paste0(var, "_mapvar")
  ### Map for observations with changes
  map_var <- sales_process_repeat_chg %>%
    filter(!is.na(.data[[var]])) %>%
    select("ID_ENCUESTA","ROWUUID", !!sym(var))
  ### Changing df
  sales_process_repeat_df <- sales_process_repeat_df %>%
    left_join(map_var,
              by = c("ID_ENCUESTA","ROWUUID"), 
              suffix = c("", "_mapvar")) %>%
    mutate(
      !!sym(var) := ifelse(!is.na(.data[[var_cor]]),
                           .data[[var_cor]],
                           .data[[var]])
    )
  ### Cleaning auxiliary columns
  sales_process_repeat_df <- sales_process_repeat_df %>%
    select(-all_of(var_cor), -matches("_mapvar$"))
}

rm("base_chg","area_terr_chg","sales_process_repeat_chg",
   "map_var","vars_corregidas","var","var_cor")


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
                  "2023-10-16-T-139-X-588","2023-10-09-T-596-X-833","2023-10-27-T-627-X-3154",
                  "2023-10-28-T-4920-X-984","2023-11-08-T-42-X-155","2023-10-28-T-6683-X-9908"),
  amountCoffeeSold = c(4000,80,740,
                       40,15,300,
                       70,5,154)
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




