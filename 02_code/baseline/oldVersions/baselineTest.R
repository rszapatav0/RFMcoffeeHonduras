
#sales_process_repeat
sales_process_df         <- read_excel(excel_path, sheet = "sales_process_repeat")
sales_process_dict       <- dictionary %>% filter(bl_loop == "sales_process_repeat")
#sales_process_df         <- convert_numeric_columns(sales_process_df, sales_process_dict)
#sales_process_df_aux     <- standardize_weights(sales_process_df, sales_process_dict)
#sales_process_df_aux     <- standardize_prices(sales_process_df, sales_process_dict)
aggregated_sales_process <- aggregate_data(sales_process_df, sales_process_dict, "ID_ENCUESTA", "_typ")
colnames(aggregated_sales_process)


#sales_repeat
sales_df         <- read_excel(excel_path, sheet = "sales_repeat")
sales_dict       <- dictionary %>% filter(bl_loop == "sales_repeat")
#sales_df  <- convert_numeric_columns(sales_df, sales_dict)
#sales_df  <- convert_to_dummies_multiple(sales_df, sales_dict)
sales_df         <- left_join(sales_df, aggregated_sales_process, by = "ID_ENCUESTA")
aggregated_sales <- aggregate_data(sales_df, sales_dict, "ID_ENCUESTA", "_buy")
colnames(aggregated_sales)

#lands
lands_df         <- read_excel(excel_path, sheet = "area_terr")
lands_dict       <- dictionary %>% filter(bl_loop == "area_terr")
#lands_df <- convert_numeric_columns(lands_df,lands_dict)  # Assuming this function is defined to convert appropriate columns to numeric
#lands_df <- convert_to_dummies(lands_df, lands_dict)
#lands_df <- convert_to_dummies_multiple(lands_df, lands_dict)
#lands_df <- standardize_areas(lands_df, lands_dict)
aggregated_lands <- aggregate_data(lands_df, lands_dict, "ID_ENCUESTA", "_trr")
colnames(aggregated_lands)

#repeatvar
repeatvar_df         <- read_excel(excel_path, sheet = "repeatvar")
repeatvar_dict       <- dictionary %>% filter(bl_loop == "repeatvar")
aggregated_repeatvar <- aggregate_data(repeatvar_df, repeatvar_dict, "ID_ENCUESTA", "_var")
colnames(aggregated_repeatvar)

#pnd_repeat
pnd_repeat_df         <- read_excel(excel_path, sheet = "pnd_repeat")
pnd_repeat_dict       <- dictionary %>% filter(bl_loop == "pnd_repeat")
#pnd_repeat_df <- convert_numeric_columns(pnd_repeat_df,pnd_repeat_dict)  # Assuming this function is defined to convert appropriate columns to numeric
#pnd_repeat_df <- convert_to_dummies(pnd_repeat_df, pnd_repeat_dict)
#pnd_repeat_df <- convert_to_dummies_multiple(pnd_repeat_df, pnd_repeat_dict)
aggregated_pnd_repeat <- aggregate_data(pnd_repeat_df, pnd_repeat_dict, "ID_ENCUESTA", "_dop")
colnames(aggregated_pnd_repeat)

#tech_repeat
tech_repeat_df         <- read_excel(excel_path, sheet = "tech_repeat")
tech_repeat_dict       <- dictionary %>% filter(bl_loop == "tech_repeat")
#tech_repeat_df <- convert_numeric_columns(tech_repeat_df,tech_repeat_dict)  # Assuming this function is defined to convert appropriate columns to numeric
#tech_repeat_df <- convert_to_dummies(tech_repeat_df, tech_repeat_dict)
#tech_repeat_df <- convert_to_dummies_multiple(tech_repeat_df, tech_repeat_dict)
aggregated_tech_repeat <- aggregate_data(tech_repeat_df, tech_repeat_dict, "ID_ENCUESTA", "_tec")
colnames(aggregated_tech_repeat)

#hh_pop_repeat
hh_pop_repeat_df         <- read_excel(excel_path, sheet = "hh_pop_repeat")
hh_pop_repeat_dict       <- dictionary %>% filter(bl_loop == "hh_pop_repeat")
#hh_pop_repeat_df <- convert_numeric_columns(hh_pop_repeat_df,hh_pop_repeat_dict)  # Assuming this function is defined to convert appropriate columns to numeric
#hh_pop_repeat_df <- convert_to_dummies(hh_pop_repeat_df, hh_pop_repeat_dict)
#hh_pop_repeat_df <- convert_to_dummies_multiple(hh_pop_repeat_df, hh_pop_repeat_dict)
aggregated_hh_pop_repeat <- aggregate_data(hh_pop_repeat_df, hh_pop_repeat_dict, "ID_ENCUESTA", "_pop")
colnames(aggregated_hh_pop_repeat)

############################

sales_process_df         <- read_excel(excel_path, sheet = "sales_process_repeat")
sales_process_dict       <- dictionary %>% filter(bl_loop == "sales_process_repeat")
aggregated_sales_process <- aggregate_data(sales_process_df, sales_process_dict, "_submission__uuid", "_typ")
colnames(aggregated_sales_process)

df <- sales_process_df
dict <- sales_process_dict
tab_suffix <- "_typ"

sales_process_df_2 <- standardize_prices(df, dict)

############################

# Verificar si ambas columnas no tienen valores al mismo tiempo
ambas_con_valor <- df %>%
     filter(!is.na("landLegalStatusForCultivation/own_land_trr") & !is.na("landLegalStatusForCultivation/own_land"))

# Verificar si hay filas donde ambas columnas son NA
ambas_NA <- df %>%
     filter(is.na("landLegalStatusForCultivation/own_land_trr") & is.na("landLegalStatusForCultivation/own_land"))

# Resultados
cat("Número de observaciones donde ambas columnas tienen valores: ", nrow(ambas_con_valor), "\n")
cat("Número de observaciones donde ambas columnas son NA: ", nrow(ambas_NA), "\n")

# Filtrar las filas donde ambas columnas son NA y seleccionar la columna _submission__uuid
uuid_ambas_NA <- df %>%
     filter(is.na(coffeeAreaLastHarvestInHectares_trr) & is.na(E2XC)) %>%
     select("_uuid")
uuid_coffe_area_zero <- df %>%
     filter(coffeeAreaLastHarvestInHectares_trr==0) %>%
     select("surveyID")

# Ver los valores
uuid_coffe_area_zero
print(n = uuid_coffe_area_zero)

##########################

# For the report
# Lista de nombres de las columnas
column_names <- c("amountSoldInKgGreen", 
                  "totalFarmAreaInHectares_trr", 
                  "totalAreaUsedForAgricultureInHectares_trr", 
                  "coffeeAreaLastHarvestInHectares_trr", 
                  "minimumReceivedPriceForCoffeeInGreenKgs_typ_buy", 
                  "maximumReceivedPriceForCoffeeInGreenKgs_typ_buy", 
                  "amountOfCoffeeProducedLastHarvestInGreenKgs", 
                  "yieldKgPerHectares", 
                  "densityPlantsPerHectares")

# Loop para iterar sobre las columnas
for (i in seq_along(column_names)) {
  column <- column_names[i]
  
  # Crear un nuevo nombre para la variable outlier
  outlier_col <- paste0("out_", column)
  
  # Mutar el dataframe agregando la nueva columna de outliers
  df <- df %>%
    mutate(!!outlier_col := as.numeric(.data[[column]] > results[["upper_bound"]][[i]]))

}

df2 <- df %>% select(surveyID, completeNameReplacement,out_amountSoldInKgGreen,out_totalFarmAreaInHectares_trr, 
                     out_totalAreaUsedForAgricultureInHectares_trr, out_coffeeAreaLastHarvestInHectares_trr, 
                     out_minimumReceivedPriceForCoffeeInGreenKgs_typ_buy,out_maximumReceivedPriceForCoffeeInGreenKgs_typ_buy, 
                     out_amountOfCoffeeProducedLastHarvestInGreenKgs,out_yieldKgPerHectares, out_densityPlantsPerHectares)

library(openxlsx)
write.xlsx(df2, "outliers_report.xlsx")

########################################

#Lands loop
lands_df <- read_excel(excel_path, sheet = "lands")
lands_df <- lands_df %>%
  select(
    c(E3XC, E4C, E2XC, '_submission__uuid')
    ) %>%
  rename(
    totalFarmAreaInHectares = E3XC, totalAreaUsedForAgricultureInHectares = E4C, coffeeAreaLastHarvestInHectares = E2XC
  ) %>%
  mutate(
    gps_polygon = ifelse(totalFarmAreaInHectares >= 4 & coffeeAreaLastHarvestInHectares > 0, 1, 0),
    gps_point   = ifelse(totalFarmAreaInHectares <  4 & coffeeAreaLastHarvestInHectares > 0, 1, 0)
  )
#Lands group
lands_grouped_df <- lands_df %>%
  group_by(`_submission__uuid`) %>%
  summarise(
    gps_polygon_sum = sum(gps_polygon, na.rm = TRUE),
    gps_point_sum   = sum(gps_point,   na.rm = TRUE)
  )


#Base completa
base_df <- read_excel(excel_path, sheet = "Encuesta de Línea Intermedia...") %>%
  select(
    c(surveyID, '_index', '_uuid', numberOfLands, E3XC, E4C, E2XC)
         ) %>%
  rename(
    totalFarmAreaInHectares = E3XC, totalAreaUsedForAgricultureInHectares = E4C, coffeeAreaLastHarvestInHectares = E2XC
  ) %>%
mutate(
  gps_polygon = ifelse(totalFarmAreaInHectares >= 4 & coffeeAreaLastHarvestInHectares > 0, 1, 0),
  gps_point   = ifelse(totalFarmAreaInHectares <  4 & coffeeAreaLastHarvestInHectares > 0, 1, 0)
) %>%
  select(-c(totalFarmAreaInHectares, totalAreaUsedForAgricultureInHectares, coffeeAreaLastHarvestInHectares))


#Joining
df <- left_join(base_df, lands_grouped_df, by = c("_uuid"="_submission__uuid"))
df <- df %>%
  mutate(
    gps_polygon_sum = ifelse(is.na(gps_polygon_sum), gps_polygon, gps_polygon_sum),
    gps_point_sum   = ifelse(is.na(gps_point_sum),   gps_point,   gps_point_sum)
  ) %>%
  select(
    -(c(gps_polygon, gps_point, '_uuid'))
  ) %>%
  rename(
    gps_point = gps_point_sum, gps_polygon = gps_polygon_sum, 
  ) %>% 
  arrange('_index')

#Saving
library(writexl)
write_xlsx(df, path = "C:/Users/rszap/OneDrive - Universidad EAFIT/CIAT/RFM WP1/Línea final/Geolocalización2/PointsAndPolygons.xlsx")
