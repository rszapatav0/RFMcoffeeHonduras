#Seleccionando puntos y polígonos por parcela
#Nota: este script genera un excel con el número de puntos y polígonos que deben
#tomarse por productor. Está hecho para pegarse a la encuesta (en Excel).
#Nota2: si se va a correr, cambiar el la ruta en la línea 69.

#setwd("C:/Users/feder.DESKTOP-471V2SD/OneDrive - CGIAR/RMI WP1/Armonizacion")
setwd("C:/Users/rszap/OneDrive - Universidad EAFIT/CIAT/RFM WP1/Armonizacion/")
rm(list=ls())

excel_path      <- "./Endline/endlineRaw.xlsx"
dictionary_path <- "./dictionary.xlsx"
output_path     <- "./dictionary.csv"

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
