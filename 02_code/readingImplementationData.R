#Anonimizing implementation data

# Packages
library(openxlsx)
set.seed(21)

# Log data ---------------------------------------------------------------------
rm(list=ls())
#smslog1 <- read.csv("C:/Users/rszap/OneDrive - Universidad EAFIT/CIAT/RFMcoffeeHonduras/01_data/raw/implementation/----.csv")
#AC2510...2024-05-06
#smslog2 <- read.csv("C:/Users/rszap/OneDrive - Universidad EAFIT/CIAT/RFMcoffeeHonduras/01_data/raw/implementation/----.csv")
#ACbe98...2024-05-06

## smslog1 -------------------------

# 0. Variables auxiliares
smslog1$To_aux   <- smslog1$To
smslog1$From_aux <- smslog1$From
# 1. Valores únicos de la variable, tal como están
unicosTo   <- unique(smslog1$To)
unicosFrom <- unique(smslog1$From)
# 2. Mezclar el orden al azar (opcional, recomendado)
unicosTo   <- sample(unicosTo)
unicosFrom <- sample(unicosFrom)
# 3. Asignar un ID a cada valor único: 1, 2, 3, ...
smslog1$To   <- match(smslog1$To_aux, unicosTo)
smslog1$From <- match(smslog1$From_aux, unicosFrom)
# 4. Quitar la variable original
smslog1$To_aux   <- NULL
smslog1$From_aux <- NULL


## smslog2 -------------------------

# 0. Variables auxiliares
smslog2$To_aux   <- smslog2$To
smslog2$From_aux <- smslog2$From
# 1. Valores únicos de la variable, tal como están
unicosTo   <- unique(smslog2$To)
unicosFrom <- unique(smslog2$From)
# 2. Mezclar el orden al azar (opcional, recomendado)
unicosTo   <- sample(unicosTo)
#unicosFrom <- sample(unicosFrom)
# 3. Asignar un ID a cada valor único: 1, 2, 3, ...
smslog2$To   <- match(smslog2$To_aux, unicosTo)
smslog2$From <- match(smslog2$From_aux, unicosFrom)
# 4. Quitar la variable original
smslog2$To_aux   <- NULL
smslog2$From_aux <- NULL


## Saving data -------------------------
file_path <- "C:/Users/rszap/OneDrive - Universidad EAFIT/CIAT/RFMcoffeeHonduras/01_data/processed/implementation/implementationJoined.xlsx"
wb        <- loadWorkbook(file_path)
writeData(wb, "smslog1", smslog1)
writeData(wb, "smslog2", smslog2)
saveWorkbook(wb, file_path, overwrite = TRUE)



# Capacitaciones data ----------------------------------------------------------
rm(list=ls())
group <- read_excel("C:/Users/rszap/OneDrive - Universidad EAFIT/CIAT/RFMcoffeeHonduras/01_data/raw/implementation/trainingJoined.xlsx",
                    sheet="CAPACITACIONES")
indiv <- read_excel("C:/Users/rszap/OneDrive - Universidad EAFIT/CIAT/RFMcoffeeHonduras/01_data/raw/implementation/trainingJoined.xlsx",
                    sheet="VISITAS")

## group -------------------------

# 0. Variables auxiliares
group$PRODUCTORES_aux <- group$PRODUCTORES
group$TECNICO_aux     <- group$TECNICO
# 1. Valores únicos de la variable, tal como están
unicosPRODUCTORES <- unique(group$PRODUCTORES)
unicosTECNICO     <- unique(group$TECNICO)
# 2. Mezclar el orden al azar (opcional, recomendado)
unicosPRODUCTORES <- sample(unicosPRODUCTORES)
unicosTECNICO     <- sample(unicosTECNICO)
# 3. Asignar un ID a cada valor único: 1, 2, 3, ...
group$PRODUCTORES <- match(group$PRODUCTORES_aux, unicosPRODUCTORES)
group$TECNICO     <- match(group$TECNICO_aux, unicosTECNICO)
# 4. Quitar la variable original
group$PRODUCTORES_aux <- NULL
group$TECNICO_aux     <- NULL


## indiv -------------------------

# 0. Variables auxiliares
indiv$PRODUCTOR_aux <- indiv$PRODUCTOR
indiv$TECNICO_aux   <- indiv$TECNICO
# 1. Valores únicos de la variable, tal como están
unicosPRODUCTOR <- unique(indiv$PRODUCTOR)
unicosTECNICO   <- unique(indiv$TECNICO)
# 2. Mezclar el orden al azar (opcional, recomendado)
unicosPRODUCTOR <- sample(unicosPRODUCTOR)
unicosTECNICO   <- sample(unicosTECNICO)
# 3. Asignar un ID a cada valor único: 1, 2, 3, ...
indiv$PRODUCTOR <- match(indiv$PRODUCTOR_aux, unicosPRODUCTOR)
indiv$TECNICO   <- match(indiv$TECNICO_aux, unicosTECNICO)
# 4. Quitar la variable original
indiv$PRODUCTOR_aux <- NULL
indiv$TECNICO_aux   <- NULL

# Coordenadas
indiv$LATITUD <- round(indiv$LATITUD, 1)
indiv$LONGTUD <- round(indiv$LONGTUD, 1)


## Saving data -------------------------
file_path <- "C:/Users/rszap/OneDrive - Universidad EAFIT/CIAT/RFMcoffeeHonduras/01_data/processed/implementation/implementationJoined.xlsx"
wb        <- loadWorkbook(file_path)
writeData(wb, "Individual Training", indiv)
writeData(wb, "Group Training", group)
saveWorkbook(wb, file_path, overwrite = TRUE)
