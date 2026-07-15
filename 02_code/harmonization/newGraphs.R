
# Loading Packages
packageList <- c("dplyr","tidyr","stringr","tidyverse","ggplot2","ggalluvial")
lapply(packageList,require,character.only=TRUE)
rm(list=ls())

long_file     <- "01_data/processed/harmonization/longDF.csv"

longDF <- read.csv(long_file)
colnames(longDF) <- gsub("\\.", "/", colnames(longDF))
longDF <- longDF %>%
  mutate(farmAspectsImproved = case_when(
    is.na(farmAspectsImproved) ~ 0,
    farmAspectsImproved == "none" ~ 0,
    TRUE ~ 1
  ))


procesos <- c(
  "coffeeProcessingDoneOnFarm/N",
  "coffeeProcessingDoneOnFarm/despulpado",
  "coffeeProcessingDoneOnFarm/fermentado",
  "coffeeProcessingDoneOnFarm/lavado",
  "coffeeProcessingDoneOnFarm/secado"
)

df_estados <- longDF %>%
  rowwise() %>%
  mutate(
    estado = paste(
      c(
        if(`coffeeProcessingDoneOnFarm/N` == 1) "N",
        if(`coffeeProcessingDoneOnFarm/despulpado` == 1) "D",
        if(`coffeeProcessingDoneOnFarm/fermentado` == 1) "F",
        if(`coffeeProcessingDoneOnFarm/lavado` == 1) "L",
        if(`coffeeProcessingDoneOnFarm/secado` == 1) "S"
      ),
      collapse = " + "
    )
  ) %>%
  ungroup()
df_wide <- df_estados %>%
  select(surveyID, time, estado) %>%
  pivot_wider(
    names_from = time,
    values_from = estado,
    names_prefix = "t"
  )
flows <- df_wide %>%
  count(t0, t1, name = "Freq") %>%
  filter(t1!="")


win.graph()
p <- ggplot(
  flows, aes(axis1 = t0, axis2 = t1, y = Freq)) +
  geom_alluvium(aes(fill = t0)) +
  geom_stratum(width = .3) +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Time 1", "Time 0")) +
  labs(caption = "N = Ninguno, D = Despulpado, F = Fermentado, L = Lavado, S = Secado") +
  theme_minimal() #+
  #coord_flip()
ggsave(
  filename = "04_plots/harmonization/coffeeProcessingDoneOnFarm_alluvial.png", plot = p)
