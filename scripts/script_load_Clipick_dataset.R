library(tidyverse)
library(glue)

data_meteo_Clipick_raw <- read.csv2(glue::glue("{path_to_meteo_Clipick_full_dataset}/{studied_site}.csv", .null = character), sep = ",", dec = ".", header = F)

data_meteo_Clipick <- data_meteo_Clipick_raw[-1, ]
colnames(data_meteo_Clipick) <- data_meteo_Clipick[1, ]
data_meteo_Clipick <- data_meteo_Clipick[-1, ]

data_meteo_Clipick <- data_meteo_Clipick %>%
  dplyr::mutate(DATE = lubridate::date(glue("{Year}-{Month}-{Day}"))) %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(DOY = as.POSIXlt(DATE)$yday + 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    WT = rep(data_watertable_default[, 1], length.out = nrow(data_meteo_Clipick)),
    AIR_CO2 = 330
  )

data_meteo_Clipick <- data_meteo_Clipick %>%
  dplyr::select(DOY, Year, Month, Day, tasmax, tasmin, hursmax, hursmin, rss, pr, wss, WT, AIR_CO2)

write.table(data_meteo_Clipick, file = glue("{rstudioapi::getActiveProject()}/meteo/output_data_meteo_Clipick.wth", .null = character), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

rm(data_meteo_Clipick_raw)

print("Clipick dataset fully loaded")
