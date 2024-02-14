library(tidyverse)
library(glue)

read_Agri4cast_location <- function(file, LAT = 51, LON = 3.1, START, STOP, NB_STATION = 1) {
  data <- readr::read_delim(file = file, delim = ";", col_names = T, show_col_types = FALSE) %>%
    dplyr::mutate(DATE = DAY %>% as.character() %>% lubridate::as_date(format = "%Y%m%d"))
  Grid_JRC <- data %>%
    dplyr::select(GRID_NO, LATITUDE, LONGITUDE) %>%
    dplyr::distinct() %>%
    dplyr::mutate(PROXIMITY = base::rank((LONGITUDE - LON)^2 + (LATITUDE - LAT)^2))
  list <- vector("list", NB_STATION)
  for (i in 1:NB_STATION) {
    list[[i]] <- data %>%
      dplyr::filter(GRID_NO == Grid_JRC[Grid_JRC$PROXIMITY == i, ]$GRID_NO) %>%
      dplyr::filter(between(DATE, lubridate::as_date(START), lubridate::as_date(STOP)))
  }
  # Function to retrieve meteo data closest available from the raw dataset
  # Initial function created by Tom de Swaef
  return(list)
}

data_meteo_Agri4cast_raw <- read_Agri4cast_location(
  file = path_to_meteo_Agri4cast_full_dataset,
  LAT = experimental_site_properties[experimental_site_properties$experiment == studied_experiment, ]$latitude,
  LON = experimental_site_properties[experimental_site_properties$experiment == studied_experiment, ]$longitude,
  START = "1994-01-01", STOP = "2022-12-31",
  NB_STATION = 3
)

data_meteo_Agri4cast <- data_meteo_Agri4cast_raw

for (i in 1:length(data_meteo_Agri4cast_raw)) {
  data_meteo_Agri4cast[[i]] <- data_meteo_Agri4cast_raw[[i]] %>%
    dplyr::select(-DAY) %>%
    dplyr::mutate(DAY = lubridate::day(DATE), MONTH = lubridate::month(DATE), YEAR = lubridate::year(DATE), .after = DATE) %>%
    dplyr::group_by(YEAR) %>%
    dplyr::mutate(DOY = as.POSIXlt(DATE)$yday + 1, year_precip = sum(PRECIPITATION)) %>% # Calcul Julian Day
    dplyr::ungroup() %>%
    dplyr::mutate(WT = rep(data_watertable_default[, 1], length.out = nrow(data_meteo_Agri4cast_raw[[i]]))) %>% # Adding Watertable Depth based on sample dataset provided by Hi-sAFe modeller
    dplyr::mutate(AIR_CO2 = 330) %>%
    dplyr::mutate(RADIATION = RADIATION / 1000) %>% # Conversion kJ in MJ
    dplyr::mutate(
      ES_MAX = 6.112 * exp((17.67 * TEMPERATURE_MAX) / (TEMPERATURE_MAX + 243.5)),
      ES_MIN = 6.112 * exp((17.67 * TEMPERATURE_MIN) / (TEMPERATURE_MIN + 243.5))
    ) %>% # Computation of ES
    dplyr::mutate(
      RH_MIN = (VAPOURPRESSURE / ES_MAX) * 100,
      RH_MAX = (VAPOURPRESSURE / ES_MIN) * 100
    ) %>% # Computation of Relative Humidity
    dplyr::mutate(
      RH_MAX = ifelse(RH_MAX >= 100, 100, round(RH_MAX, digits = 2)),
      RH_MIN = round(RH_MIN, digits = 2)
    ) # Ronding of Relative Humidity percentage
  # Determination of Relative Humidity by Clausius-Clapeyron Equation

  data_meteo_Agri4cast[[i]] <- data_meteo_Agri4cast[[i]] %>%
    select(DOY, YEAR, MONTH, DAY, TEMPERATURE_MAX, TEMPERATURE_MIN, RH_MAX, RH_MIN, RADIATION, PRECIPITATION, WINDSPEED, WT, AIR_CO2)
}

for (j in 1:length(data_meteo_Agri4cast)) {
  assign(
    x = glue::glue("data_meteo_Agri4cast_station_{j}", .null = character),
    value = data_meteo_Agri4cast[[i]]
  )

  write.table(
    x = get(glue::glue("data_meteo_Agri4cast_station_{j}", .null = character)),
    file = glue::glue("{rstudioapi::getActiveProject()}/meteo/output_data_meteo_Agri4cast_station_{j}.wth"), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE
  )
}

rm(list = c("data_meteo_Agri4cast_raw", "data_meteo_Agri4cast"))

print("Agri4cast dataset fully loaded")
