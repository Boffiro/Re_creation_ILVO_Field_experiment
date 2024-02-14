library(tidyverse)
library(glue)

read_WOSIS_location <- function(path, LAT = 51, LON = 3.1, soil_type = "Luvisol", NB_STATION = 1) {
  data <- readr::read_delim(file = glue::glue("{path}/wosis_latest_profiles.csv", .null = character), delim = ",", col_names = T, show_col_types = FALSE)

  data_clay <- readr::read_delim(file = glue::glue("{path}/wosis_latest_clay.csv", .null = character), delim = ",", col_names = T, show_col_types = FALSE)
  data_sand <- readr::read_delim(file = glue::glue("{path}/wosis_latest_sand.csv", .null = character), delim = ",", col_names = T, show_col_types = FALSE)
  data_silt <- readr::read_delim(file = glue::glue("{path}/wosis_latest_silt.csv", .null = character), delim = ",", col_names = T, show_col_types = FALSE)

  data_orgc <- readr::read_delim(file = glue::glue("{path}/wosis_latest_orgc.csv", .null = character), delim = ",", col_names = T, show_col_types = FALSE)

  Grid_WOSIS <- data %>%
    dplyr::filter(is.na(cfao_version) == F, cfao_major_group == soil_type) %>%
    dplyr::filter(profile_id %in% data_clay[data_clay$clay_value_avg != 0, ]$profile_id & profile_id %in% data_orgc[data_orgc$orgc_value_avg != 0, ]$profile_id) %>%
    # dplyr::filter(profile_id %in% data_clay$profile_id & profile_id %in% data_orgc$profile_id) %>%
    dplyr::select(profile_id, Y, X) %>%
    dplyr::rename(
      GRID_NO = "profile_id",
      LATITUDE = "Y",
      LONGITUDE = "X"
    ) %>%
    dplyr::mutate(
      dist = (LONGITUDE - LON)^2 + (LATITUDE - LAT)^2
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(PROXIMITY = base::rank((LONGITUDE - LON)^2 + (LATITUDE - LAT)^2)) %>%
    dplyr::filter(PROXIMITY %in% c(1:NB_STATION))

  list <- vector("list", NB_STATION)

  for (i in 1:NB_STATION) {
    Px_ID <- Grid_WOSIS %>%
      dplyr::filter(GRID_NO == Grid_WOSIS[Grid_WOSIS$PROXIMITY == i, ]$GRID_NO)

    temp <- data %>%
      dplyr::filter(profile_id == Px_ID$GRID_NO) %>%
      dplyr::select(profile_id, country_name, latitude, longitude, cfao_major_group, cfao_soil_unit)

    temp_clay <- data_clay %>%
      dplyr::filter(profile_id == temp$profile_id) %>%
      dplyr::select(profile_id, profile_layer_id, upper_depth, lower_depth, clay_value_avg)

    temp_sand <- data_sand %>%
      dplyr::filter(profile_id == temp$profile_id) %>%
      dplyr::select(profile_id, profile_layer_id, upper_depth, lower_depth, sand_value_avg)

    temp_silt <- data_silt %>%
      dplyr::filter(profile_id == temp$profile_id) %>%
      dplyr::select(profile_id, profile_layer_id, upper_depth, lower_depth, silt_value_avg)

    temp_orgc <- data_orgc %>%
      dplyr::filter(profile_id == temp$profile_id) %>%
      dplyr::select(profile_id, profile_layer_id, upper_depth, lower_depth, orgc_value_avg)

    temp <- merge.data.frame(x = temp, y = temp_clay, sort = F)
    temp <- merge.data.frame(x = temp, y = temp_sand, sort = F)
    temp <- merge.data.frame(x = temp, y = temp_silt, sort = F)
    temp <- merge.data.frame(x = temp, y = temp_orgc, sort = F)

    list[[i]] <- temp
  }

  return(list)
  # function to retrieve soil data closest available from the raw dataset
}

data_soil_WOSIS_raw <- read_WOSIS_location(
  path = path_to_soil_WOSIS_folder,
  LAT = experimental_site_properties[experimental_site_properties$experiment == studied_experiment, ]$latitude,
  LON = experimental_site_properties[experimental_site_properties$experiment == studied_experiment, ]$longitude,
  soil_type = experimental_site_properties[experimental_site_properties$experiment == studied_experiment, ]$soil_type,
  # soil_type = "Cambisol"
  NB_STATION = 3
)

for (i in 1:length(data_soil_WOSIS_raw)) {
  if (nrow(data_soil_WOSIS_raw[[i]]) < 4) {
    while (nrow(data_soil_WOSIS_raw[[i]]) < 4) {
      data_soil_WOSIS_raw[[i]][nrow(data_soil_WOSIS_raw[[i]]) + 1, ] <- data_soil_WOSIS_raw[[i]][nrow(data_soil_WOSIS_raw[[i]]), ]
    }
  }

  if (nrow(data_soil_WOSIS_raw[[i]]) > 4) {
    data_soil_WOSIS_raw[[i]] <- data_soil_WOSIS_raw[[i]] %>%
      dplyr::slice(1:4)
  }
}

data_soil_WOSIS <- data_soil_WOSIS_raw
data_soil_WOSIS_physical_properties <- data_soil_WOSIS

for (i in 1:length(data_soil_WOSIS_raw)) {
  data_soil_WOSIS[[i]] <- data_soil_WOSIS_raw[[i]] %>%
    dplyr::mutate(
      name = "Layer",
      thick = (lower_depth - upper_depth) / 100
    ) %>%
    dplyr::mutate(
      partSizeSand = 290,
      stone = 0,
      stoneType = 6,
      infiltrability = 50
    )

  data_soil_WOSIS_physical_properties[[i]] <- data_soil_WOSIS[[i]] %>%
    dplyr::select(name, thick, sand_value_avg, clay_value_avg, silt_value_avg, orgc_value_avg, partSizeSand, stone, stoneType, infiltrability) %>%
    dplyr::rename(
      sand = "sand_value_avg",
      clay = "clay_value_avg",
      limeStone = "silt_value_avg",
      organicMatter = "orgc_value_avg"
    )
}

for (i in 1:length(data_soil_WOSIS_physical_properties)) {
  assign(
    x = glue::glue("data_soil_WOSIS_physical_properties_{i}", .null = character),
    value = data_soil_WOSIS_physical_properties[[i]]
  )
}

rm(list = c("data_soil_WOSIS_raw", "data_soil_WOSIS", "data_soil_WOSIS_physical_properties", "i"))

print("WOSIS dataset fully loaded")
