library(tidyverse)
library(glue)

read_SoilGrid250m_location <- function(path, LAT = 51, LON = 3.1, soil_type = "Luvisol", NB_STATION = 1) {
  soil_type <- paste(soil_type, "s", sep = "")

  data <- readr::read_csv2(file = glue::glue("{path}/data_SoilGrid250m_2.csv", delim = ".", .null = character), col_names = T, show_col_types = FALSE, progress = F)
  data <- data[, -1]

  Grid_SoilGrid250m <- data %>%
    dplyr::filter(Soil_Classif == soil_type) %>%
    dplyr::filter(rowSums(select(., all_of(c("Clay_content", "Sand", "Silt")))) > 0) %>%
    dplyr::select(GRID_NO, y, x) %>%
    dplyr::rename(
      LATITUDE = "y",
      LONGITUDE = "x"
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(PROXIMITY = base::rank((LONGITUDE - LON)^2 + (LATITUDE - LAT)^2))

  list <- vector("list", NB_STATION)

  for (i in 1:NB_STATION) {
    list[[i]] <- data %>%
      dplyr::filter(GRID_NO == Grid_SoilGrid250m[Grid_SoilGrid250m$PROXIMITY == i, ]$GRID_NO)
  }

  return(list)
  # function to retrieve soil data closest available from the raw dataset
}

data_soil_SoilGrid250m_raw <- read_SoilGrid250m_location(
  path = path_to_project,
  LAT = experimental_site_properties[experimental_site_properties$experiment == studied_experiment, ]$latitude,
  LON = experimental_site_properties[experimental_site_properties$experiment == studied_experiment, ]$longitude,
  soil_type = experimental_site_properties[experimental_site_properties$experiment == studied_experiment, ]$soil_type,
  NB_STATION = 3
  # soil_type = "Cambisols"
)

data_soil_SoilGrid250m <- data_soil_SoilGrid250m_raw
data_soil_SoilGrid250m_physical_properties <- data_soil_SoilGrid250m

for (i in 1:length(data_soil_SoilGrid250m_raw)) {
  data_soil_SoilGrid250m[[i]] <- data_soil_SoilGrid250m_raw[[i]] %>%
    dplyr::mutate(thick = (Prof_Bottom_Layer - Prof_Top_Layer) / 100) %>%
    dplyr::mutate(
      partSizeSand = 290,
      stone = 0,
      stoneType = 6,
      infiltrability = 50
    ) %>%
    dplyr::mutate(
      waterContent = Vol_water_content_at_33_kPa / 1000,
      no3Concentration = Nitrogen / 100,
      nh4concentration = 0
    ) %>%
    dplyr::slice(1:4)
  # dplyr::slice(2:5)

  data_soil_SoilGrid250m_physical_properties[[i]] <- data_soil_SoilGrid250m[[i]] %>%
    dplyr::select(Layer, thick, Sand, Clay_content, Silt, Organic_Carbon_Density, partSizeSand, stone, stoneType, infiltrability) %>%
    dplyr::mutate(
      Layer = "Layer",
      Sand = Sand / 10,
      Clay_content = Clay_content / 10,
      Silt = Silt / 10,
      Organic_Carbon_Density = Organic_Carbon_Density / 100
    ) %>%
    dplyr::rename(
      name = "Layer",
      sand = "Sand",
      clay = "Clay_content",
      limeStone = "Silt",
      organicMatter = "Organic_Carbon_Density"
    )

  # data_soil_SoilGrid250m_chemical_properties <- data_soil_SoilGrid250m %>%
  #   dplyr::select(Layer, waterContent, no3Concentration, nh4concentration) %>%
  #   dplyr::mutate(Layer = "LayerInit")
}

for (i in 1:length(data_soil_SoilGrid250m_physical_properties)) {
  assign(
    x = glue::glue("data_soil_SoilGrid250m_physical_properties_{i}", .null = character),
    value = data_soil_SoilGrid250m_physical_properties[[i]]
  )
}

rm(list = c("data_soil_SoilGrid250m", "data_soil_SoilGrid250m_raw", "data_soil_SoilGrid250m_physical_properties"))

print("SoilGrid250m dataset fully loaded")
