library(tidyverse)
library(hisafer)
library(beepr)
library(rstudioapi)
library(glue)
library(ggforce)
library(viridis)

# 00 - Setup ----------------------------------------
## 00.01 - Creating Paths --------------------------------------------------

path_to_project <- rstudioapi::getActiveProject()

path_to_experimental_site_properties <- glue("{path_to_project}/ressources/site_properties.csv", .null = character)

path_to_experimental_results <- glue("{path_to_project}/data_crop_yield.csv", .null = character)
path_to_experimental_results_legend <- glue("{path_to_project}/data_crop_yield_legend.csv", .null = character)

path_to_references_yield <- glue("{path_to_project}/ressources/sub_nat_crop_stat.csv", .null = character)

path_to_meteo_control <- glue("{path_to_project}/meteo/", .null = character)

path_to_meteo_Clipick_full_dataset <- glue("{rstudioapi::getActiveProject()}/meteo/", .null = character)
# path_to_meteo_Clipick_full_dataset <- glue("{rstudioapi::getActiveProject()}/meteo/output_data_meteo_Clipick.csv", .null = character)

path_to_meteo_Agri4cast_full_dataset <- glue("{path_to_project}/meteo/Agri4cast/Agri4Cast_Belgium.csv", .null = character)

path_to_soil_WOSIS_folder <- "C:/Users/fsoulard/Proton Drive/florian.soulard/My files/Thèse/Dataset/soil/WOSIS/cleaned"
path_to_soil_WOSIS_profiles <- glue("{path_to_soil_WOSIS_folder}/wosis_latest_profiles.csv", .null = character)

path_to_soil_SoilGrid250m_profiles <- glue("{path_to_project}/data_SoilGrid250m.csv", .null = character)

path_to_simulation <- glue("{path_to_project}/simulation", .null = character)

## 00.02 - Creating Functions --------------------------------------------------

source("script_loading_function.R")

## 00.03 - Experimental Datasets (from ILVO) ------------------------------------------
### 00.03.01 - Experimental Site Properties ------------------------------------------

experimental_site_properties <- read.csv2(file = path_to_experimental_site_properties)


## 00.04 - Utility Datasets ------------------------------------------

data_watertable_default <- read.csv2(glue("{path_to_project}/watertable_default.txt", .null = character), header = F)

## 00.05 - Choice of Experiment to Model  --------------------------------------------------

studied_experiment_list <- c("AF1", "AF2", "AF3", "AF4", "AF5", "AF6")

for (a in 1:length(studied_experiment_list)) {
  studied_experiment <- studied_experiment_list[a]

  # 01 - Loading & Cleaning - Formating of Datasets -------------------------------------
  ## 01.01 - Meteorological Data ---------------------------------------------
  ### 01.01.01 - Agri4Cast Dataset --------------------------------------------

  source("script_load_Agri4cast_dataset.R")

  ### 01.01.02 - Clipick Dataset --------------------------------------------

  studied_site <- experimental_site_properties[experimental_site_properties$experiment == studied_experiment, ]$sites

  source("script_load_Clipick_dataset.R")

  ## 01.02 - Soil Data ------------------------------------------
  ### 01.02.01 - WOSIS Dataset ----------------------------------------------------

  source("script_load_WOSIS_dataset.R")

  ### 01.02.02 - SoilGrid250m Dataset ----------------------------------------------------

  source("script_load_SoilGrid250m_dataset.R")

  # 02 - Creation of the runs -------------------------------------

  ## 02.01 - Studied factor choice  -------------------------------------

  # factor_agricultural_system = c("monocrop", "agroforestry", "forestry")
  factor_agricultural_system <- ("monocrop")
  factor_agricultural_systemd_design <- ("default")
  factor_year_start <- c(2007)
  factor_year_end <- c(2022)
  factor_tree_species <- c("poplar")
  factor_tree_start_age <- c(20)
  factor_tree_intervention <- c("default")
  factor_crop_species <- c("wheat")
  
  # factor_crop_ITK <- c("default")
  factor_crop_ITK <- c("wheat_no_irrig")

  # factor_soil <- c("data_soil_SoilGrid250m", "data_soil_WOSIS")
  factor_soil <- ls()[grepl(pattern = "data_soil", x = ls())]
  
  # factor_climate <- c("data_meteo_Clipick", "data_meteo_Agri4cast_station_1")
  factor_climate <- ls()[grepl(pattern = "data_meteo", x = ls())]

  ## 02.02 - Creation of runs factors df -------------------------------------

  factor_list <- list(
    factor_agricultural_system = factor_agricultural_system,
    factor_agricultural_systemd_design = factor_agricultural_systemd_design,
    factor_year_start = factor_year_start,
    factor_year_end = factor_year_end,
    factor_tree_species = factor_tree_species,
    factor_crop_species = factor_crop_species,
    factor_crop_ITK = factor_crop_ITK,
    factor_soil = factor_soil,
    factor_climate = factor_climate
  )

  input_runs <- expand.grid(factor_list, stringsAsFactors = FALSE)

  RunID <- data.frame(matrix(nrow = nrow(input_runs), ncol = 1))

  for (i in 1:length(row_number(input_runs))) {
    RunID[i, ] <- paste(input_runs[i, ], collapse = "_")
    RunID[i, ] <- gsub("Default_", "", RunID[i, ])
  }

  input_runs <- input_runs %>%
    mutate(
      run_ID = RunID[, 1],
      run_number = row_number()
    ) %>%
    select(run_ID, run_number, everything())

  rm(RunID)

  ## 02.03 - Creation run config files -------------------------------------

  experimentation_name <- "ILVO_Agroforestry"

  # for (k in 1:nrow(input_runs)) {
  for (k in 4:4) {
    simulation_name <- glue("{studied_experiment}_{input_runs$run_number[k]}", .null = character)

    assign(
      x = glue("hip_{input_runs$run_number[k]}"),
      value = hisafer::define_hisafe(
        path = path_to_simulation,
        template = glue("{path_to_project}/hi-safe_ressources/Hi-sAFe_template/{input_runs$factor_agricultural_system[k]}", .null = character),
        # template = glue("{input_runs$factor_agricultural_system[i]}", .null = character),
        exp.name = experimentation_name,
        SimulationName = simulation_name,
        simulationYearStart = input_runs$factor_year_start[k],
        simulationDayStart = 250,
        latitude = experimental_site_properties[experimental_site_properties$experiment == studied_experiment, ]$latitude,
        mainCropSpecies = glue("{input_runs$factor_crop_species[k]}.plt"),
        mainCropItk = glue("{input_runs$factor_crop_species[k]}.tec"),
        weatherFile = glue("{rstudioapi::getActiveProject()}/meteo/output_{input_runs$factor_climate[k]}.wth"),
        sticsReport = 1
      )
    )

    hisafer::build_hisafe(
      hip = get(glue("hip_{input_runs$run_number[k]}")),
      files = "all",
      # plot.scene = TRUE,
      summary.files = TRUE,
      stics.diagnostics = FALSE
    )

    modification_north_orientation(
      hip = get(glue("hip_{input_runs$run_number[k]}")),
      orientation = experimental_site_properties$tree_rows_orientation_given[experimental_site_properties$experiment == studied_experiment]
    )

    modification_nb_years_simulation(
      hip = get(glue("hip_{input_runs$run_number[k]}")),
      duration = (input_runs$factor_year_end[k] - input_runs$factor_year_start[k])
    )

    modification_soil_physical_parameters(
      hip = get(glue("hip_{input_runs$run_number[k]}")),
      df_physical_properties = get(glue("{input_runs$factor_soil[i]}"))
    )

    hisafer::run_hisafe(
      hip = get(glue("hip_{input_runs$run_number[k]}")),
      capsis.path = "C:/Users/fsoulard/Documents/CAPSIS/"
    )

    assign(
      x = glue("hop_{studied_experiment}_{k}"),
      value = hisafer::read_hisafe(hip = get(glue("hip_{k}")))
    )
  }

  print(glue("Simulation de AF{a} finie"))

  save(input_runs, file = glue("{path_to_simulation}/input_runs_{studied_experiment_list[a]}.RData"))
}

list_result <- ls()[grepl(pattern = "hop_", x = ls()) == T]

save(list = list_result, file = glue("{path_to_simulation}/save_hop_files.RData"))

# -------------------------------------------------------------------------

load(file = glue("{path_to_simulation}/input_runs_AF1.RData"))
input_runs_AF1 <- input_runs
load(file = glue("{path_to_simulation}/input_runs_AF2.RData"))
input_runs_AF2 <- input_runs
load(file = glue("{path_to_simulation}/input_runs_AF3.RData"))
input_runs_AF3 <- input_runs
load(file = glue("{path_to_simulation}/input_runs_AF4.RData"))
input_runs_AF4 <- input_runs
load(file = glue("{path_to_simulation}/input_runs_AF5.RData"))
input_runs_AF5 <- input_runs
load(file = glue("{path_to_simulation}/input_runs_AF6.RData"))
input_runs_AF6 <- input_runs

temp <- data.frame()
input_runs <- data.frame()

for (i in 1:6) {

  temp <- get(glue("input_runs_AF{i}"))
  
  input_runs <- rbind(input_runs, temp)
  
  rm(list = glue("input_runs_AF{i}"))
  
}

rm(temp)

input_runs <- input_runs %>%
  rename(run_factor_number = "run_number") %>% 
  mutate(run_number = row_number(run_ID), .before = "run_factor_number") %>% 
  select(run_ID, run_number, run_factor_number, factor_soil, factor_climate) %>% 
  # mutate(factor_soil = grepl(x = factor_soil, pattern = "data_soil_"))
  mutate(factor_soil = gsub(x = factor_soil, pattern = "data_soil_", replacement = ""),
         factor_climate = gsub(x = factor_climate, pattern = "data_meteo_", replacement = "")) %>% 
  mutate(factor_soil = gsub(x = factor_soil, pattern = "physical_properties_", replacement = "")) %>% 
  mutate(factor_combination = glue("{factor_soil}_{factor_climate}"))

save(input_runs, file = glue("{path_to_simulation}/input_runs.RData"))


