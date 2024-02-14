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

# 01 - Loading Results ----------------------------------------

studied_experiment_list <- c("AF1", "AF2", "AF3", "AF4", "AF5", "AF6")

## 01.02 - Inputs Runs ------------------------------------------

load(glue("{path_to_simulation}/input_runs.RData"))

## 01.01 - Experimental Field Results ------------------------------------------

experimental_results <- read.csv2(path_to_experimental_results) %>%
  filter(
    crop == "winterwheat",
    grp_dist == "D",
    field %in% studied_experiment_list
  )

experimental_results_mean <- experimental_results %>% 
  select(field, year, tonDM) %>% 
  group_by(field, year) %>% 
  mutate(mean_tonDM = mean(tonDM)) %>% 
  ungroup() %>% 
  select(-tonDM) %>% 
  unique()

experimental_results_legend <- read.csv2(path_to_experimental_results_legend)

## 01.02 - Reference Datasets ----------------------------------------------------

### 01.02.01 - Reference Yield Dataset ----------------------------------------------------

reference_yield <- read.csv2(file = path_to_references_yield) %>%
  filter(
    COUNTRY == "BE",
    CROP_NAME == "soft_wheat",
    VARIABLE == "yield"
  ) %>%
  select(YEAR, VALUE) %>%
  group_by(YEAR) %>%
  mutate(YEARLY_MEAN_YIELD = mean(VALUE, na.rm = T)) %>%
  select(-VALUE) %>%
  unique()

## 01.03 - Simulation Results ------------------------------------------

load(file = glue("{path_to_simulation}/save_hop_files.RData"))

list_result <- ls()[grepl(pattern = "hop_", x = ls()) == T]

### 01.03.01 - Water Budget Results ------------------------------------------

water_budget_daily <- data.frame()
water_budget_monthly <- data.frame()
water_budget_yearly <- data.frame()

for (i in 1:length(list_result)) {
  temp <- hisafer::hisafe_budget(hop = get(glue("{list_result[i]}")), cycle = "water", freq = "day") %>%
    mutate(
      Site = substr(SimulationName, 1, 3),
      Year = year(Date),
      .after = Date)
  
  water_budget_daily <- rbind(water_budget_daily, temp)
  print(glue("{i} budget loaded on {length(list_result)}"))
}

for (i in 1:length(list_result)) {
  temp <- hisafer::hisafe_budget(hop = get(glue("{list_result[i]}")), cycle = "water", freq = "month") %>%
    mutate(
      Site = substr(SimulationName, 1, 3),
      Year = year(Date),
      .after = Date)
  
  water_budget_monthly <- rbind(water_budget_monthly, temp)
  print(glue("{i} budget loaded on {length(list_result)}"))
}

# for (i in 1:length(list_result)) {
#   temp <- hisafer::hisafe_budget(hop = get(glue("{list_result[i]}")), cycle = "water", freq = "year") %>%
#     mutate(
#       Site = substr(SimulationName, 1, 3),
#       Year = year(Date),
#       .after = Date)
# 
#   water_budget_yearly <- rbind(water_budget_yearly, temp)
#   print(glue("{i} budget loaded on {length(list_result)}"))
# }

rm(temp)

save(water_budget_yearly, file = glue("{path_to_simulation}/water_budget_yearly.RData"))

# load(file = glue("{path_to_simulation}/water_budget_yearly.RData"))

### 01.03.02 - Plot Crop Yield Results ------------------------------------------

result_plot <- data.frame()

for (i in 1:length(list_result)) {
  temp <- get(list_result[[i]])[["plot"]]
  result_plot <- rbind(result_plot, temp)
  print(glue("{i} plot result loaded on {length(list_result)}"))
}

rm(temp)

result_plot <- result_plot %>%
  mutate(mainCropPhenologicStageVegetative = ifelse(mainCropPhenologicStageVegetative == 0 & JulianDay >= 297, 1, mainCropPhenologicStageVegetative)) %>%
  mutate(YearCampagne = ifelse(mainCropPhenologicStageVegetative != 0, Year, NA)) %>% 
  mutate(YearCampagne = ifelse(!is.na(YearCampagne) & Month %in% c(9:12), YearCampagne + 1,YearCampagne)) %>% 
  mutate(CampagneDay = ifelse(mainCropPhenologicStageVegetative != 0, 1 , NA)) %>% 
  group_by(SimulationName, YearCampagne) %>% 
  mutate(CampagneDay = ifelse(!is.na(CampagneDay), na.omit(cumsum(CampagneDay)), CampagneDay)) %>% 
  ungroup() %>% 
  mutate(
    Site = substr(SimulationName, 1, 3),
    Factor = substr(SimulationName, 5, 6),
    .after = SimulationName)

result_plot_yield <- result_plot %>% 
  filter(mainCropPhenologicStageVegetative == 10) %>% 
  select(Site, SimulationName, Factor, Date, Day, Month, Year, JulianDay, YearCampagne, CampagneDay, mainCropArea, mainCropMeanYield, mainCropMeanBiomass, mainCropGrainNumber, mainCropGrainNumber, mainCropMeanHeight, mainCropPlantDensity) %>% 
  unique()

### 01.03.03 - Plot Crop Pheno Stage Results ------------------------------------------

results_plot_pheno_stage <- result_plot %>% 
  select(Site, SimulationName, Factor, Date, Day, Month, Year, JulianDay, YearCampagne, CampagneDay, mainCropPhenologicStageVegetative, mainCropPhenologicStageReproductive) %>% 
  group_by(SimulationName, YearCampagne, mainCropPhenologicStageVegetative, mainCropPhenologicStageReproductive) %>% 
  slice(1) %>% 
  ungroup() %>% 
  filter(mainCropPhenologicStageReproductive != 0)

# 02 - Save Results for Graphs or Excel ------------------------------------------

rm(i)

save.image(file = glue("{path_to_simulation}/results.RData"))





