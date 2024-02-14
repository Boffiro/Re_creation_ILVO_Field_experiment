library(tidyverse)
library(hisafer)
library(beepr)
library(rstudioapi)
library(glue)
library(ggforce)
library(ggpattern)
library(viridis)

# 01 - Load Results

# source("script_result_treatment.R", echo = F)

load(file = "C:/Users/fsoulard/Documents/GitHub/Re_creation_ILVO_Field_experiment/simulation/results.RData")

# 02 - Creating Graphs ----------------------------------------

## 02.01 - Graphs Yield ----------------------------------------

### 02.01.01 - Graphs Yield Basics ----------------------------------------

#### Measured Yield ----------------------------------------

graph_measured_yield <- ggplot() +
  geom_boxplot(data = experimental_results, aes(x = year, y = tonDM, group = interaction(year, field), fill = field)) + # Measured Yield
  geom_point(data = experimental_results_mean, aes(x = year, y = mean_tonDM, group = interaction(year, field)), position = position_dodge(width = 0.75)) +
  theme_light() +
  scale_fill_manual(values = c("AF1" = "blue", "AF2" = "green", "AF4" = "red", "AF6" = "cyan")) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_x_continuous(limits = c(2012, 2023), breaks = seq(2012, 2023, by = 1), labels = seq(2012, 2023, by = 1)) +
  scale_y_continuous(limits = c(0, 11), breaks = seq(0, 11, by = 1), labels = seq(0, 11, by = 1)) +
  labs(
    title = "Measured Yield per year"
  ) +
  xlab("Year") +
  ylab("Crop Yield (in t/ha)") +
  guides(alpha = "none", fill = guide_legend(title = "Experiment"))

plot(graph_measured_yield)

#### National Reference Yield ----------------------------------------

graph_reference_yield <- ggplot(data = reference_yield) +
  geom_line(aes(x = YEAR, y = YEARLY_MEAN_YIELD, col = "Reference Yield"), colour = "black") +
  geom_point(aes(x = YEAR, y = YEARLY_MEAN_YIELD, col = "Reference Yield"), colour = "black") +
  theme_light() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_x_continuous(limits = c(2012, 2023), breaks = seq(2012, 2023, by = 1), labels = seq(2012, 2023, by = 1)) +
  scale_y_continuous(limits = c(0, 11), breaks = seq(0, 11, by = 1), labels = seq(0, 11, by = 1)) +
  labs(title = "Reference Yield per year") +
  xlab("Year") +
  ylab("Crop Yield (in t/ha)") +
  guides(alpha = "none", col = guide_legend(title = "Experiment"))

plot(graph_reference_yield)

#### Simulated Yield per Year ----------------------------------------

graph_yield_per_year <- ggplot(data = result_plot_yield %>% filter(interaction(result_plot_yield$Year, result_plot_yield$Site) %in% interaction(experimental_results$year, experimental_results$field) == T), aes(x = Year)) +
  geom_line(data = reference_yield, aes(x = YEAR, y = YEARLY_MEAN_YIELD)) + # Reference Yield / Year
  geom_point(data = reference_yield, aes(x = YEAR, y = YEARLY_MEAN_YIELD)) + # Reference Yield / Year
  geom_boxplot(data = experimental_results, aes(x = year, y = tonDM, group = interaction(year, field), fill = field), position = position_dodge(preserve = "single")) + # Measured Yield
  geom_boxplot_pattern(aes(y = mainCropMeanYield, group = interaction(Year, Site), fill = Site),
    position = position_dodge(preserve = "single"),
    color = "black", pattern = "stripe", pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, pattern_spacing = 0.0125, pattern_key_scale_factor = 0.6
  ) +
  theme_light() +
  scale_fill_manual(values = c("AF1" = "blue", "AF2" = "green", "AF4" = "red", "AF6" = "cyan")) +
  scale_pattern_manual(values = c(Simulated = "stripe", Measured = "none")) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) +
  scale_x_continuous(limits = c(2012, 2023), breaks = seq(2012, 2023, by = 1), labels = seq(2012, 2023, by = 1)) +
  scale_y_continuous(limits = c(0, 11), breaks = seq(0, 11, by = 1), labels = seq(0, 11, by = 1)) +
  labs(
    title = "Yield per year",
    subtitle = "Line is Reference Yield. Plain Boxplot is Measured Yield. Hatched Boxplot is Simulated Yield",
    x = "Year", y = "Crop Yield (in t/ha)", pattern = "Source"
  ) +
  guides(fill = guide_legend(title = "Experiment", override.aes = list(pattern = "none")))

plot(graph_yield_per_year)

#### Simulated Yield per Site ----------------------------------------

graph_yield_per_site <- ggplot(data = result_plot_yield %>% filter(interaction(result_plot_yield$Year, result_plot_yield$Site) %in% interaction(experimental_results$year, experimental_results$field) == T), aes(x = Site)) +
  geom_boxplot(data = experimental_results, aes(x = field, y = tonDM, group = interaction(year, field), fill = field), position = position_dodge(preserve = "single")) + # Measured Yield
  geom_boxplot_pattern(aes(y = mainCropMeanYield, group = interaction(Year, Site), fill = Site),
    position = position_dodge(preserve = "single"),
    color = "black", pattern = "stripe", pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, pattern_spacing = 0.0125, pattern_key_scale_factor = 0.6
  ) +
  theme_light() +
  scale_fill_manual(values = c("AF1" = "blue", "AF2" = "green", "AF4" = "red", "AF6" = "cyan")) +
  scale_pattern_manual(values = c(Simulated = "stripe", Measured = "none")) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) +
  # scale_x_continuous(limits = c(2012, 2023), breaks = seq(2012, 2023, by = 1), labels = seq(2012, 2023, by = 1)) +
  scale_y_continuous(limits = c(0, 11), breaks = seq(0, 11, by = 1), labels = seq(0, 11, by = 1)) +
  labs(
    title = "Yield per site",
    subtitle = "Plain Boxplot is Measured Yield. Hatched Boxplot is Simulated Yield",
    x = "Site", y = "Crop Yield (in t/ha)", pattern = "Source"
  ) +
  guides(fill = guide_legend(title = "Experiment", override.aes = list(pattern = "none")))

plot(graph_yield_per_site)

#### Simulated Yield per Factor ----------------------------------------

graph_yield_per_factor <- ggplot(data = result_plot_yield %>% filter(interaction(result_plot_yield$Year, result_plot_yield$Site) %in% interaction(experimental_results$year, experimental_results$field) == T), aes(x = Factor)) +
  geom_boxplot(aes(y = mainCropMeanYield, group = interaction(Factor, Site), fill = Site)) +
  theme_light() +
  scale_fill_manual(values = c("AF1" = "blue", "AF2" = "green", "AF4" = "red", "AF6" = "cyan")) +
  scale_pattern_manual(values = c(Simulated = "stripe", Measured = "none")) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) +
  # scale_x_continuous(limits = c(2012, 2023), breaks = seq(2012, 2023, by = 1), labels = seq(2012, 2023, by = 1)) +
  scale_x_discrete() +
  scale_y_continuous(limits = c(0, 11), breaks = seq(0, 11, by = 1), labels = seq(0, 11, by = 1)) +
  labs(
    title = "Yield per Factor",
    subtitle = "Line is Reference Yield. Plain Boxplot is Measured Yield. Hatched Boxplot is Simulated Yield",
    x = "Factor", y = "Crop Yield (in t/ha)", pattern = "Source"
  ) +
  guides(fill = guide_legend(title = "Experiment", override.aes = list(pattern = "none")))

plot(graph_yield_per_factor)

### 02.01.02 - Graphs Yield Filtered Factors ----------------------------------------

#### Simulated Yield per Year ----------------------------------------

graph_yield_per_year <- ggplot(data = result_plot_yield %>% filter(interaction(result_plot_yield$Year, result_plot_yield$Site) %in% interaction(experimental_results$year, experimental_results$field) == T & Factor == 4), aes(x = Year)) +
  geom_line(data = reference_yield, aes(x = YEAR, y = YEARLY_MEAN_YIELD)) + # Reference Yield / Year
  geom_point(data = reference_yield, aes(x = YEAR, y = YEARLY_MEAN_YIELD)) + # Reference Yield / Year
  geom_boxplot(data = experimental_results, aes(x = year, y = tonDM, group = interaction(year, field), fill = field), position = position_dodge(preserve = "single")) + # Measured Yield
  geom_point(aes(y = mainCropMeanYield, group = interaction(Year, Site), col = Site), position = position_dodge(width = 0.75)) + # Aligning geom_point with geom_boxplot
  theme_light() +
  scale_color_manual(values = c("AF1" = "blue", "AF2" = "green", "AF4" = "red", "AF6" = "cyan")) +
  scale_fill_manual(values = c("AF1" = "blue", "AF2" = "green", "AF4" = "red", "AF6" = "cyan")) +
  scale_pattern_manual(values = c(Simulated = "stripe", Measured = "none")) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) +
  scale_x_continuous(limits = c(2012, 2023), breaks = seq(2012, 2023, by = 1), labels = seq(2012, 2023, by = 1)) +
  scale_y_continuous(limits = c(0, 11), breaks = seq(0, 11, by = 1), labels = seq(0, 11, by = 1)) +
  labs(
    title = "Yield per year",
    subtitle = "Boxplot is Measured Yield. Point is Simulated Yield",
    x = "Year", y = "Crop Yield (in t/ha)", pattern = "Source"
  ) +
  guides(fill = guide_legend(title = "Experiment", override.aes = list(pattern = "none")))

plot(graph_yield_per_year)

#### Simulated Yield per Site ----------------------------------------

graph_yield_per_site <- ggplot(data = result_plot_yield %>% filter(interaction(result_plot_yield$Year, result_plot_yield$Site) %in% interaction(experimental_results$year, experimental_results$field) == T & Factor == 4), aes(x = Site)) +
  geom_boxplot(data = experimental_results, aes(x = field, y = tonDM, group = interaction(year, field), fill = field), position = position_dodge(0.75)) + # Measured Yield
  geom_point(aes(y = mainCropMeanYield, group = interaction(Year, Site), col = Site), position = position_dodge(0.75)) +
  theme_light() +
  scale_fill_manual(values = c("AF1" = "blue", "AF2" = "green", "AF4" = "red", "AF6" = "cyan")) +
  scale_color_manual(values = c("AF1" = "blue", "AF2" = "green", "AF4" = "red", "AF6" = "cyan")) +
  scale_pattern_manual(values = c(Simulated = "stripe", Measured = "none")) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) +
  scale_y_continuous(limits = c(0, 11), breaks = seq(0, 11, by = 1), labels = seq(0, 11, by = 1)) +
  labs(
    title = "Yield per site",
    subtitle = "Boxplot is Measured Yield. Point is Simulated Yield",
    x = "Site", y = "Crop Yield (in t/ha)", pattern = "Source"
  ) +
  guides(fill = guide_legend(title = "Experiment", override.aes = list(pattern = "none")))

plot(graph_yield_per_site)

### 02.01.02 - Graphs Difference Yield Simulated VS Yield Measured ----------------------------------------

data_graph_diff_yield <- result_plot_yield %>%
  filter(interaction(result_plot_yield$Year, result_plot_yield$Site) %in% interaction(experimental_results$year, experimental_results$field) == T & Factor == 4) %>%
  merge(experimental_results %>% select(field, year, tonDM) %>% rename(Site = "field", Year = "year")) %>%
  mutate(diff_yield = abs(tonDM - mainCropMeanYield)) %>%
  select(-mainCropMeanYield, -tonDM) %>%
  unique()

graph_diff_yield <- ggplot(data = data_graph_diff_yield %>% filter(interaction(data_graph_diff_yield$Year, data_graph_diff_yield$Site) %in% interaction(experimental_results$year, experimental_results$field) == T & Factor == 4), aes(x = Year)) +
  geom_boxplot(aes(y = diff_yield, group = interaction(Year, Site), fill = Site), position = position_dodge(0.75)) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "black") +
  theme_light() +
  scale_color_manual(values = c("AF1" = "blue", "AF2" = "green", "AF4" = "red", "AF6" = "cyan")) +
  scale_fill_manual(values = c("AF1" = "blue", "AF2" = "green", "AF4" = "red", "AF6" = "cyan")) +
  scale_pattern_manual(values = c(Simulated = "stripe", Measured = "none")) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) +
  scale_x_continuous(limits = c(2012, 2023), breaks = seq(2012, 2023, by = 1), labels = seq(2012, 2023, by = 1)) +
  scale_y_continuous(limits = c(0, 11), breaks = seq(0, 11, by = 1), labels = seq(0, 11, by = 1)) +
  labs(
    title = "Difference between Simulated & Measured Yield per year",
    subtitle = "in dashed the goal of max difference desired",
    x = "Year", y = "Diff Crop Yield (in t/ha)", pattern = "Source"
  ) +
  guides(fill = guide_legend(title = "Experiment", override.aes = list(pattern = "none")))

plot(graph_diff_yield)

## 02.02 - Graphs Reyes ----------------------------------------

result_plot_selected_runs <- result_plot %>%
  filter(interaction(result_plot$Year, result_plot$Site) %in% interaction(experimental_results$year, experimental_results$field) == T & Factor == 4)

selected_runs <- as.vector(unique(interaction(result_plot_selected_runs$SimulationName, result_plot_selected_runs$Year), na.rm = T))

for (i in 1:length(selected_runs)) {
  
  graph_reyes_A <- ggplot(result_plot_selected_runs %>% filter(interaction(result_plot_selected_runs$SimulationName, result_plot_selected_runs$Year) == selected_runs[i])) +
    geom_line(aes(x = CampagneDay, y = mainCropMeanYield * 5)) +
    geom_line(aes(x = CampagneDay, y = parIncidentMainCrop), colour = "red") +
    theme_light() +
    theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) +
    # scale_x_continuous(limits = c(100, 365)) +
    scale_y_continuous(name = "Incident PAR (MJ.m-2)", limits = c(0, 60), sec.axis = sec_axis(trans = ~ . / 5, name = "Yield (t.ha-1)")) +
    labs(
      title = glue("Graphe A from Reyes for {selected_runs[i]}"),
      x = "Day from Seeding", y = ""
    ) +
    guides()

  plot(graph_reyes_A)

  graph_reyes_B <- ggplot(result_plot_selected_runs %>% filter(interaction(result_plot_selected_runs$SimulationName, result_plot_selected_runs$Year) == selected_runs[i])) +
    geom_line(aes(x = CampagneDay, y = mainCropMeanLai)) +
    geom_line(aes(x = CampagneDay, y = mainCropHisafeWaterStress * 7.5), colour = "red") +
    theme_light() +
    theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) +
    # scale_x_continuous(limits = c(100, 365)) +
    scale_y_continuous(name = "LAI (m²/m²)", limits = c(0, 7.5), sec.axis = sec_axis(trans = ~ . / 7.5, name = "Water Stress")) +
    labs(
      title = glue("Graphe B from Reyes for {selected_runs[i]}"),
      x = "Day from Seeding", y = ""
    ) +
    guides()

  plot(graph_reyes_B)

  graph_reyes_C <- ggplot(result_plot_selected_runs %>% filter(interaction(result_plot_selected_runs$SimulationName, result_plot_selected_runs$Year) == selected_runs[i])) +
    geom_line(aes(x = CampagneDay, y = waterEvaporatedInMainCrop)) +
    geom_line(aes(x = CampagneDay, y = waterStock / 40), colour = "red") +
    theme_light() +
    theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) +
    # scale_x_continuous(limits = c(100, 365)) +
    scale_y_continuous(name = "ETP (mm)", limits = c(0, 10), sec.axis = sec_axis(trans = ~ . * 40, name = "Water Stock (mm)")) +
    labs(
      title = glue("Graphe C from Reyes for {selected_runs[i]}"),
      x = "Day from Seeding", y = ""
    ) +
    guides()

  plot(graph_reyes_C)
  
}

# -------------------------------------------------------------------------

test <- result_plot %>% 
  filter(SimulationName == "AF1_4") %>% 
  select(SimulationName, Date, JulianDay, CampagneDay, mainCropMeanYield, mainCropSceneYield, mainCropPhenologicStageVegetative, mainCropPhenologicStageReproductive, mainCropMeanLai, mainCropMeanBiomass, Year) 

graph_test <- ggplot(test) +
  geom_point(aes(x = mainCropPhenologicStageVegetative, y = mainCropMeanYield, col = Year)) +
  theme_light() +
  labs(
    x = "Crop Phenologic Stage Vegetative", y = "Yield")

plot(graph_test)

graph_test <- ggplot(test) +
  geom_point(aes(x = mainCropPhenologicStageVegetative, y = mainCropSceneYield, col = Year)) +
  theme_light() +
  labs(
    x = "Crop Phenologic Stage Vegetative", y = "Yield")

plot(graph_test)

graph_test <- ggplot(test) + 
  geom_point(aes(x = mainCropPhenologicStageVegetative, y = mainCropMeanBiomass, col = Year)) +  
  theme_light() +
  labs(
    x = "Crop Phenologic Stage Vegetative", y = "Biomass")

plot(graph_test)

graph_test <- ggplot(test) + 
  geom_point(aes(x = mainCropPhenologicStageVegetative, y = mainCropMeanLai, col = Year)) +  
  theme_light() +
  labs(
    x = "Crop Phenologic Stage Vegetative", y = "LAI")

plot(graph_test)

# -------------------------------------------------------------------------

test <- result_plot %>% 
  filter(SimulationName == "AF1_4") %>% 
  filter(Year == 2014) %>% 
  select(SimulationName, Date, JulianDay, CampagneDay, mainCropMeanYield, mainCropSceneYield, mainCropPhenologicStageVegetative, mainCropPhenologicStageReproductive, mainCropMeanLai, mainCropMeanBiomass, Year, YearCampagne)

graph_test <- ggplot(test) +
  geom_line(aes(x = CampagneDay, y = mainCropMeanYield)) +
  scale_y_continuous(limits = c(0, 10)) +
  theme_light() +
  labs(x = "Day from Seeding", y = "Yield")

plot(graph_test)

graph_test <- ggplot(test) +
  geom_line(aes(x = CampagneDay, y = mainCropMeanBiomass)) +
  theme_light() +
  labs(x = "Day from Seeding", y = "Biomass")

plot(graph_test)


# -------------------------------------------------------------------------

test <- experimental_results %>% 
  select(field, year, tonDM) %>% 
  rename(Site = "field",
         Year = "year",
         MeasuredYield = "tonDM")

temp <- result_plot_yield %>% 
  select(Site, Year, mainCropMeanYield) %>% 
  rename(SimulatedYield = "mainCropMeanYield")

temp_2 <- merge(test, temp)

plot(x = temp_2$MeasuredYield, y = temp_2$SimulatedYield)

# -------------------------------------------------------------------------

# lm([target] ~ [predictor / features], data = [data source])
accuracy_simulation <- lm(formula = measured_yield ~ simul_yield + year + simul_factor, data = data_yield)

summary(accuracy_simulation)

plot(accuracy_simulation)

## 02.02 - Graphs Hydric Budget ----------------------------------------

test <- hisafer::plot_hisafe_cycle_bar(hop_AF1_1, cycle = "water", freq = "year")

ggplot(data = water_budget_yearly %>% filter(interaction(water_budget_yearly$Site, water_budget_yearly$Year) %in% interaction(experimental_results$field, experimental_results$year) == T)) +
  geom_point(aes(x = Year, y = ))

colnames(water_budget_yearly %>% filter(interaction(water_budget_yearly$Site, water_budget_yearly$Year) %in% interaction(experimental_results$field, experimental_results$year) == T))

plot(test)

water_budget_yearly_clean <- water_budget_yearly %>%
  filter(interaction(water_budget_yearly$Site, water_budget_yearly$Year) %in% interaction(experimental_results$field, experimental_results$year) == T)

plot_water_end_budget <- ggplot(data = water_budget_yearly_clean, aes(x = Year)) +
  # geom_point(aes(y = totalStockEnd, col = Site), shape = 17) +
  geom_point(aes(y = -precipitation, col = Site)) +
  theme_light() +
  scale_color_manual(values = c("AF1" = "blue", "AF2" = "green", "AF4" = "red", "AF6" = "cyan")) +
  scale_x_continuous(minor_breaks = c()) +
  theme()

plot(plot_water_end_budget)  
