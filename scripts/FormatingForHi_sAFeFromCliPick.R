library(tidyverse)
library(rstudioapi)
library(glue)

# Script Formating Data For Hi-sAFe Dataset from CliPick to a format suitable for meteo data used in Hi-sAFe

rm(list = ls())

source_data <- "CliPick"

path_raw_data <- as.character(glue("{rstudioapi::getActiveProject()}/meteo"))

watertable_default <- read.csv2(as.character(glue("{rstudioapi::getActiveProject()}/watertable_default.txt")), header = F)

# Read the datasets present

list_data <- list.files(path = path_raw_data)

for (i in 1:length(list_data)) {
  
  temp <- read.csv2(as.character(glue("{rstudioapi::getActiveProject()}/meteo/{list_data}"))[i], sep = ",", dec = ".", header = F)
  temp <- temp[-1,]
  colnames(temp) <- temp[1,]
  temp <- temp[-1,]
  
  # View(temp)
  
  temp <- temp %>% 
    mutate(DOY = yday(make_date(day = Day, month = Month, year = Year)), .before = Day) %>% 
    mutate(WT = rep(watertable_default[, 1], length.out = nrow(temp)),
           AIR_CO2 = 330) %>% 
    relocate(Year, .after = DOY) %>% 
    relocate(Day, .before = tasmax)
  
  write.table(temp, file = as.character(glue("{rstudioapi::getActiveProject()}/meteo/output_dataset_clipick_{i}.wth")), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
}
