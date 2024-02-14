library(glue)

# PLD File Modification Functions ----------------------------------------------------

modification_add_second_tree <- function(hip) {
  pld <- readLines(glue::glue("{hip$path}/{hip$exp.plan$SimulationName}/{hip$exp.plan$SimulationName}.pld", .null = character))
  pld <- R.utils::insert(x = pld, ats = 71, values = pld[70])
  pld <- R.utils::insert(x = pld, ats = 75, values = pld[75])
  pld[70] <- glue::glue("TreeInit\tpoplar_V3\t3\t0.75\t0.375\t0.25\t{tree_coordinates[1,1]}\t{tree_coordinates[1,2]}\t1\t1\t0,240,605", .null = character)
  writeLines(pld, glue::glue("{hip$path}/{hip$exp.plan$SimulationName}/{hip$exp.plan$SimulationName}.pld", .null = character))
  
  return("Addition of a second tree done")
}

modification_north_orientation <- function(hip, orientation) {
  pld <- readLines(glue::glue("{hip$path}/{hip$exp.plan$SimulationName}/{hip$exp.plan$SimulationName}.pld", .null = character))
  pld[7] <- as.character(glue::glue("northOrientation = {orientation}"))
  writeLines(pld, glue::glue("{hip$path}/{hip$exp.plan$SimulationName}/{hip$exp.plan$SimulationName}.pld", .null = character))
  
  return("Modification of north orientation done")
}

modification_soil_physical_parameters <- function(hip, df_physical_properties) {
  pld <- readLines(glue::glue("{hip$path}/{hip$exp.plan$SimulationName}/{hip$exp.plan$SimulationName}.pld", .null = character))
  pld <- pld[-c(54:58)]
  data_soil_physical <- df_physical_properties
  for (i in rev(1:nrow(data_soil_physical))) {
    pld <- R.utils::insert(x = pld, ats = 54, values = pld[54])
    pld[54] <- paste(as.character(data_soil_physical[i, ]), collapse = "\t")
  }
  writeLines(pld, glue::glue("{hip$path}/{hip$exp.plan$SimulationName}/{hip$exp.plan$SimulationName}.pld", .null = character))
  
  return("Modification of soil physical parameters done")
}

modification_soil_chemical_parameters <- function(hip, df_chemical_properties) {
  pld <- readLines(glue::glue("{hip$path}/{hip$exp.plan$SimulationName}/{hip$exp.plan$SimulationName}.pld", .null = character))
  pld <- pld[-c(62:66)]
  data_soil_chemical <- df_chemical_properties
  for (i in rev(1:nrow(data_soil_chemical))) {
    pld <- R.utils::insert(x = pld, ats = 62, values = pld[62])
    pld[62] <- paste(as.character(data_soil_chemical[i, ]), collapse = "\t")
  }
  writeLines(pld, glue::glue("{hip$path}/{hip$exp.plan$SimulationName}/{hip$exp.plan$SimulationName}.pld", .null = character))
  
  return("Modification of soil chemical parameters done")
}

# SIM File Modification Functions ----------------------------------------------------

modification_nb_years_simulation <- function(hip, duration) {
  sim <- readLines(glue::glue("{hip$path}/{hip$exp.plan$SimulationName}/{hip$exp.plan$SimulationName}.sim", .null = character))
  sim[2] <- as.character(glue::glue("nbSimulations = {duration}"))
  writeLines(sim, glue::glue("{hip$path}/{hip$exp.plan$SimulationName}/{hip$exp.plan$SimulationName}.sim", .null = character))
  
  return("Modification of the number of years done")
}

