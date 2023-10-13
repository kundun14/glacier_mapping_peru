
######
###### ENV
######

pacman::p_load(sf, sp, glmnet, ranger, kknn, gbm, spaMM, gstat,
               spgwr, spdep, caret , Metrics, mccr,
               geoR, raster, rgeos, sperrorest, ggplot2, gridExtra, tidyverse)

######
###### READ
######


list_files <- list.files(path = "./data/sampling_points_split/",
                         recursive = TRUE,
                         full.name = TRUE,
                         pattern = ".*_train\\.gpkg$")  # Only include files ending with "_train.gpkg"

list_gpkg <- lapply(list_files, FUN = sf::read_sf)

######
###### TO INDIVIDUAL FILES 
######


cordilleras <- c("Blanca","Central","Huallanca",
                 "Huayhuasha","Huaytapallana","LaRaya",
                 "LaViuda","Raura","Urubamba",
                 "Vilcabamba","Vilcanota")


for (i in seq_along(cordilleras)) {
  variable_name <- paste0(cordilleras[i], "_train")
  assign(variable_name, list_gpkg[[i]])
}


Blanca_train
Central_train
Huallanca_train
Huayhuasha_train




