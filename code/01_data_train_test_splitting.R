pacman::p_load(sf)

# todos los puntos en WGS 84 / UTM zone 19S

list_files <- list.files(path = "./data/sampling_points/",
                         recursive = TRUE,
                         full.name = TRUE, # directorio relativo raster
                         pattern = "\\.gpkg$")
list_gpkg <- lapply(list_files, FUN =  sf::read_sf)

#SPLIT TRAIN- TEST


# # filter train points
 
list_gpkg_train <- list()
for (i in seq_along(list_gpkg)) {
  list_gpkg_train[[i]] <- list_gpkg[[i]] %>%
    dplyr::filter(train_test=="train") %>%
    dplyr::select(!c("cell","CLASS.1", "train_test")) %>%
    na.omit()

}
 
# #test data
 
list_gpkg_test <- list()
for (i in seq_along(list_gpkg)) {
  list_gpkg_test[[i]] <- list_gpkg[[i]] %>%
    dplyr::filter(train_test=="test") %>%
    dplyr::select(!c("cell","CLASS.1", "train_test")) %>%
    na.omit()
}


# #SPATIAL TASK
# 
cordilleras <- c("Blanca","Central","Huallanca",
                 "Huayhuasha","Huaytapallana","LaRaya",
                 "LaViuda","Raura","Urubamba",
                 "Vilcabamba","Vilcanota")

#WRITE GPKG

for (i in 1:length(list_gpkg_train)) {
  # Creating a filename using the corresponding name from the vector
  filename <- paste0("./data/sampling_points_split/", cordilleras[i], "_train", ".gpkg")
  
  # Saving the SF object to the file
  st_write(list_gpkg_train[[i]], filename)
}


#save test

for (i in 1:length(list_gpkg_test)) {
  # Creating a filename using the corresponding name from the vector
  filename <- paste0("./data/sampling_points_split/" ,cordilleras[i], "_test", ".gpkg")
  
  # Saving the SF object to the file
  st_write(list_gpkg_test[[i]], filename)
}




