#ENV


pacman::p_load(rgdal, raster, terra, stars, sp, sf, tidyverse,  ggplot2, clhs)

# DATA

# data <- readRDS("./data/harmonized_covs/harmonized_data.rds")

# covariates_aoi <- readRDS("D:/DOCS/CIENCIA DE SUELOS/R_SUELOS/glacier_mlMODIS/data/harmonized_covs/harmonized_data_pred.rds")

covariates_aoi_names <- list.files(path = "./data/harmonized_covs/stacks/",
                              recursive = TRUE,
                              full.name = TRUE,
                              pattern = "*.gri$") 
raster_list <- list()

for (file in covariates_aoi_names) {
  raster_stack <- raster::stack(file)
  raster_list[[file]] <- raster_stack
}



raster_list

lits_names <- c("Blanca",
                "Central",
                "Huallanca",
                "Huayhuasha",
                "Huaytapallana",
                "LaRaya",
                "LaViuda",
                "Raura",
                "Urubamba",
                "Vilcabamba",
                "VIlcanota"
)

names(raster_list) <- lits_names



#FUNCION


# sampling_clh <- function(lista_stack, sampling_size){
#   
#   set.seed(1234)
#   # crear mask rasters 
#   # si es clasificacion multicategoria usar un raster para cada categoria
#   
#   sampling_list <- list()
#   
#   for (i in 1:length(lista_stack)) {
#     
#     mask_in <- lista_stack[[i]]$CLASS # mascara clase  glaciar lista_stack[[i]][[1]]
#     mask_out <- lista_stack[[i]]$CLASS # mask class  no glaciar lista_stack[[i]][[1]]
#     
#     mask_in[mask_in != 1] <- NA # 1 : glaciar, NA: no glaciar
#     mask_out[mask_out == 1] <- NA  # 1: no glaiar, NA : glaciar
#     
#     
#     # masking
#     
#     stack_in <- raster::mask(
#       lista_stack[[i]], #rasterstack original
#       mask_in, # mask
#       maskvalue = NA) 
#     
#     stack_out <- raster::mask(
#       lista_stack[[i]], 
#       mask_out, 
#       maskvalue = NA) 
#     
#     #clh, ambas clases igual sampling_size de puntos, muestreo balanciado
#     
#     # MUESTREO ESTRATIFICADO EN CADA CLASE
#     # clase 1
#     stack_df_in <- rasterToPoints(stack_in, spatial=TRUE) # all pixels to dataframe
#     sample_in <- sampleRandom(stack_in, size = sampling_size, sp = TRUE) # all pixels to dataframe
#     # sample_in <- clhs(stack_df_in, size = sampling_size,
#     #                           progress = FALSE, iter = 10000, simple = FALSE) # clh object
# 
#     
#     # #clase 2
#     # stack_df_out <- rasterToPoints(stack_out, spatial=TRUE) # all pixels to dataframe
#     sample_out <- sampleRandom(stack_out, size = sampling_size, sp = TRUE)
#     # sample_out <- clhs(stack_df_out, size = sampling_size, 
#     #                            progress = FALSE, iter = 10000, simple = FALSE)
#     # 
#     
#     # sampling original stack on clh points
#     
#     # muestra <- rbind(clhs_points_df_in$sampled_data,
#     #                   clhs_points_df_out$sampled_data ) 
#     # 
#     #unir sp objetcs
#     
#     
#     muestra <- rbind(sample_in, # $sampled_data si se usa clh
#                      sample_out) 
#     
#     muestra_sf <- st_as_sf(muestra)
#     
#     # sampling_list <- list()
#     sampling_list[[i]] <- muestra_sf
#     
#   }
#   
#   return(sampling_list)
# }
# 
# 
# # limpiar memoria 
# 
# # rm(list=setdiff(ls(), "data"))
# # # rm(list = ls()) # limpiar todo el envi
# # memory.size()
# # memory.limit()
# # memory.limit(size=56000)
# # gc() # limpiar ram
# # 
# 
# ### apply function a toda la lista de stacks 
# sample1000 <- sampling_clh(data[9], 500) # escoger 500 como paper AGB ploton # falta el 10
# 
# #Huallanca 3
# #Huayhuasha 4 
# # Huaytapallana 5 
# # raura 8 
# # la raya 6
# # vilcanota 11
# #urubamba 9
# #vilcabamba 10
# 
# #contar nas
# sapply(sample1000[[1]], function(x) sum(is.na(x)))
# 
# 
# #WRITE COMO CSV
# 
# # saveRDS(sample1000[[1]], "./data/sampling_points/Blanca_samples1000.rds") 
# st_write(sample1000[[1]],
#          "./data/sampling_points/Urubamba_samples1000.gpkg",
#          driver="GPKG") 
# 
# #WRITE COMO  geopkg
# 
# 
# # for (i in 1:length(list_stck)) {
# #   
# #   raster::writeRaster(list_stck[[i]],
# #                       filename = paste0("./data/landsat/indices/index-",
# #                       landsat_cordilleras_chr[i], ".grd"), 
# #                       format = "raster")
# #   
# # }
# 
# 
# cordilleras <- c("Blanca",
#                           "Central",
#                           "Huallanca",
#                           "Huayhuasha",
#                           "Huaytapallana",
#                           "LaRaya",
#                           "LaViuda",
#                           "Raura",
#                           "Urubamba",
#                           "Vilcabamba",
#                           "Vilcanota"
# )
# 
# 
# 
# for (i in 1:length(sample1000)) {
#   
# st_write(sample1000[[i]], 
#            # "./data/sampling_points/sampling_points_blanca.gpkg",
#            paste0("./data/sampling_points/sample_",cordilleras[i], ".gpkg"),
#            driver="GPKG") 
# }
# 




# sampling_points_gpkg <- st_as_sf(sampling_points[[1]])
# st_write(sampling_points[[1]], "./data/sampling_points/sampling_points_blanca.gpkg", driver="GPKG")  # Create a geopackage file
# saveRDS(sampling_points[[1]], "./data/harmonized_covs/sampling_points_blanca.rds") 



#puntos
# x <- sampleStratified(data$Vilcanota$CLASS, size=500, sp = TRUE)
# #extraer data from stacks
# x_ <- raster::extract(data$Vilcanota, x, sp=TRUE, method = "simple")
# # #write
# st_write(st_as_sf(x_),
#          "./data/sampling_points/sample1000_vilcanota.gpkg",
#          driver="GPKG")
#plot
# par(mar = c(1,1,1,1))
# plot(data$Vilcanot$BLUE, axes=FALSE)
# plot(x_, col = "red", add=TRUE)
# 

####
####  MUESTREO ESTRATIFICADOO
####

sample_list <- list()
for (i in 1:length(data)) {
 
  sample_point <- sampleStratified(data[[i]]$CLASS, size=500, sp = TRUE) #1000 puntos de muestreo
  sample <- raster::extract(data[[i]], sample_point, sp=TRUE, method = "simple")
  sample <- st_as_sf(sample)
  sample_list[[i]] <- sample
}

#####
##### CHECK CRS

for (i in 1:length(sample_list)) {
  crs <- st_crs(sample_list[[i]])$input
  print(crs)
}



# # warp one to crs of others
# 
# for (i in 1:length(samples_gpkg)) {
#   # Transform the CRS of each sf object to the target CRS
#   samples_gpkg[[i]] <- st_transform(samples_gpkg[[i]], st_crs("EPSG:4326"))
#   # Print the CRS of the transformed sf object
#   print(st_crs(samples_gpkg[[i]])$input)
# }
# 



for (i in 1:length(sample_list)) {
  
  st_write(sample_list[[i]], 
           paste0("./data/sampling_points/sample1000_",cordilleras[i], ".gpkg"),
           driver="GPKG") 
}

####
####  MUESTREO SIMPLE
####

sample_list <- list()
for (i in 1:length(raster_list)) {
  
  sample_point <- sampleRandom(raster_list[[i]], size= 1000, sp = TRUE) #1000 puntos de muestreo
  sample <- raster::extract(raster_list[[i]], sample_point, sp=TRUE, method = "simple")
  sample <- st_as_sf(sample)
  sample_list[[i]] <- sample
}


#####
##### CHECK CRS

for (i in 1:length(sample_list)) {
  crs <- st_crs(sample_list[[i]])$input
  print(crs)
}



# # warp one to crs of others
# 
# for (i in 1:length(samples_gpkg)) {
#   # Transform the CRS of each sf object to the target CRS
#   samples_gpkg[[i]] <- st_transform(samples_gpkg[[i]], st_crs("EPSG:4326"))
#   # Print the CRS of the transformed sf object
#   print(st_crs(samples_gpkg[[i]])$input)
# }
# 


for (i in 1:length(sample_list)) {
  
  st_write(sample_list[[i]], 
           paste0("./output/test_samples/", lits_names[i], "_test",".gpkg"),
           driver="GPKG") 
}




##############
##############   CRS TRANSFORMATION
#############

pacman::p_load(mlr,mlrMBO,DiceKriging, rgenoud, sf, terra, ranger, xgboost, tidyverse, ggplot2, ggExtra  )
#data/task


list_files <- list.files(path = "./data/sampling_points/",
                         recursive = TRUE, 
                         full.name = TRUE, # directorio relativo raster
                         pattern = "\\.gpkg$")

list_gpkg <- lapply(list_files, FUN =  sf::read_sf) 
# set crs EPSG:4326

for (i in seq_along(list_gpkg)) {
  sf::st_crs(list_gpkg[[i]]) <- 4326
}



# sf::st_crs(list_gpkg[[8]]) 


# proyectar a utm 18 y 19 (vilcanota y la raya)

cordilleras <- c("Blanca","Central","Huallanca",
                 "Huayhuasha","Huaytapallana","LaRaya",
                 "LaViuda","Raura","Urubamba",
                 "Vilcabamba","Vilcanota")
names(list_gpkg) <- cordilleras
cordilleras_utm18<- c("Blanca","Central","Huallanca",
                      "Huayhuasha","Huaytapallana","LaRaya",
                      "LaViuda","Raura","Urubamba",
                      "Vilcabamba")
cordilleras_utm19<- c("LaRaya","Vilcanota")


list_gpkg_utm <- list()
for (i in seq(list_gpkg)) {
  
  epsg <- list(utm18 = 32718, 
               utm19 = 32719 )
  
  if (names(list_gpkg[i]) %in% cordilleras_utm18 ) {
    
    list_gpkg_utm[[i]] <-  sf::st_transform(list_gpkg[[i]], crs = epsg[[1]]  )
  } else {
    
    list_gpkg_utm[[i]] <- sf::st_transform(list_gpkg[[i]], crs = epsg[[2]] )
  }
}

list_gpkg_utm


# revisar cuadros, extrare 500 puntos el benchmarking y 500 para evaluacion final
#etiquetar 500 obs

names(list_gpkg_utm) <- cordilleras

train_points <- c(rep("train", 500) , rep("test", 500))
train_points_ <- sample(train_points, size = 1000, replace = FALSE)

x <-  list_gpkg_utm$Raura

# contar obseraciones de la clase 1 y clase 0 
# y <- x %>% dplyr::mutate(train_test = train_points_) 



list_gpkg_utm_ <- list()
for (i in seq_along(list_gpkg_utm)) {
  
  set.seed(1234)
  train_points <- c(rep("train", 500) , rep("test", 500))
  train_points_ <- sample(train_points, size = 1000, replace = FALSE)
  
  list_gpkg_utm_[[i]] <- list_gpkg_utm[[i]] %>% 
    dplyr::mutate(train_test = train_points_)
  
}


#comprobar balance data de entrenamiento
list_gpkg_utm_[[11]] %>% 
  dplyr::filter(train_test=="train") %>% 
  count(CLASS, sort = TRUE, name= "Code_frequency")

#comprobar nas
x <- list_gpkg_utm_[[11]]
sapply(x, function(x) sum(is.na(x)))






#write
# st_write(y, 
#            "./test/raura_train_points_.gpkg",
#            driver="GPKG") 


for (i in 1:length(list_gpkg_utm_)) {
  
  st_write(list_gpkg_utm_[[i]], 
           paste0("./data/sampling_points/sample1000utm_",cordilleras[i], ".gpkg"),
           driver="GPKG") 
}


