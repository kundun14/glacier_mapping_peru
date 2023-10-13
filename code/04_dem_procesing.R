
library(tidyverse)
# library(osmdata)

library(sf) # sf_use_s2()
library(sp)

# library(rayshader) # To install rayshader, must install XQuartz first en mac : https://github.com/tylermorganwall/rayshader/issues/86
library(raster)
library(elevatr)
library(terrainr) # buffers
library(terra)

# library(plotly) 
# library(reshape2)
# library(akima)
# library(htmlwidgets)
# library(rgl)
# library(leaflet)

# library(scales)

# BBOX DE LAS COORDILLERAS

list_files_bbox <- list.files(path = "data/inaigem/shape_tiles/",
                         recursive = TRUE, 
                         full.name = TRUE, # directorio relativo raster
                         pattern = "\\.shp$")

list_files_bbox_sf <- lapply(list_files_bbox, FUN =  sf::st_read)
# list.files.stack <- lapply(list.files, FUN =  raster::stack) 

# convertir a sp object cada bbox

# alay_sp <- as(alay$geometry, "Spatial") 
# sf::as_Spatial(list_files_bbox_sf[[1]])


#comprobar CRS proyectar de ser necesario


# #CUENCA 
# 
# alay <- sf::st_read("./dem_covs/alay_grass_delim/alay_grass.shp") 


#Bounding box:  xmin: -70.94431 ymin: -13.89851 xmax: -70.85465 ymax: -13.77737
# alay_sp <- as(alay$geometry, "Spatial")
# mapview(alay_sp)

## DEM
# en caso ya se tenga el dem descargado

# dem_alos <- raster::raster( "./dem_alos.tif")
# aoi <- raster::stack( "./stacks_indices/imagen_1.tif")
# dem_alos_mask  <- raster::crop(dem_alos, aoi)
# mapview(dem_alos_mask)
# raster::writeRaster(dem_alos_mask, "dem_alos_cuenca", format = "GTiff")
# raster::writeRaster(raster::projectRaster(from =dem_alos_mask, crs = crs(alay) ), "dem_alos_cuenca_utm19", format = "GTiff")

# raster::extent(alos)


# DESCARGAR DEM EN BASE AL BBOX DE CADA CORDILLERA

# SRTMGL3, SRTMGL1, AW3D30, and SRTM15Plus datasets
#https://dwtkns.com/srtm30m/
#API KEY OPEN TOPO d64d42bbd6e4c891134dfde0af54fcd8

set_opentopo_key("d64d42bbd6e4c891134dfde0af54fcd8")
# get_elev_raster(clip_box, expand = 0.1) # buffer de descarga en uniades del mapa
# dem_alos_shape <- get_elev_raster(alay_sp, src = "alos", clip = "bbox", expand = 0.1)# buffer de descarga en uniades del mapa


dem_bbox <-  list()
for (i in 1:length(list_files_bbox)) {
  
  dem_bbox[[i]] <- get_elev_raster(list_files_bbox_sf[[i]], #Either a data.frame of x (long) and y (lat), an sp, sf, or raster object as input
                                   src = "alos", 
                                   clip = "bbox", 
                                   expand = 0.01)# buffer  en unidades de mapa

}

#names
cordilleras_bbox_chr <- c("Blanca_tile","Central_tile","Huallanca_tile",
                          "Huayhuasha_tile","Huaytapallana_tile","LaRaya_tile",
                          "LaViuda_tile","Raura_tile","Urubamba_tile",
                          "Vilcabamba_tile","Vilcanota_tile")

names(dem_bbox) <- cordilleras_bbox_chr

#PLOT PRUEBA
#SIEMPRE PROBAR PRIMERO CON CORDILLERA RAURA, ES PEQUEÑA

dem_bbox[[8]] # raura?


#ALOS World 3D - 30m
# plot(dem_alos_shape)
# plot(alay_sp, add = T)


#WRITE DEMS

# raster::writeFormats() # "GTiff"
# raster::writeRaster(dem_alos, "./dem_alos", format = "GTiff" )


for (i in 1:length(dem_bbox)) {
  
  raster::writeRaster(dem_bbox[[i]],
                      filename = paste0("./data/dem/dem_", cordilleras_bbox_chr[i], ".tif"), 
                      format = "GTiff")
  
}



# COVRIBLES GENERDS  O SAGA

# twi_saga <- raster::raster( "./dem_covs/twi.tif")
# mapview(twi_saga)
# Flow_Accumulation <- raster::raster( "./dem_covs/Flow_Accumulation.tif") 
# mapview(Flow_Accumulation)
# # red de flujo
# reclass_df <- c(0, 50000, NA, # menos de 100000 m de acumulcion de flujo no es corriente de agua
#                 50000, 100000, 1 , # mas de 100000 m de acumulcion de flujo si es corriente de agua
#                 100000, 150000, 2,
#                 150000, 300000, 3, 
#                 300000, Inf, 4) 
# 
# reclass_m <- matrix(reclass_df,
#                     ncol = 3,
#                     byrow = TRUE)
# 
# water_lines <- reclassify(Flow_Accumulation,
#                              reclass_m)
# water_lines[water_lines == 0] <- NA
# mapview(water_lines)
# raster::writeRaster(water_lines, "./dem_covs/water_lines", format = "GTiff" )
# 
# # COVRIBLES GENERDS EN GRASS 
# 
# flow_dir <- raster::raster( "./dem_covs/flow_dir_grass.tif")
# mapview(flow_dir)



# LOAD SAGA COVARIABLES




# crop to original dem ( sin buffer)
#stack saga covs + landsats covs


# LOAD DATA 



# SAGA + DEM
#SAGA COVS

#saga_dem_Blanca_tile
saga_dem_Blanca_tile <- list.files(path = "./data/dem/saga_dem_Blanca_tile/",
                             recursive = TRUE, 
                             full.name = TRUE, # directorio relativo raster
                             pattern = "\\.tif$")
saga_dem_Blanca_tile <- lapply(saga_dem_Blanca_tile, FUN =  raster::raster) 
saga_Blanca_stack <- raster::stack(saga_dem_Blanca_tile)


#saga_dem_Central_tile
saga_dem_Central_tile <- list.files(path = "./data/dem/saga_dem_Central_tile/",
                                   recursive = TRUE, 
                                   full.name = TRUE, # directorio relativo raster
                                   pattern = "\\.tif$")
saga_dem_Central_tile <- lapply(saga_dem_Central_tile, FUN =  raster::raster) 
saga_Central_stack <- raster::stack(saga_dem_Central_tile)


#saga_Huallanca_stack
saga_dem_Huallanca_tile <- list.files(path = "./data/dem/saga_dem_Huallanca_tile/",
                                    recursive = TRUE, 
                                    full.name = TRUE, # directorio relativo raster
                                    pattern = "\\.tif$")
saga_dem_Huallanca_tile <- lapply(saga_dem_Huallanca_tile, FUN =  raster::raster) 
saga_Huallanca_stack <- raster::stack(saga_dem_Huallanca_tile)


#saga_Huayhuasha_stack
saga_dem_Huayhuasha_tile <- list.files(path = "./data/dem/saga_dem_Huayhuasha_tile/",
                                    recursive = TRUE, 
                                    full.name = TRUE, # directorio relativo raster
                                    pattern = "\\.tif$")
saga_dem_Huayhuasha_tile <- lapply(saga_dem_Huayhuasha_tile, FUN =  raster::raster) 
saga_Huayhuasha_stack <- raster::stack(saga_dem_Huayhuasha_tile)

#saga_Huaytapallana_stack
saga_dem_Huaytapallana_tile <- list.files(path = "./data/dem/saga_dem_Huaytapallana_tile/",
                                    recursive = TRUE, 
                                    full.name = TRUE, # directorio relativo raster
                                    pattern = "\\.tif$")
saga_dem_Huaytapallana_tile <- lapply(saga_dem_Huaytapallana_tile, FUN =  raster::raster) 
saga_Huaytapallana_stack <- raster::stack(saga_dem_Huaytapallana_tile)

#saga_LaRaya_stack
saga_dem_LaRaya_tile <- list.files(path = "./data/dem/saga_dem_LaRaya_tile/",
                                    recursive = TRUE, 
                                    full.name = TRUE, # directorio relativo raster
                                    pattern = "\\.tif$")
saga_dem_LaRaya_tile <- lapply(saga_dem_LaRaya_tile, FUN =  raster::raster) 
saga_LaRaya_stack <- raster::stack(saga_dem_LaRaya_tile)

#saga_LaViuda_stack
saga_dem_LaViuda_tile <- list.files(path = "./data/dem/saga_dem_LaViuda_tile/",
                                    recursive = TRUE, 
                                    full.name = TRUE, # directorio relativo raster
                                    pattern = "\\.tif$")
saga_dem_LaViuda_tile <- lapply(saga_dem_LaViuda_tile, FUN =  raster::raster) 
saga_LaViuda_stack <- raster::stack(saga_dem_LaViuda_tile)

#saga_Raura_stack
saga_dem_Raura_tile <- list.files(path = "./data/dem/saga_dem_Raura_tile/",
                                    recursive = TRUE, 
                                    full.name = TRUE, # directorio relativo raster
                                    pattern = "\\.tif$")
saga_dem_Raura_tile <- lapply(saga_dem_Raura_tile, FUN =  raster::raster) 
saga_Raura_stack <- raster::stack(saga_dem_Raura_tile)

#saga_Urubamba_stack
saga_dem_Urubamba_tile <- list.files(path = "./data/dem/saga_dem_Urubamba_tile/",
                                    recursive = TRUE, 
                                    full.name = TRUE, # directorio relativo raster
                                    pattern = "\\.tif$")
saga_dem_Urubamba_tile <- lapply(saga_dem_Urubamba_tile, FUN =  raster::raster) 
saga_Urubamba_stack <- raster::stack(saga_dem_Urubamba_tile)




#saga_Vilcabamba_stack
saga_dem_Vilcabamba_tile <- list.files(path = "./data/dem/saga_dem_Vilcabamba_tile/",
                                     recursive = TRUE, 
                                     full.name = TRUE, # directorio relativo raster
                                     pattern = "\\.tif$")
saga_dem_Vilcabamba_tile <- lapply(saga_dem_Vilcabamba_tile, FUN =  raster::raster) 
saga_Vilcabamba_stack <- raster::stack(saga_dem_Vilcabamba_tile)

#saga_Vilcanota_stack
saga_dem_Vilcanota_tile <- list.files(path = "./data/dem/saga_dem_Vilcanota_tile/",
                                     recursive = TRUE, 
                                     full.name = TRUE, # directorio relativo raster
                                     pattern = "\\.tif$")
saga_dem_Vilcanota_tile <- lapply(saga_dem_Vilcanota_tile, FUN =  raster::raster) 
saga_Vilcanota_stack <- raster::stack(saga_dem_Vilcanota_tile)


# CROP
# MASK AL EXTENT DE CADA TILE ORIGINAL (SIN BUFFER)

#STACKEAR PARA TODAS LAS CORDILLERAS

saga_list_stacks <- list(saga_Blanca_stack,
                         saga_Central_stack,
                         saga_Huallanca_stack,
                         saga_Huayhuasha_stack,
                         saga_Huaytapallana_stack,
                         saga_LaRaya_stack,
                         saga_LaViuda_stack,
                         saga_Raura_stack,
                         saga_Urubamba_stack,
                         saga_Vilcabamba_stack,
                         saga_Vilcanota_stack)

# alay_latlon <- st_transform(alay, crs(dem_alos_mask)) # shapeen lan lot

# TILES

# tiles_list <- list.files(path = "./data/inaigem/shape_tiles/",
#                                       recursive = TRUE, 
#                                       full.name = TRUE, # directorio relativo raster
#                                       pattern = "\\.shp$")
# 
# tiles_list_sf <- lapply(tiles_list, FUN =  sf::read_sf) 
# 
#DEM

# list_files_dem <- list.files(path = "./data/dem/dems/",
#                              recursive = TRUE,
#                              full.name = TRUE, # directorio relativo raster
#                              pattern = "\\.tif$")
# 
# dems_list <- lapply(list_files_dem, FUN =  raster::raster)



# DEM ES UN POCO MAS GRANDE QUE LAS COV SAGA
#  COVS SAGA SON MAS GRANDES QUE LANDSAT COVS


# 
# 
# # saga_Vilcanota_stack <- raster::stack(saga_dem_Vilcanota_tile)
# 
# 
# # MASK DEMS COVS TO TILES 
# 
# 
# dem_mask <- function(dems_list,tiles_list_sf){
#   
#   # dems_list : lista de raster* objects
#   # tiles_list_sf : lista de sf objects
#   
#   dem_msk_tile <- list() # list de dem masked (bbox de cada cordillera)
#   dems_list <- lapply(dems_list, FUN =  terra::rast) #convertir previamente de raster* to spatRas para  funcion de terra::mask
#   tiles_list_sf <- lapply(tiles_list_sf, FUN =  terra::vect) ####convertir previamente de sf to spatVector para  funcion de terra::mask
#   
#   for (i in 1:length(dems_list)) {
#     
#     
#     dem_msk_tile[[i]] <- terra::mask(dems_list[[i]],
#                                      tiles_list_sf[[i]])
#     
#   }
#   
#   dem_msk_tile <- lapply(dem_msk_tile, FUN =  raster::raster) # volver a raster*object
#   return(dem_msk_tile)
# }
#   
# 
# dem_mask_list <- dem_mask(dems_list,tiles_list_sf) # lista de dems masked
# 
# 
# 
# 
# 
# # MASK SAGA COVS TO TILES 
# 
# 
# 
# saga_mask <- function(saga_list_stacks,tiles_list_sf){
#   
#   # saga_list_stacks : lista de raster*stacks objects para cada cordillera
#   # tiles_list_sf : lista de sf objects
#   
#   saga_msk_tile <- list() # list de dem masked (bbox de cada cordillera)
#   saga_list_stacks <- lapply(saga_list_stacks, FUN =  terra::rast) #convertir previamente de raster* to spatRas para  funcion de terra::mask
#   tiles_list_sf <- lapply(tiles_list_sf, FUN =  terra::vect) ####convertir previamente de sf to spatVector para  funcion de terra::mask
#   
#   for (i in 1:length(saga_list_stacks)) {
#     
#     
#     saga_msk_tile[[i]] <- terra::mask(saga_list_stacks[[i]],
#                                      tiles_list_sf[[i]])
#     
#   }
#   
#   saga_msk_tile <- lapply(saga_msk_tile, FUN =  raster::stack) # volver a raster*object
#   return(saga_msk_tile)
# }
# 
# 
# saga_mask_list <- saga_mask(saga_list_stacks,tiles_list_sf) # lista de dems masked
# 
# #write prueba
# raster::write (saga_mask_list[[8]], "./raura_saga_masked.tif", format = "GTif" ) # scribir rarura
# 


#STACK DEM + SAGA COVS PARA CADA CORDILLERA EN UNA LISTA



# saga_mask_list[[8]]




#stacking

# dem_saga_tiled <- list()
# for (i in 1:length(dem_mask_list)) {
#   
# dem_saga_tiled[[i]]  <- raster::stack(dem_mask_list[[i]],
#                                       saga_mask_list[[i]])
#   
# }
# 
# #cambiar nombres de covs.
# 
# names_covs <- c ("DEM","ASPECT","CROSC","LONGC","MAXIC","MINIC","PLANC","PROFC","SLOPE")
# 
# 
# for (i in 1:length(saga_mask_list)) {
#   
#   names(dem_saga_tiled[[i]]) <- names_covs 
#   
# }



# # WRITE LISTA DE STACKS

#nombre para cade elemto de la lista ed stacks segun la cordillera de estudio
saga_cordilleras_chr <- c("Blanca_saga_dem_tiled",
                          "Central_saga_dem_tiled",
                          "Huallanca_saga_dem_tiled",
                          "Huayhuasha_saga_dem_tiled",
                          "Huaytapallana_saga_dem_tiled",
                          "LaRaya_saga_dem_tiled",
                          "LaViuda_saga_dem_tiled",
                          "Raura_saga_dem_tiled",
                          "Urubamba_saga_dem_tiled",
                          "Vilcabamba_saga_dem_tiled",
                          "Vilcanota_saga_dem_tiled"
)

names(dem_saga_tiled) <- saga_cordilleras_chr


saveRDS(dem_saga_tiled, "./data/dem/saga_dem_tiled/dem_saga_tiled.rds")

#EXPORT RAURA EJEMPLO

# raster::write (dem_saga_tiled[[8]][1], "./raura_dem_masked.tif", format = "GTif" ) # dem rarura
# raster::write (dem_saga_tiled[[8]][9], "./raura_dem_masked.tif", format = "GTif" ) #  SLOPE rarura



#LANDSAT SF


#LANDSAT VI


#FINALMENTE SE TIENE UN STACK DE TODAS LAS COVARIABLES PARA CADA CORDILLERA ...








# REPROYECTAR


# latlon <- "+proj=longlat +datum=WGS84 +no_defs"
# 
# 
# list_files_covs_dem_ltlon  <- list()
# 
# for (i in 1:length(list_files_covs_dem)) {
#   list_files_covs_dem_ltlon[[i]] <- raster::projectRaster(from = list_files_covs_dem[[i]], 
#                                                           crs = latlon )
#   
# }

# MASK AL EXTENT DE CADA DEM ORIGINAL (SIN BUFFER)

alay_latlon <- st_transform(alay, crs(dem_alos_mask)) # shapeen lan lot

list_files_covs_dem_msk <- list()
for (i in 1:length(list_files_covs_dem)) {
  
  list_files_covs_dem_msk[[i]] <- terra::mask(list_files_covs_dem_ltlon[[i]], alay_latlon)
  
}

mapview(list_files_covs_dem_msk[[1]])

#STACK COVARIABLES

# dem <- terra::mask(dem_alos, alay_latlon)
covs_dem_stck <-  raster::stack(list_files_covs_dem_msk)


