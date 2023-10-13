
#las labels estaban bien antes de stackear,
#ver,  D:\DOCS\CIENCIA DE SUELOS\R_SUELOS\glacier_mlMODIS\data\inaigem\labels
# el problema era el resmping con bilinear en vez de ngb

pacman::p_load(rgdal, raster, terra, stars, sp, sf, tidyverse,  ggplot2)



#LABELS



list_files_labels <- list.files(path = "data/inaigem/labels/",
                              recursive = TRUE, 
                              full.name = TRUE, # directorio relativo raster
                              pattern = "\\.tif$")

list_labels <- lapply(list_files_labels, FUN =  raster::raster) 

# writeRaster(list_labels[[8]], "./test/raura_CLASS_og_prestack.tif") # no es 1 a 0



#stack dem +saga +landsat +bandas+ indices


# LANDSAT BANDS
list_files_comp <- list.files(path = "./data/landsat/landsat2017_composite_glaciers/",
                              recursive = TRUE, 
                              full.name = TRUE, # directorio relativo raster
                              pattern = "\\.tif$")

list_stack_comp <- lapply(list_files_comp, FUN =  raster::stack) 

# LANDSAT INDEXS

list_files_index <- list.files(path = "./data/landsat/indices/",
                               recursive = TRUE, 
                               full.name = TRUE, # directorio relativo raster
                               pattern = "\\.grd$")

list_stack_index <- lapply(list_files_index, FUN =  raster::stack) 


##################3
#PRUEBA STACK


x <- list()
for (i in 1:length(list_labels)) {
  
  x[[i]]  <- raster::stack(list_labels[[i]],list_stack_comp[[i]],list_stack_index[[i]] )
  
}

x # labels + landsat covs


#DEM

# list_files_dem <- list.files(path = "./data/dem/dems/",
#                              recursive = TRUE,
#                              full.name = TRUE, # directorio relativo raster
#                              pattern = "\\.tif$")
# 
# dems_list <- lapply(list_files_dem, FUN =  raster::raster)


#SAGA COVS + DEM

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

#CRS SAGA UTM to EPSG:4326

EPSG4326_proj  <- "+proj=longlat +datum=WGS84 +no_defs"

saga_list<- list()
for (i in 1:length(saga_list_stacks)) {
  
  saga_list[[i]]  <- raster::projectRaster(saga_list_stacks[[i]],
                                           crs = EPSG4326_proj)
  
}

y <- saga_list # y???

#aqui es donde se pudre todo
# usar 

# RESAMPLING 
# LANDSAT COVS = LABLES < COV SAGA < DEM

# CROP AND STACK

# LANDSAT COVS = LABLES < COV SAGA < DEM

#crop dem saga covs y  usando landsat x extent

z <- list()
for (i in 1:length(y)) {
  
  z[[i]]  <- raster::crop(y[[i]],
                           x[[i]])
  
}

z # nueva y 

memory.size()
memory.limit()
memory.limit(size=56000)


# REVISAR EN QGIS
#AMBOS QUEDAN DENTRO DE LOS TILES PERO TIENE DIFERENTE EXTENT


# RESAMPLING 

# w <- lapply(x, FUN =  raster::resample, z) # raster ed referencia del dem

# for loop option

w <- list() # nueva x
for (i in 1:length(x)) {

  w[[i]]  <- raster::resample(x[[i]],
                              z[[i]],
                              method="ngb")
  # el remuestreo usando el metodo bilinear modifica el rango original de las layer
  # de clases para cada cordillera , hau que usar nearest neuoborst

}



#stacking

a <- list() # error, debi adjuntar a a no a w
for (i in 1:length(z)) {
  
  a[[i]]  <- raster::stack(w[[i]],z[[i]]) # unir landsat covs + dem covs
  
}


# a[[1]] # todas las covariables + labels

names(a[[1]])

# COVS NAMES

covs_names <- c( "CLASS","BLUE", "GREEN",  "RED",   "NIR", "SWIR1",  "SWIR2",
                "NDFI", "ndsi" , "ndvi", "NDWI", "NDWIns", "NDSInw" ,
                 "nbr2", "VNSIR", "NDMI", "TCG", 
                "ASPECT","CROSC","DEM","LONGC","MAXIC","MINIC",
                "PLANC","PROFC","SLOPE")


for (i in 1:length(a)) {
  
  names(a[[i]]) <- covs_names 
  
}

#nombre para cade elemto de la lista ed stacks segun la cordillera de estudio

# lits_names <- c("Blanca",
#             "Central",
#             "Huallanca",
#             "Huayhuasha",
#             "Huaytapallana",
#             "LaRaya",
#             "LaViuda",
#             "Raura",
#             "Urubamba",
#             "Vilcabamba",
#             "Vilcanota"
# )

lits_names <- c("Blanca",
                "Central",
                "Huallanca",
                "Huayhuasha",
                "Huaytapallana",
                "LaRaya",
                "LaViuda",
                "Raura",
                "Urubamba",
                "Vilcabamba"
)

names(a) <- lits_names

#write
writeRaster(a[[8]]$CLASS, "./test/raura_CLASS_ngb.tif") # no es 1 a 0
writeRaster(a[[10]]$CLASS, "./test/vilcabamba_CLASS_ngb.tif") # no hay stack 11, el 10 es vilcababda
writeRaster(a[[10]]$SLOPE, "./test/vilcabamba_SLOPE_ngb.tif") # no hay stack 11, el 10 es vilcababda


saveRDS(a, "./data/harmonized_covs/harmonized_data.rds") 


###############################3
vilcanota

# w <- list() # nueva x
# for (i in 1:length(x)) {
#   
#   w[[i]]  <- raster::resample(x[[i]],
#                               z[[i]],
#                               method="ngb")
#   # el remuestreo usando el metodo bilinear modifica el rango original de las layer
#   # de clases para cada cordillera , hau que usar nearest neuoborst
#   
# }


w_vilcanota <- raster::resample(x[[11]],
                 z[[11]],
                 method="ngb")


# a <- list() # error, debi adjuntar a a no a w
# for (i in 1:length(z)) {
#   
#   a[[i]]  <- raster::stack(w[[i]],z[[i]]) # unir landsat covs + dem covs
#   
# }

a_vilcanota  <- raster::stack(w_vilcanota,z[[11]])


# names(a[[1]])

# COVS NAMES

covs_names <- c( "CLASS","BLUE", "GREEN",  "RED",   "NIR", "SWIR1",  "SWIR2",
                 "NDFI", "ndsi" , "ndvi", "NDWI", "NDWIns", "NDSInw" ,
                 "nbr2", "VNSIR", "NDMI", "TCG", 
                 "ASPECT","CROSC","DEM","LONGC","MAXIC","MINIC",
                 "PLANC","PROFC","SLOPE")



names(a_vilcanota) <- covs_names 
# names(a) <- lits_names
#adjuntar a las demas cordilleras en a

b <- list(Vilcanota = a_vilcanota)
a_ <- append(a, b, after = 10) # todas las cordilleras.

saveRDS(a_, "./data/harmonized_covs/harmonized_data.rds") 


#prueba 

writeRaster(a_[[8]]$CLASS, "./test/a_raura_CLASS_ngb.tif") # no es 1 a 0
writeRaster(a_[[10]]$CLASS, "./test/a_vilcabamba_CLASS_ngb.tif") # no hay stack 11, el 10 es vilcababda
writeRaster(a_[[1]]$CLASS, "./test/a_blanca_CLASSE_ngb.tif") # no hay stack 11, el 10 es vilcababda

#prueba raura

writeRaster(a_[[8]]$CLASS, "./test/a_raura_CLASS_ngb.tif") # no es 1 a 0
writeRaster(a_[[8]]$ndvi, "./test/a_raura_ndvi_ngb.tif") # no hay stack 11, el 10 es vilcababda
writeRaster(a_[[8]]$SLOPE, "./test/a_raura_SLOPE_ngb.tif") # no hay stack 11, el 10 es vilcababda
writeRaster(a_[[8]]$BLUE, "./test/a_raura_BLUE_ngb.tif") # no hay stack 11, el 10 es vilcababda
writeRaster(a_[[8]]$ndsi, "./test/a_raura_ndsi_ngb.tif") # no hay stack 11, el 10 es vilcababda
writeRaster(x[[8]][[9]], "./test/a_raura_ndsi_og.tif") # no hay stack 11, el 10 es vilcababda

writeRaster(list_stack_comp[[8]]$Blue, "./test/a_raura_blue_pre.tif") # no hay stack 11, el 10 es vilcababda

list_stack_comp[[8]]$Blue

writeRaster(x[[8]]$Blue, "./test/a_raura_BLUE_prengb.tif") # no hay stack 11, el 10 es vilcababda



#benchmark terra vs raster

# 
# library(raster)
# library(terra)
# # install.packages("rbenchmark")
# library(rbenchmark)
# library(tictoc)
# 
# 
# #raster**
# 
# x0 <- x[[8]]$Blue
# zo <- x[[8]]$Blue
# 
# 
# tic("raster resmapling")  
# res_terra <- raster::resample(x0,zo,method="ngb")
# toc()
# 
# 
# 
# #terra
# install.packages('raster', repos='https://rspatial.r-universe.dev')
# 
# x00 <- terra::rast(x[[8]], lyrs = "Blue", nlyrs = 1)
# z00 <-  terra::rast(x[[8]], lyrs = "Blue", nlyrs = 1)
# 
# tic("terra resampling")
# res_raster <- terra::resample(x00,z00,method="ngb")
# toc()

#SAVE
#comprobar
# raster::writeRaster(w[[8]][[5]], "./test/raura_nirW.tif", format = "GTiff" ) # dem rarura
# raster::writeRaster(w[[8]][[18]], "./test/raura_demW.tif", format = "GTiff" ) #  SLOPE rarura
raster::writeRaster(a[[8]][[26]], "./test/raura_slope_final.tif", format = "GTiff" ) #  SLOPE rarura
writeRaster(a[[8]]$CLASS, "./test/raura_CLASS_ngb.tif") # no es 1 a 0




rm(list=setdiff(ls(), "a"))

memory.size()
memory.limit()
memory.limit(size=56000)

saveRDS(a, "./data/harmonized_covs/harmonized_data.rds") 


# aa <- readRDS("./data/harmonized_covs/harmonized_data.rds")

