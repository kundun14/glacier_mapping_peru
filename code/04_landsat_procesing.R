# library(tidyverse) #Loads ggplot2 as well
# library(raster)    #Raster operations
library(here)      #Path management
library(prismatic) #Approximation to hexcode colors in console
# library(sf)
# library(terra)
library(terrainr)
# library(ggplot2)

# spectral indices...

# require(pacman)
pacman::p_load(rgdal, raster, terra, sp, sf, dplyr, lubridate, mapview, ggplot2)

# LOAD DATA 

#POLIGONOS

gla <-  read_sf("./data/inaigem/glac_inaigem.gpkg") 
cordilleras_factor <- gla %>% as_tibble() %>% pull(Cordillera) %>% as.factor() %>% unique() # 18 cordilleras
cordilleras_chr <- cordilleras_factor %>% as.character() # nombres de todas las coordilleras
cordilleras_chr


# IMAGENES LANDSAT

list.files <- list.files(path = "./data/landsat/landsat2017_composite_glaciers/",
                         recursive = TRUE, 
                         full.name = TRUE, # directorio relativo raster
                         pattern = "\\.tif$")

list.files.stack <- lapply(list.files, FUN =  raster::stack) 
# list.files.stack[1:5]

#SOLO ALGUNAS COORDILLERAS PARA ANALISIS TIENEN IMAGENES LANDSAT

landsat_cordilleras_chr <- c("l78composite_Blanca_tile",
                             "l78composite_Central_tile",
                             "l78composite_Huallanca_tile",
                             "l78composite_Huayhuasha_tile",
                             "l78composite_Huaytapallana_tile",
                             "l78composite_LaRaya_tile",
                             "l78composite_LaViuda_tile",
                             "l78composite_Raura_tile",
                             "l78composite_Urubamba_tile",
                             "l78composite_Vilcabamba_tile",
                             "l78composite_Vilcanota_tile"
                             )

landsat_cordilleras_chr

# ELIMINAR BNDA QA

# list.files.stack_ <- list()
# for (j in 1:length(list.files.stack)) {
#   
#   list.files.stack_[[j]] <-  dropLayer(list.files.stack[[j]], 7) 
#   
# }

#list.files.stack_

# #PLOT RGB
# 
# viewRGB(
#   list.files.stack_[[20]],
#   r = 3,
#   g = 2,
#   b = 1,
#   quantiles = c(0.02, 0.98))


# INDICES

## MASK A LA BUFFER DE LA CUENCA
# ahrrar memoria y tiempo de computo y evitar probllemas de diferencias de extentes entres rasters 

# buffer <- sf::st_read("./buffer/buffer.shp") 
# buffer <- as(buffer$geometry, "Spatial")
# 
# list.files.stack <- list()
# for (i in 1:length(files.stack)) {
#   
#   list.files.stack[[i]] <- terra::mask(files.stack[[i]], buffer )
#   
# }
# 
# mapview(list.files.stack[[20]][[1]]) # probar mascara



# NDFI RED SWIR

list.files.stack_NDFI <- list()
for (j in 1:length(list.files.stack)) {
  
  list.files.stack_NDFI[[j]] <-  (list.files.stack[[j]]$Red - list.files.stack[[j]]$SWIR1)/(list.files.stack[[j]]$Red + list.files.stack[[j]]$SWIR1) 
  
}

## NDSI      GREEn SWIR                                 
list.files.stack_ndsi <- list()
for (j in 1:length(list.files.stack)) {
  
  list.files.stack_ndsi[[j]] <-  (list.files.stack[[j]]$Green - list.files.stack[[j]]$SWIR1)/(list.files.stack[[j]]$Green + list.files.stack[[j]]$SWIR1) 
  
}

##NDVI
list.files.stack_ndvi <- list()
for (j in 1:length(list.files.stack)) {
  
  list.files.stack_ndvi[[j]] <-  (list.files.stack[[j]]$NIR - list.files.stack[[j]]$Red)/(list.files.stack[[j]]$NIR + list.files.stack[[j]]$Red) 
  
}

# NDWI green NIR

list.files.stack_NDWI <- list()
a <- 2
for (j in 1:length(list.files.stack)) {
  
  list.files.stack_NDWI[[j]] <-  (list.files.stack[[j]]$Green -  list.files.stack[[j]]$NIR)/(list.files.stack[[j]]$Green + list.files.stack[[j]]$NIR) 
  
}



# NDWIns = (ρGreen − a × ρNIR)/(ρGreen + ρNIR)  a =2
list.files.stack_NDWIns <- list()
a <- 2
for (j in 1:length(list.files.stack)) {
  
  list.files.stack_NDWIns[[j]] <-  (list.files.stack[[j]]$Green - a * list.files.stack[[j]]$NIR)/(list.files.stack[[j]]$Green + list.files.stack[[j]]$NIR) 
  
}


#  NDSInw = (ρNIR − ρSWIR1 − b)/(ρNIR + ρSWIR1) ; b  = 0.05
list.files.stack_NDSInw <- list()
b <- 0.05
for (j in 1:length(list.files.stack)) {
  
  list.files.stack_NDSInw[[j]] <-  (list.files.stack[[j]]$NIR -  list.files.stack[[j]]$SWIR1 - b)/(list.files.stack[[j]]$NIR + list.files.stack[[j]]$SWIR1) 
  
}

# nbr2 ["SWIR1","SWIR2"

list.files.stack_nbr2 <- list()
for (j in 1:length(list.files.stack)) {
  
  list.files.stack_nbr2[[j]] <-  (list.files.stack[[j]]$SWIR1 - list.files.stack[[j]]$SWIR2)/(list.files.stack[[j]]$SWIR1 + list.files.stack[[j]]$SWIR2) 
  
}


# VNSIR   1 - ((2 * Red - Green - Blue) + 3 (SWIR2 - NIR)) 

list.files.stack_VNSIR <- list()
for (j in 1:length(list.files.stack)) {
  
  list.files.stack_VNSIR[[j]] <- 1 - ((2* list.files.stack[[j]]$Red - list.files.stack[[j]]$Green - list.files.stack[[j]]$Blue ) +
                                        3 *(list.files.stack[[j]]$SWIR1 + list.files.stack[[j]]$SWIR2)) 
  
}

#Normalized Difference Moisture Index (NDMI)	

list.files.stack_NDMI <- list()
for (j in 1:length(list.files.stack)) {
  
  list.files.stack_NDMI[[j]] <-  (list.files.stack[[j]]$NIR - list.files.stack[[j]]$SWIR2)/(list.files.stack[[j]]$NIR + list.files.stack[[j]]$SWIR2) 
  
}


# TCG Blue * (-0.2941) + Green *(-0.243) + Red * (-0.5424) + NIR * 0.7276 + SWIR1 * 0.0713 + SWIR2 * (-0.1608)

list.files.stack_TCG <- list()
for (j in 1:length(list.files.stack)) {
  
  list.files.stack_TCG[[j]] <-  (list.files.stack[[j]]$Blue * (-0.2941)  +
                                   list.files.stack[[j]]$Green * (-0.243) + 
                                   list.files.stack[[j]]$Red * (-0.5424) +
                                   list.files.stack[[j]]$NIR * 0.7276 +
                                   list.files.stack[[j]]$SWIR1 * 0.0713 +
                                   list.files.stack[[j]]$SWIR2 * (-0.1608))
  
  
}


# STACK DE CADA RASTER POR CORDILLERA

# UN STACK DE REFLECTANCIAS + INDICES PARA CORDILLERA BLANCA
# UN STACK DE REFLECTANCIAS + INDICES PARA CORDILLERA BLANCA



# i = 1 es cordillera blanca para cada stack


list.files.stack <- list() # lista de stacks para cada corddillera

for (i in 1:length(list.files.stack_NDFI)) {
  
list.files.stack[[i]] <-   raster::stack(list.files.stack_NDFI[[i]],
                list.files.stack_ndsi[[i]],
                list.files.stack_ndvi[[i]],
                list.files.stack_NDWI[[i]],
                list.files.stack_NDWIns[[i]],
                list.files.stack_NDSInw[[i]],
                list.files.stack_nbr2[[i]],
                list.files.stack_VNSIR[[i]],
                list.files.stack_NDMI[[i]],
                list.files.stack_TCG[[i]])

}


# vi_blanca <- raster::stack()
# 
# list.files.stack_NDFI[[i]]
# list.files.stack_ndsi[[i]]
# list.files.stack_ndvi[[i]]
# list.files.stack_NDWI[[i]]
# list.files.stack_NDWIns[[i]]
# list.files.stack_NDSInw[[i]]
# list.files.stack_nbr2[[i]]
# list.files.stack_VNSIR[[i]]
# list.files.stack_NDMI[[i]]
# list.files.stack_TCG[[i]]



# PLOT

# mapview(list.files.stack_NDMI[[20]])

# JUNTAR lista lemente wise, cd elemnto con los respectivos elemntos de ls otrs lists
# covertir cada stck de un ño  un dtfraame
# en total serian 37 datafrmes
# hcaer un datafrme aanidado

list_stck <- list()
for (i in 1:length(list.files.stack_NDFI)) {
  list_stck[[i]] <- raster::stack(list.files.stack_NDFI[[i]],
                                  list.files.stack_ndsi[[i]],
                                  list.files.stack_ndvi[[i]],
                                  list.files.stack_NDWI[[i]],
                                  list.files.stack_NDWIns[[i]],
                                  list.files.stack_NDSInw[[i]],
                                  list.files.stack_nbr2[[i]],
                                  list.files.stack_VNSIR[[i]],
                                  list.files.stack_NDMI[[i]],
                                  list.files.stack_TCG[[i]])
}

#se elmina el nombre de cada layer (covariable)
list_stck[[1]]

names(list_stck) <- landsat_cordilleras_chr # adjuntar nombre a cada stack
list_stck

#   EXPORT INDICES PARA CADA CORDILLERA BAJO ANALISIS



for (i in 1:length(list_stck)) {
  
  raster::writeRaster(list_stck[[i]],
                      filename = paste0("./data/landsat/indices/index-", landsat_cordilleras_chr[i], ".grd"), 
                      format = "raster")
  
}


# READ

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




# MASK LANDSAT COVS TO DEM COVS (TILES) 

# TILES

tiles_list <- list.files(path = "./data/inaigem/shape_tiles/",
                         recursive = TRUE, 
                         full.name = TRUE, # directorio relativo raster
                         pattern = "\\.shp$")

tiles_list_sf <- lapply(tiles_list, FUN =  sf::read_sf) 

#MASK COMPOSITES

# landsat_mask <- function(list_stack_comp,tiles_list_sf){
#   
#   # saga_list_stacks : lista de raster*stacks objects para cada cordillera
#   # tiles_list_sf : lista de sf objects
#   
#   landsatcom_tiled <- list() # list de dem masked (bbox de cada cordillera)
#   list_stack_comp <- lapply(list_stack_comp, FUN =  terra::rast) #convertir previamente de raster* to spatRas para  funcion de terra::mask
#   tiles_list_sf <- lapply(tiles_list_sf, FUN =  terra::vect) ####convertir previamente de sf to spatVector para  funcion de terra::mask
#   
#   for (i in 1:length(list_stack_comp)) {
#     
#     
#     landsatcom_tiled[[i]] <- terra::mask(list_stack_comp[[i]],
#                                       tiles_list_sf[[i]])
#     
#   }
#   
#   landsatcom_tiled <- lapply(landsatcom_tiled, FUN =  raster::stack) # volver a raster*object
#   return(landsatcom_tiled)
# }
# 
# 
# landsatcom_mask <- landsat_mask(list_stack_comp,tiles_list_sf) # lista de dems masked
# 
# 
# #MASK INDEXS
# 
# landsat_index_mask <- function(list_stack_index,tiles_list_sf){
#   
#   # saga_list_stacks : lista de raster*stacks objects para cada cordillera
#   # tiles_list_sf : lista de sf objects
#   
#   landsatcom_index_tiled <- list() # list de dem masked (bbox de cada cordillera)
#   list_stack_index <- lapply(list_stack_index, FUN =  terra::rast) #convertir previamente de raster* to spatRas para  funcion de terra::mask
#   tiles_list_sf <- lapply(tiles_list_sf, FUN =  terra::vect) ####convertir previamente de sf to spatVector para  funcion de terra::mask
#   
#   for (i in 1:length(list_stack_index)) {
#     
#     
#     landsatcom_index_tiled[[i]] <- terra::mask(list_stack_index[[i]],
#                                                tiles_list_sf[[i]])
#     
#   }
#   
#   landsatcom_index_tiled <- lapply(landsatcom_index_tiled, FUN =  raster::stack) # volver a raster*object
#   return(landsatcom_index_tiled)
# }
# 
# 
# index_mask <- landsat_index_mask(list_stack_index,tiles_list_sf) # lista de dems masked
# 
# 

#stacking

landsatcom_index_stack <- list()
for (i in 1:length(landsatcom_mask)) {

  landsatcom_index_stack[[i]]  <- raster::stack(landsatcom_mask[[i]],
                                                index_mask[[i]])

}


# COVS NAMES

landsat_names_covs <- c(  "BLUE", "GREEN",  "RED",   "NIR", "SWIR1",  "SWIR2",
                          "NDFI", "ndsi" , "ndvi", "NDWI", "NDWIns", "NDSInw" , "nbr2", "VNSIR", "NDMI", "TCG" )


for (i in 1:length(landsatcom_index_stack)) {
  
  names(landsatcom_index_stack[[i]]) <- landsat_names_covs 
  
}

#nombre para cade elemto de la lista ed stacks segun la cordillera de estudio

cordilleras_chr <- c("Blanca_landsatcom_index__tiled",
                     "Central_landsatcom_index__tiled",
                     "Huallanca_landsatcom_index__tiled",
                     "Huayhuasha_landsatcom_index__tiled",
                     "Huaytapallana_landsatcom_index__tiled",
                     "LaRaya_landsatcom_index__tiled",
                     "LaViuda_landsatcom_index__tiled",
                     "Raura_landsatcom_index__tiled",
                     "Urubamba_landsatcom_index__tiled",
                     "Vilcabamba_landsatcom_index__tiled",
                     "Vilcanota_landsatcom_index__tiled"
)

names(landsatcom_index_stack) <- cordilleras_chr


# ya se puede stackerar con las covs de saga???


# landsatcom_index_dem_saga_tiled <- list()
# for (i in 1:length(landsatcom_index_stack)) {
#   
#   landsatcom_index_dem_saga_tiled[[i]]  <- raster::stack(landsatcom_index_stack[[i]],
#                                                          dem_saga_tiled[[i]])
#   
# }


# #write prueba
# raster::write (saga_mask_list[[8]], "./raura_saga_masked.tif", format = "GTif" ) # scribir rarura


# # WRITE LISTA DE STACKS

saveRDS(landsatcom_index_stack, "./data/landsat/landsatcomp_index_tiled/landsatcomp_index_tiled.rds") 
# saveRDS(list_stck, "./data/landsat/indices/lista_stack_indices.rds") # INDICES



#expandir memoria de ser el caso
memory.size()
memory.limit()
memory.limit(size=56000)

# prueba lectura

# stack_index_l78composite_Blanca = raster::stack("./data/landsat/indices/index-l78composite_Blanca_tile.grd") # leer stack desde folder
# 
# stack_index_l78composite_Blanca


# COMPROBAR UN RASTER
#SIEMPRE PROBAR PRIMERO CON CORDILLERA RAURA, ES PEQUEÑA

index_l78composite_Raura_tile <-  raster::stack("./data/landsat/indices/index_l78composite_Raura_tile.grd")

writeRaster(index_l78composite_Raura_tile[[1]],
            filename = "./data/landsat/indices/index_l78composite_Raura_tile_layer1.tif",
            format = "GTiff",
            overwrite= T)



raster::writeFormats()

# PLOT RGB

# l78composite_Raura_tile <-  raster::stack("./data/landsat/landsat2017_composite_glaciers/l78composite_Raura_tile.tif")
# 
# l78composite_Raura_tile_tib <- tabularaster::as_tibble(l78composite_Raura_tile, cell = TRUE, dim = TRUE ,xy = TRUE)  %>%
#   na.omit() # %>% dplyr::rename(DWSSM = cellvalue)# stack to dataframe longer
# 
# l78composite_Raura_tile_tib$dimindex <- as.factor(l78composite_Raura_tile_tib$dimindex) # dimmindex rerpesetna la fecha de cada raster 
# levels(l78composite_Raura_tile_tib$dimindex) <- c("Blue" , "Green" ,"Red"  , "NIR"  , "SWIR1" ,"SWIR2") # cmambiar los niveles 1 , 2 , 3 por lasfechas "2021-05-14" "2021-05-15" "2021-05-16"
# 
# 
# ggplot(dws_tib_1_20, aes(x, y)) +
#   coord_equal() +
#   theme_void() +
#   geom_raster(aes(fill = DWSSM)) +
#   ggtitle("rgb \n") +
#   theme(plot.title = element_text(hjust = 0.5, size = 11))
# 
# 
# # library("terrainr")
# # library("ggplot2")
# install.packages("RStoolbox")
# library(devtools)
# install_github("bleutner/RStoolbox")
# 
# # library("RStoolbox")
# 
# library(raster)
# library(ggplot2)
# library(RStoolbox)
# 
# 
# ggplot(data = df_, aes(x = x, y =y)) +
#   geom_raster(fill = rgb(r = df_$Red,
#                          g = df_$Green,
#                          b = df_$Blue,
#                          maxColorValue = 255),
#               show.legend = FALSE) +
#   scale_fill_identity() +
#   ggtitle("Plot .tif rgb")
# 
# 
# #+
#   geom_sf(data = simulated_data) +
#   coord_sf(crs = 4326)

# SI HACEMOS ANALISIS GEOMORFOLOGICO PARACONSIDERAR GEOMORFONES Y DISTRIBUCION DE VEGETACION Y CUERPOS DE AGUA
# JOINT INDICES CON GEOMORPH

# list_index_mask # lista de stacks (indices multi) para cada año enmascadardos a la cuenca
# covs_dem_stck # stck de covriables estatico
# 
# 
# n <- length(list_index_mask) 
# covs_dem_stck_list <- rep(list(covs_dem_stck), n) # lista de n stacks (todos iguales, variables estaticas)
# 
# covs_dem_stck_list # stack de covs geomorfologics para cada ano

# HARMONIZAR

#  resample()

# list_index_resampled <- list()
# for (i in 1:39) {
#   list_index_resampled[[i]] <- raster::resample(list_index_mask[[i]],
#                                              covs_dem_stck_list[[i]])
# }



# STACKEAR GEOMORFONES

# list_stack_index_geo <- list()
# for (i in 1:39) {
#   list_stack_index_geo[[i]] <- raster::stack(list_index_resampled[[i]],
#                                   covs_dem_stck_list[[i]])
# }


# STACKEAR  INDICES EN UNA LISTA POR AÑO (1984...) #MODIFICAR POR CORDILLERA

# list_stack_index_año <- list()
# for (i in 1:39) {
#   list_stack_index_año[[i]] <- raster::stack(list_stck[[i]])
# }
# 
# # WRITE LISTA DE STACKS
# saveRDS(list_stack_index_año, "lista_indices.rds")
# 
# # WRITE STACKS FILES
# 
# imagenes_nombres = paste(rep("imagen"), 1984:2022, sep = "_" )
# for (i in 1:length(list_stck)) {
#   writeRaster(list_stack_index_año[[i]], imagenes_nombres[i], format='GTiff')
# }
# 
# # prueba <- raster::stack("./stacks_covs/imagen_1984.tif")
# 
# 
# 
# names_inde <- c(  "NDFI", "ndsi" , "ndvi", "NDWI", "NDWIns", "NDSInw" , "nbr2", "VNSIR", "NDMI", "TCG" )
# names_geo <- c ("Convergence_Index"  ,        "Downslope_Curvature"    ,    "Flow_Width"   ,             
#                 "Flow_Accumulation" ,         "Generalized_Surface" ,       "Local_Curvature"  ,         
#                 "Longitudinal_Curvature" ,    "Morphometric_Features"  ,    "MRRTF" ,                    
#                 "MRVBF" ,                     "Plan_Curvature",             "Profile_Curvature",         
#                 "Slope",                      "Topographic_Position_Index" ,"twi",                       
#                 "Upslope_Curvature")

# names(prueba) <- names
# prueba$ndvi
# mapview(prueba$ndvi)


# extract y dataframe construcction




###################################################

#plot

# Blanca_sf <- sf::read_sf("./data/inaigem/shape_cordilleras/Blanca.shp")#blanca
# l78composite_Blanca_tile <- raster::stack("./data/landsat/l78composite_Blanca_tile.tif")
# l78composite_Blanca_tile <- terra::rast("./data/landsat/l78composite_Blanca_tile.tif")

# l78composite_Blanca_tile[[1:3]]

# l78composite_Blanca_tile <- raster::stack("./data/landsat/landsat2017_composite_glaciers/l78composite_Blanca_tile.tif")
# 
# mb_ras <- stretch(x=l78composite_Blanca_tile, minv=0, maxv=255)
# df <- as.data.frame(mb_ras, xy= TRUE)
# # mb_df[!complete.cases(mb_df),] #Returns zero rows, no pixel is lacking any data
# mb_df <-  df %>% na.omit()
# 
# # head(df) 
# 
# # Blue, Green, Red layer 1, layer 2 layer 3
# 
# ggplot(data=mb_df, aes(x=x, y=y, fill=rgb(layer.3,layer.2,layer.1, maxColorValue = 255))) + 
#   coord_equal() + theme_bw() + geom_tile() + scale_fill_identity() + 
#   
#   scale_x_continuous(breaks=range(mb_df$x)*c(1.01, 0.99), labels=range(mb_df$x), expand = c(0,0)) +
#   scale_y_continuous(breaks=range(mb_df$y)*c(0.99, 1.01), labels=range(mb_df$y), expand = c(0,0)) +
#   
#   theme(panel.grid=element_blank(), plot.title = element_text(size = 10))
  
  
  
# prismatic::color(rgb(r = Red,              #Specify Bands
#                      g = Green,
#                      b = Blue,
#                      maxColorValue = 255)[1:200]) #subsample

#plot

df_ <- df %>% na.omit()


# ggplot(data = df_, aes(x = x, y =y)) + 
#   geom_raster(fill = rgb(r = df_$Red,
#                          g = df_$Green,
#                          b = df_$Blue,
#                          maxColorValue = 255),
#               show.legend = FALSE) +  
#   scale_fill_identity() + 
#   ggtitle("Plot .tif rgb")


# ggplot() +
#   geom_spatial_rgb(
#     data = l78composite_Blanca_tile[[1:3]],
#     mapping = aes(
#       x = x,
#       y = y,
#       r = Red,
#       g = Green,
#       b = Blue
#     )
#   ) +
#   geom_sf(data = Blanca_sf) +
#   coord_sf(crs = 4326)



# tab <- as.data.frame(im, xy = TRUE)
# df_ <- df %>% dplyr::select(c("x","y","Blue","Green","Red"))
# names(df_) <- c("x", "y", "red", "green", "blue")
# # df_$hex <- rgb(df_$red, df_$green, df_$blue, maxColorValue = 1)
# 
# g <- ggplot(df_, aes(x, y, fill = hex)) + 
#   geom_raster() + 
#   coord_equal()  +  
#   scale_fill_identity()

# g + geom_sf(data = Blanca_sf, colour = "red", fill = "red") 


# ggplot() +
#   geom_spatial_rgb(
#     data = df_,
#     mapping = aes(
#       x = x,
#       y = y,
#       r = red,
#       g = green,
#       b = blue
#     )
#   ) +
#   geom_sf(data = simulated_data) +
#   coord_sf(crs = 4326)
