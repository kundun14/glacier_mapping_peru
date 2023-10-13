pacman::p_load(rgdal, raster, terra, stars, sp, sf, tidyverse,  ggplot2)



# tiles 

tiles_list <- list.files(path = "./data/inaigem/shape_tiles/",
                                      recursive = TRUE,
                                      full.name = TRUE, # directorio relativo raster
                                      pattern = "\\.shp$")

tiles_list_sf <- lapply(tiles_list, FUN =  sf::read_sf)

# glaciares

gla <-  read_sf("./data/inaigem/glac_inaigem.gpkg") 

# filter cordilleras bajo analisis

cordilleras <- c("Blanca", "Central" ,"Huallanca", "Huayhuash",
                 "Huaytapallana", "La Raya" , "La viuda", "Raura",
                 "Urubamba","Vilcabamba" ,"Vilcanota")

gla_filt <-  gla %>% 
  dplyr::filter(Cordillera %in%  cordilleras)


#add one column (for rasterization) 1 =glacier, 0 = non glacier

gla_filt$X = 1 # añade una columna llamada X dode todo es igual a 1

# CRS

gla_filt_4326 <- st_transform(gla_filt,'EPSG:4326')

#lista glaciares por cordillera

gla_filt_4326_list <- gla_filt_4326 %>%  # lista de sf objects cordilleras 
  dplyr::group_by(Cordillera) %>% 
  group_split()


#clip : tiles_list_sf

tiles_gl_inters <- list()

for (i in 1:length(tiles_list_sf)) {

  tiles_gl_inters[[i]]  <-  st_intersection(gla_filt_4326_list[[i]], tiles_list_sf[[i]])
  
}


# sf::write_sf(tiles_gl_inters[[8]], "./test/raura_test.shp")#Huallanca

#RATERIZE


# LANDSAT BANDS
list_files_comp <- list.files(path = "./data/landsat/landsat2017_composite_glaciers/",
                              recursive = TRUE, 
                              full.name = TRUE, # directorio relativo raster
                              pattern = "\\.tif$")

grd_list <- lapply(list_files_comp, FUN =  raster::stack) 


# grd_list <- list_stack_comp # para redibilidad. mejroar codigo




gla_rasterize <- function(sf_list,grd_list ){
  
  gla_rasterize_list  <- list() # list de dem masked (bbox de cada cordillera)
  sp_list_list <- lapply(sf_list, FUN =  as, 'Spatial')
  
  for (i in 1:length(sp_list_list)) {
    
    
    
    gla_rasterize_list[[i]] <- !is.na(raster::rasterize(x = sp_list_list[[i]],
                                                 y = grd_list[[i]][[1]],
                                                 field = "X"))
    
    
  }
  
  # gla_rasterize_list <- lapply(gla_rasterize_list, FUN =  raster::raster) # volver a raster*object
  return(gla_rasterize_list)
}


w <- gla_rasterize(tiles_gl_inters,grd_list)
w


# plot(w[[8]])# raura
# raster::writeRaster(w[[8]], "./test/raura_rast_30.tif", format = "GTiff" ) # dem rarura
# 
# s <- as(tiles_gl_inters[[8]],'Spatial' )
# # x <-  !is.na(st_rasterize(raura[,3])) # sin grilla
# x <- !is.na(raster::rasterize(x = s,
#                        y = grd_list[[8]][[1]],
#                        field = "X"))
# 
# plot(x)

# WRITE

# WRITE STACKS FILES

labels <- c("Blanca",
            "Central",
            "Huallanca",
            "Huayhuasha",
            "Huaytapallana",
            "LaRaya",
            "LaViuda",
            "Raura",
            "Urubamba",
            "Vilcabamba",
            "Vilcanota"
)

for (i in 1:length(w)) {
  writeRaster(w[[i]], paste("data/inaigem/labels/labels_",labels[i],".tif" ), format='GTiff')
}



# raster::writeRaster(x, "./test/raura_rast_30_isna.tif", format = "GTiff" ) # dem rarura



#reclasify 



sp_list_list <- lapply(tiles_gl_inters, FUN =  as, 'Spatial')

tiles_gl_inters