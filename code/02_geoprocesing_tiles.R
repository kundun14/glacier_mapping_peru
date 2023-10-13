
library(tidyverse)
library(sf)
library(ggplot2)


#DATA 

gla <-  read_sf("./data/inaigem/glac_inaigem.gpkg") 
  


#cuantas coordilleras hay?

cordilleras_factor <- gla %>% as_tibble() %>% pull(Cordillera) %>% as.factor() %>% unique() # 18 cordilleras
cordilleras_chr <- cordilleras_factor %>% as.character()

# TILES


#extraer bbox de cada cordillera 
#crear tiles para cada coordillera
# hacer modelos para cada coordillera


gla_list <- gla %>%  # lista de sf objects cordilleras 
  dplyr::group_by(Cordillera) %>% 
  group_split()



## lista de glaciares por cordillera para plot + landsat images

names(gla_list) <- cordilleras_chr
gla_list


# aplucar a cada sf bbox y exportar

bbox_list <- list() # lista de bbox de cada corrdillera

for (i in 1:length(gla_list) ) {
  
bbox_list[[i]] <-  st_as_sfc(st_bbox(gla_list[[i]])) %>% st_transform('EPSG:4326') # polygon bbox de cordillera i

#proyectar # cnvertir CRS to latlon  
# nc.32119 <- st_transform(nc, 'EPSG:4326') 

}

names(bbox_list) <- cordilleras_chr
# bbox_list[[1]]

# UNION + DISOLVE


tiles_sf <- do.call("rbind", bbox_list) %>% 
  as.data.frame() %>% 
  st_as_sf(sf_column_name="V1") %>% 
  st_set_crs("EPSG:4326") %>% 
  add_column(Cordillera = cordilleras_chr)


# dissolve_sf <- st_union(single_sf)

#EXPORT BBOX

sf::write_sf(tiles_sf, "./data/inaigem/tiles.gpkg")
#exportar 1 solo poligono

# sf::write_sf(bbox_list[[18]], "./data/inaigem/shape_tiles/Vilcanota_tile.shp")#Vilcanota
# sf::write_sf(bbox_list[[3]], "./data/inaigem/shape_tiles/Blanca_tile.shp")#blanca
# sf::write_sf(bbox_list[[14]], "./data/inaigem/shape_tiles/LaViuda_tile.shp")#La viuda
# sf::write_sf(bbox_list[[5]], "./data/inaigem/shape_tiles/Central_tile.shp")#Central
# sf::write_sf(bbox_list[[12]], "./data/inaigem/shape_tiles/Huaytapallana_tile.shp")#Huaytapallana
# sf::write_sf(bbox_list[[17]], "./data/inaigem/shape_tiles/Vilcabamba_tile.shp")#Vilcabamba
# sf::write_sf(bbox_list[[16]], "./data/inaigem/shape_tiles/Urubamba_tile.shp")#Urubamba
# sf::write_sf(bbox_list[[13]], "./data/inaigem/shape_tiles/LaRaya_tile.shp")#La Raya
# sf::write_sf(bbox_list[[18]], "./data/inaigem/shape_tiles/Vilcanota_tile.shp")#Vilcanota
# 
# sf::write_sf(bbox_list[[11]], "./data/inaigem/shape_tiles/Huayhuasha_tile.shp")#Huayhuash
# sf::write_sf(bbox_list[[15]], "./data/inaigem/shape_tiles/Raura_tile.shp")#Raura
# sf::write_sf(bbox_list[[9]], "./data/inaigem/shape_tiles/Huallanca_tile.shp")#Huallanca


#WRITE SHAPES CORRDILERAS

gla_list_4326<- list() # lista de bbox de cada corrdillera

for (i in 1:length(gla_list) ) {
  
  gla_list_4326[[i]] <-  st_as_sfc(gla_list[[i]]) %>% st_transform('EPSG:4326') # polygon bbox de cordillera i
  
}
names(gla_list_4326) <- cordilleras_chr
# gla_list_4326[[3]] %>% plot()


# sf::write_sf(gla_list_4326[[3]], "./data/inaigem/shape_cordilleras/Blanca.shp")#blanca
# sf::write_sf(gla_list_4326[[14]], "./data/inaigem/shape_cordilleras/LaViuda.shp")#La viuda
# sf::write_sf(gla_list_4326[[5]], "./data/inaigem/shape_cordilleras/Central.shp")#Central
# sf::write_sf(gla_list_4326[[12]], "./data/inaigem/shape_cordilleras/Huaytapallana.shp")#Huaytapallana
# sf::write_sf(gla_list_4326[[17]], "./data/inaigem/shape_cordilleras/Vilcabamba.shp")#Vilcabamba
# sf::write_sf(gla_list_4326[[16]], "./data/inaigem/shape_cordilleras/Urubamba.shp")#Urubamba
# sf::write_sf(gla_list_4326[[13]], "./data/inaigem/shape_cordilleras/LaRaya.shp")#La Raya
# sf::write_sf(gla_list_4326[[18]], "./data/inaigem/shape_cordilleras/Vilcanota.shp")#Vilcanota
# sf::write_sf(gla_list_4326[[11]], "./data/inaigem/shape_cordilleras/Huayhuasha.shp")#Huayhuash
# sf::write_sf(gla_list_4326[[15]], "./data/inaigem/shape_cordilleras/Raura.shp")#Raura
# sf::write_sf(gla_list_4326[[9]], "./data/inaigem/shape_cordilleras/Huallanca.shp")#Huallanca

#PLOTS

# IMPORT LANDAST
#iterar para cada bbox - hacer facet

#plot 18 cordilleras por separado

# library(tmap)
# 
# tm_shape(gla) +
#   tm_borders() +
#   tm_facets(by = "Cordillera")

#mejorar plor, poniendo tile, dolor facet etc.

#cow plot
# 
# library(cowplot)
# 
# g <- purrr::map(gla$Cordillera,
#                 function(x) {
#                   ggplot() +
#                     geom_sf(data = filter(gla, Cordillera == x)) +
#                     guides(fill = FALSE) +
#                     ggtitle(x)
#                 })
# 
# g2 <- cowplot::plot_grid(plotlist = g)


Blanca_sf <- sf::read_sf("./data/inaigem/shape_cordilleras/Blanca.shp")#blanca
l78composite_Blanca_tile <- raster::stack("./data/landsat/l78composite_Blanca_tile.tif")





#PASOS

#primero dividir los tiles segun cordilleras OK
# crear un sf objetc para cada coordillera  OK
# descargar lansaat images para cad tile (earth engine)  OK
# plotear tiles con polgons para papper

#https://medium.com/@tobias.stalder.geo/plot-rgb-satellite-imagery-in-true-color-with-ggplot2-in-r-10bdb0e4dd1f


#usar lansat images para posterior modelamiento

# derivar indices spectrales OK
# derivar parermtros geomorfometricos OK
# muestrear (hyper cube)
# constuir dataframe ( balanceado) para cada coordillera
# modelameinto( clasificacion)

#bbox 

nc.32119 %>% 
  select(BIR74) %>% 
  plot(graticule = TRUE, axes = TRUE)