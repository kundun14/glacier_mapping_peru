pacman::p_load(sf, sp, glmnet, ranger, kknn, gbm, spaMM, gstat,
               spgwr, spdep, caret , Metrics, mccr,
               geoR, raster, rgeos, sperrorest, ggplot2, 
               gridExtra, tidyverse)

###############
################ # MODELS
################ 


models <- readRDS("./resultados/output/models/trained_models.RDS")

###############
################ # TRAIN DATA
################ 

list_files <- list.files(path = "./data/sampling_points_split/",
                         recursive = TRUE,
                         full.name = TRUE,
                         pattern = ".*_train\\.gpkg$")  # Only include files ending with "_train.gpkg"

list_gpkg <- lapply(list_files, FUN = sf::read_sf)
list_gpkg <- list_gpkg[c(1,2,3,4,5,8,9,10,11)]



###############
################ COVS
################ 

covs_file <- list.files(path = "D:/DOCS/CIENCIA DE SUELOS/R_SUELOS/glacier_mlMODIS/data/harmonized_covs/",
                        recursive = TRUE,
                        full.name = TRUE,
                        pattern = ".gri$")  # Only include files ending with "_train.gpkg"
covs_file <- covs_file[c(1,2,3,4,5,8,9,10,11)] # sin viuda y la raya
covs_list <- lapply(covs_file, FUN = raster::stack)




###############
################ BUF POINTS
###############

bpoints <- sf::read_sf("./resultados/output/buffer_stack_aois/buffers_points_predict.gpkg")
bpoints$COR <- as.factor(bpoints$COR)

###############
############### COVS- BUFFERS EXTRATIONS 
###############

extract_covariates_aoi <- function(stack, points, buffer_size ){
  
  # INPUT:
  #stack is a rasterstackfor a specifi aoi
  # points is a spatial sf dataframe with points for a specific aoi
  # OUTPUT
  #list of covariates for each subaoi(buffer)
  
  cropped_stacks <- list()
  buffer_list <- list()
  # buffers <- list()
  
  for (i in 1:nrow(points)) {
    
    # points <- points %>% dplyr::filter(COR == aoi) # "blanca" etc.
    point <- points[i, ] 
    
    buffer <- st_transform(point, st_crs("EPSG:32719"))
    buffer <- st_buffer(point, dist  = buffer_size, endCapStyle = "SQUARE")
    buffer <- st_transform(buffer, st_crs("EPSG:4326")) # buffer en 4326 igual que stack
    
    # buffers[[i]] <- buffer
    buffer_list[[i]] <- buffer
    cropped_stack  <- terra::crop(stack, buffer)
    cropped_stacks [[i]] <- cropped_stack
  }
  result_list <- list(cropped_stacks = cropped_stacks, buffer_list = buffer_list)
  return(result_list)
  # return(cropped_stacks)
}

###############
############### SPTIAL PREDICTIONS VILCANOTA
###############

###############
############### SOLO MODIFICAR ESTO EN BASE AL AOI
###############

#VILCANOTA
aoi_train_sf <- list_gpkg[[9]]# 9 == villcanota
aoi_cov <- covs_list[[9]] 
# vilcanota buffers 75 68 72 74 69 
points <- c(75 ,68 ,72 ,74 ,69 ) # villcanota

# BLANCA
aoi_train_sf <- list_gpkg[[1]]# 9 == villcanota
aoi_cov <- covs_list[[1]] 
points <- c(2 ,3 ,4 ,5, 6) # blanca

###############
###############  SOLO MODIFICAR ESTO EN BASE AL AOI
###############


bpoints_aoi <- bpoints %>% filter(id %in% points)

output <- extract_covariates_aoi(stack = aoi_cov, 
                                 points = bpoints_aoi, 
                                 buffer_size = 1000)

buffer_list <- output$buffer_list # poligonos buffers
covs_buffers_list <- output$cropped_stacks # rsaterstacks 


###############
############### SAVE BUFFERS SHAPEFILES
###############


# for (i in seq(buffer_list)) {
#   path <- "./funciones/final_predictions/preds/buffers/vilcanota_buffer_"
#   st_write(buffer_list[[i]],
#               paste0(path, points[i],".shp"), 
#               overwrite = TRUE)
#   
# }
# 


###############
############### NDSi
###############

# reclass_ndsi <- c(-1, 0.6, 0,
#                   0.6, 1, 1)
# 
# 
# reclass_ndsi_m <- matrix(reclass_ndsi,
#                          ncol = 3,
#                          byrow = TRUE)
# 
# 
# ndsi_classified_list <- list()
# for (i in seq(covs_buffers_list)) {
#   
#   ndsi <- covs_buffers_list[[i]]$ndsi
# 
#   ndsi_classified <- reclassify(ndsi,
#                                 reclass_ndsi_m)
#   
#   ndsi_classified_list[[i]] <- ndsi_classified
# }
# 
# 
# 
# for (i in seq(ndsi_classified_list)) {
#   path <- "./funciones/final_predictions/preds/blanca_ndsi"
#   writeRaster(ndsi_classified_list[[i]],
#               paste0(path,points[i],".tif"), 
#               format = "GTiff", 
#               overwrite = TRUE)
#   
# }
# 



###############
############### RF
###############


rf_classified_list <- list()
for (i in seq(covs_buffers_list)) {
  
  # fiting 
  rf_pred_raster <- raster::predict(covs_buffers_list[[i]],
                                    models$`Vilcanota-ranger`, 
                                    type='response', 
                                    fun = function(model, ...) predict(model, ...)$predictions)
  rf_classified_list[[i]] <- rf_pred_raster
}


for (i in seq(rf_classified_list)) {
  path <- "./funciones/final_predictions/preds/blanca_rf"
  writeRaster(rf_classified_list[[i]],
              paste0(path, points[i],".tif"), 
              format = "GTiff", 
              overwrite = TRUE)
  
}










###############
############### LR
###############


# reclass_lr <- c(0, 0.5, 0,
#                 0.5, 1, 1)
#
#
# reclass_lr_m <- matrix(reclass_lr,
#                          ncol = 3,
#                          byrow = TRUE)
#
#
# lg_classified_list <- list()
# for (i in seq(covs_buffers_list)) {
#
#   lr_predic <- raster::predict(object= covs_buffers_list[[i]],
#                                model=models$`Vilcanota-lr`,
#                                type="response") # aplicar theshold
#
#   lr_predic <- reclassify(lr_predic,
#                           reclass_lr_m)
#
#   lg_classified_list[[i]] <- lr_predic
# }
#
#
#
# for (i in seq(lg_classified_list)) {
#   path <- "./funciones/final_predictions/preds/blanca_lr"
#   writeRaster(lg_classified_list[[i]],
#               paste0(path, points[i],".tif"),
#               format = "GTiff",
#               overwrite = TRUE)
#
# }
#
#
# ###############
# ############### GBM
# ###############
#
# reclass_gbm <- c(0, 0.5, 0,
#                 0.5, 1, 1)
#
#
# reclass_gbm <- matrix(reclass_gbm,
#                        ncol = 3,
#                        byrow = TRUE)
#
#
# gbm_classified_list <- list()
# for (i in seq(covs_buffers_list)) {
#
#   gbm_predic <- raster::predict(object= covs_buffers_list[[i]],
#                                 model=models$`Vilcanota-gbm`,
#                                 type="response") # # aplicar theshold
#
#   gbm_predic <- reclassify(gbm_predic,
#                            reclass_gbm)
#
#   gbm_classified_list[[i]] <- gbm_predic
# }
#
#
#
#
# for (i in seq(gbm_classified_list)) {
#   path <- "./funciones/final_predictions/preds/blanca_gbm"
#   writeRaster(gbm_classified_list[[i]],
#               paste0(path, points[i],".tif"),
#               format = "GTiff",
#               overwrite = TRUE)
#
# }
#
#
# ###############
# ############### KNNN
# ###############
#
# # TEST raster to dataframe
#
# knn_classified_list <- list()
# for (i in seq(covs_buffers_list)) {
#
#
#
#   raster_test <- subset(covs_buffers_list[[i]], c("CLASS","ndsi", "DEM"))
#   raster_test_df <- as.data.frame(raster_test, xy = TRUE)
#   raster_test_df$CLASS <-  as.factor(raster_test_df$CLASS)
#   raster_test_df <- raster_test_df %>% na.omit()
#   coords <- data.frame(x=raster_test_df[, "x"], y=raster_test_df[, "y"])
#
#   # TRAIN-DATA
#
#   data_train <- st_drop_geometry(list_gpkg[[9]])
#   data_train$CLASS <- as.factor(data_train$CLASS)
#
#   # FITING
#
#   knnn_model_preditions <- kknn::kknn(CLASS ~ ndsi + DEM, train = data_train, test = raster_test_df,
#                                       k = 10, distance = 2, kernel = "gaussian")
#   knnn_model_preditions <- knnn_model_preditions$prob
#   knnn_model_preditions <- as.data.frame(knnn_model_preditions)
#
#   merged_df <- cbind(coords, knnn_model_preditions)
#
#
#   df <- merged_df %>%
#     mutate(CLASS = case_when(
#       `0` >= 0.5 ~ 0,
#       `1` >= 0.5 ~ 1,
#       TRUE ~ NA
#     ))
#
#
#   gbm_predic <- rasterFromXYZ(df[c("x","y","CLASS")]) # dos capas a una capa
#   crs(gbm_predic) <- "+proj=longlat +datum=WGS84 +no_defs"
#
#
#   knn_classified_list[[i]] <- gbm_predic
#
# }
#
#
# for (i in seq(knn_classified_list)) {
#   path <- "./funciones/final_predictions/preds/blanca_knn"
#   writeRaster(knn_classified_list[[i]],
#               paste0(path, points[i],".tif"),
#               format = "GTiff",
#               overwrite = TRUE)
#
# }



# HACER PREDICCIONES PARA VARIOS BUFFER POINTS Y HALLAR EL UOI



# VILCANOTA BUFFERS FOR MAP
# vilcanota 75 68 72 74 69 
# blanca 1 2 3 4 5 ?????



