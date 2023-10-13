
pacman::p_load(sf, sp, glmnet, ranger, kknn, gbm, spaMM, gstat,
               spgwr, spdep, caret , Metrics, mccr,
               geoR, raster, rgeos, sperrorest, ggplot2, gridExtra, tidyverse)

# source("./funciones/moran_test.R")

aoi <- c("Blanca","Central","Huallanca",
         "Huayhuasha","Huaytapallana","LaRaya", # 6 y 7 eliminar
         "LaViuda","Raura","Urubamba",
         "Vilcabamba","Vilcanota")


#####
##### TRAIN DATA
#####

train_gpkg_list <- list.files(path = "./data/sampling_points_split/",
                              recursive = TRUE,
                              full.name = TRUE,
                              pattern = ".*_train\\.gpkg$")  # Only include files ending with "_train.gpkg"

train_gpkg <- lapply(train_gpkg_list, FUN = sf::read_sf)

train_gpkg <- train_gpkg[c(1,2,3,4,5,8,9,10,11)]




#####
##### TEST DATA
#####

test_gpkg_list <- list.files(path = "./output/test_samples/",
                              recursive = TRUE,
                              full.name = TRUE,
                              pattern = ".*_test\\.gpkg$")  # Only include files ending with "_train.gpkg"

test_gpkg <- lapply(test_gpkg_list, FUN = sf::read_sf)

test_gpkg <- test_gpkg[c(1,2,3,4,5,8,9,10,11)]


"CREO QUE HAY QUE USAR DATA CLUSTERISADA NO BALANCEADA PORQUE LOS RESULTADOS 
ESTAN SIENDO MUY PARECIDO A LA VALIDACION ALEATORIA SIMPLE, ES DECIR 
LA SP CV NO APROXIMA EL ERROR REAL.

PORQUE NDSI GENERA LA MEJO PREDICCION????? EN EL TEST DATASET

"



# ####
# #### PARAMETROS
# ####
# 
# formula <- CLASS ~ ndsi + DEM
# seedN <- 123
# 
# bandwidths <- read_rds("bandwidths.RDS")
# bandwidth <- bandwidths[[1]]
# 
# train_sf <- train_gpkg[[1]]
# test_sf <- test_gpkg[[1]]
# 


######################
###################### GWR
###################### 

gwr <- function(train_sf, test_sf, formula, seedN, bandwidth) {
  
  
  coordinates <- st_coordinates(train_sf)
  train_data <- st_drop_geometry(train_sf)
  train_data$x <- coordinates[, "X"]
  train_data$y <- coordinates[, "Y"]
  
  coordinates <- st_coordinates(test_sf)
  test_data <- st_drop_geometry(test_sf)
  test_data$x <- coordinates[, "X"]
  test_data$y <- coordinates[, "Y"]
  
  
  # data_sp <- SpatialPointsDataFrame(data[, c("x", "y")],data)
  # bandwidth <-  spgwr::ggwr.sel(CLASS ~ ndsi+DEM, data = data_sp, family = "binomial")
  
  results <- data.frame(
    Accuracy = numeric(),
    MCC = numeric(),
    TP = integer(),
    TN = integer(),
    FP = integer(),
    FN = integer(),
    Precision = numeric(),
    Recall = numeric(),
    F1_Score = numeric(),
    Kappa = numeric()
    
  )
  
  set.seed(seedN) 
  
  train_sp <- SpatialPointsDataFrame(train_data[, c("x", "y")],train_data) # tiene x y y columns
  
  test_sp <- SpatialPointsDataFrame(test_data[, c("x", "y")],test_data)
  
  cv_model <- spgwr::ggwr(CLASS ~ ndsi+DEM, data = train_sp,
                          bandwidth = bandwidth, family = "binomial", 
                          fit.points = test_sp)  
  
  coefficients  <- cv_model$SDF@data
  
  #generalizar para cualwuier formula usada.
  
  intercept <- coefficients[["X.Intercept."]]
  predictor_names <- c("ndsi", "DEM")  
  predictor_data <- test_sp[,c("ndsi","DEM")]
  
  
  predicted_values <- vector("numeric", length(predictor_data))
  
  for (i in 1:length(predictor_names)) {
    
    predictor <- predictor_names[i]
    coef <- coefficients[[predictor]]
    predicted_values <- predicted_values + coef * predictor_data[[predictor]]
    
  }
  
  predicted_values <- 1 / (1 + exp(-(intercept + predicted_values)))
  predicted_classes <- ifelse(predicted_values >= 0.5, 1, 0)
  
  accuracy <- Metrics::accuracy(test_sp$CLASS,predicted_classes)
  mcc <- mccr::mccr(test_sp$CLASS,predicted_classes)
  
  TP <- sum(test_sp$CLASS == 1 & predicted_classes == 1)
  TN <- sum(test_sp$CLASS == 0 & predicted_classes == 0)
  FP <- sum(test_sp$CLASS == 0 & predicted_classes == 1)
  FN <- sum(test_sp$CLASS == 1 & predicted_classes == 0)
  
  Precision <- TP / (TP + FP)
  Recall <- TP / (TP + FN)
  F1_Score <- 2 * (Precision * Recall) / (Precision + Recall)
  kappa <- caret::confusionMatrix(factor(predicted_classes, levels = 0:1),
                                  factor(test_sp$CLASS, levels = 0:1))$overall["Kappa"][[1]]
  
  
  results <- rbind(results, data.frame(
    Accuracy = accuracy,
    MCC = mcc,
    TP = TP,
    TN = TN,
    FP = FP,
    FN = FN,
    Precision = Precision,
    Recall = Recall,
    F1_Score = F1_Score,
    Kappa = kappa)
  )
  
  
  return(results)
}

# gwr_spCV(train_sf, test_sf, formula, seedN, bandwidth)



######################
###################### LOGISTIC REGRESION
######################


lr <- function(train_sf, test_sf, formula, seedN, bandwidth = NULL) {
  

  # coordinates <- st_coordinates(train_sf)
  data <- st_drop_geometry(train_sf)
  # data$x <- coordinates[, "X"]
  # data$y <- coordinates[, "Y"]
  
  data$CLASS <- as.factor(data$CLASS)
  
  results <- data.frame(
                        Accuracy = numeric(),
                        MCC = numeric(),
                        TP = integer(),
                        TN = integer(),
                        FP = integer(),
                        FN = integer(),
                        Precision = numeric(),
                        Recall = numeric(),
                        F1_Score = numeric(),
                        Kappa = numeric()
                        )
  
  set.seed(seedN) 
  

  train_data <-  data
  test_data <-  test_sf
  
  # train_sp <- SpatialPointsDataFrame(train_data[, c("x", "y")],train_data) 
  # test_sp <- SpatialPointsDataFrame(test_data[, c("x", "y")],test_data)
  
  cv_model <- glm(formula, data = train_data, family = "binomial") 
  
  prediction_probs <- predict(cv_model,test_data, type = "response")
  predicted_classes <- ifelse(prediction_probs >= 0.5, 1, 0)
  
  accuracy <- Metrics::accuracy(test_data$CLASS,predicted_classes)
  mcc <- mccr::mccr(test_data$CLASS,predicted_classes)
  
  TP <- sum(test_data$CLASS == 1 & predicted_classes == 1)
  TN <- sum(test_data$CLASS == 0 & predicted_classes == 0)
  FP <- sum(test_data$CLASS == 0 & predicted_classes == 1)
  FN <- sum(test_data$CLASS == 1 & predicted_classes == 0)
  
  Precision <- TP / (TP + FP)
  Recall <- TP / (TP + FN)
  F1_Score <- 2 * (Precision * Recall) / (Precision + Recall)
  kappa <- caret::confusionMatrix(factor(predicted_classes, levels = 0:1),
                                  factor(test_data$CLASS, levels = 0:1))$overall["Kappa"][[1]]
  
  results <- rbind(results, data.frame(
                                       Accuracy = accuracy,
                                       MCC = mcc,
                                       TP = TP,
                                       TN = TN,
                                       FP = FP,
                                       FN = FN,
                                       Precision = Precision,
                                       Recall = Recall,
                                       F1_Score = F1_Score,
                                       Kappa = kappa))

  
  return(list( results, cv_model))
}

# lr_spCV(train_sf, test_sf, formula, seedN)




######################
###################### KKNN
######################

kknn <- function(train_sf, test_sf, formula, seedN, bandwidth = NULL) {
  

  data <- st_drop_geometry(train_sf)

  
  data$CLASS <- as.factor(data$CLASS)
  
  results <- data.frame(
    Accuracy = numeric(),
    MCC = numeric(),
    TP = integer(),
    TN = integer(),
    FP = integer(),
    FN = integer(),
    Precision = numeric(),
    Recall = numeric(),
    F1_Score = numeric(),
    Kappa = numeric()
  )
  
  set.seed(seedN) 
  
  
  train_data <-  data
  test_data <-  test_sf
      
      
      
  cv_model <- kknn::kknn(formula, train = train_data, test = test_data,
                         k = 10, distance = 2, kernel = "gaussian" ) 
  
  predictions <- cv_model$prob
  predicted_classes <- ifelse(predictions[, 2] >= 0.5, 1, 0)
  
  accuracy <- Metrics::accuracy(test_data$CLASS, predicted_classes)
  mcc <- mccr::mccr(test_data$CLASS,predicted_classes)
  
  TP <- sum(test_data$CLASS == 1 & predicted_classes == 1)
  TN <- sum(test_data$CLASS == 0 & predicted_classes == 0)
  FP <- sum(test_data$CLASS == 0 & predicted_classes == 1)
  FN <- sum(test_data$CLASS == 1 & predicted_classes == 0)
  
  Precision <- TP / (TP + FP)
  Recall <- TP / (TP + FN)
  F1_Score <- 2 * (Precision * Recall) / (Precision + Recall)
  kappa <- caret::confusionMatrix(factor(predicted_classes, levels = 0:1),
                                  factor(test_data$CLASS, levels = 0:1))$overall["Kappa"][[1]]
  

  
  results <- rbind(results, data.frame(
    Accuracy = accuracy,
    MCC = mcc,
    TP = TP,
    TN = TN,
    FP = FP,
    FN = FN,
    Precision = Precision,
    Recall = Recall,
    F1_Score = F1_Score,
    Kappa = kappa))

  
  return(list( results, cv_model))
}

# kknn_spCV(train_sf, test_sf, formula, seedN)


######################
###################### ranger_spCV
######################

ranger <- function(train_sf, test_sf, formula, seedN, bandwidth = NULL) {
  
  data <- st_drop_geometry(train_sf)
  
  
  data$CLASS <- as.factor(data$CLASS)
  
  results <- data.frame(
    Accuracy = numeric(),
    MCC = numeric(),
    TP = integer(),
    TN = integer(),
    FP = integer(),
    FN = integer(),
    Precision = numeric(),
    Recall = numeric(),
    F1_Score = numeric(),
    Kappa = numeric()
  )
  
  set.seed(seedN) 
  
  
  train_data <-  data
  test_data <-  test_sf
      
  
  cv_model <- ranger::ranger(formula , data = train_data, num.trees = 100,
                             classification = TRUE ) #, probability = TRUE
  
  predictions  <- predict(cv_model, data = st_drop_geometry(test_data))
  predicted_classes <- predictions$predictions
  
  accuracy <- Metrics::accuracy(test_data$CLASS, predicted_classes)
  mcc <- mccr::mccr(test_data$CLASS,predicted_classes)
  
  TP <- sum(test_data$CLASS == 1 & predicted_classes == 1)
  TN <- sum(test_data$CLASS == 0 & predicted_classes == 0)
  FP <- sum(test_data$CLASS == 0 & predicted_classes == 1)
  FN <- sum(test_data$CLASS == 1 & predicted_classes == 0)
  
  Precision <- TP / (TP + FP)
  Recall <- TP / (TP + FN)
  F1_Score <- 2 * (Precision * Recall) / (Precision + Recall)
  kappa <- caret::confusionMatrix(factor(predicted_classes, levels = 0:1),
                                  factor(test_data$CLASS, levels = 0:1))$overall["Kappa"][[1]]
  
  
  
  results <- rbind(results, data.frame(
    Accuracy = accuracy,
    MCC = mcc,
    TP = TP,
    TN = TN,
    FP = FP,
    FN = FN,
    Precision = Precision,
    Recall = Recall,
    F1_Score = F1_Score,
    Kappa = kappa))
  
 
  return(list( results, cv_model))
}

# ranger_spCV(train_sf, test_sf, formula, seedN)


######################
###################### gbm
######################


gbm<- function(train_sf, test_sf, formula, seedN, bandwidth = NULL) {
  
  data <- st_drop_geometry(train_sf)
  
  
  # data$CLASS <- as.factor(data$CLASS)
  
  results <- data.frame(
    Accuracy = numeric(),
    MCC = numeric(),
    TP = integer(),
    TN = integer(),
    FP = integer(),
    FN = integer(),
    Precision = numeric(),
    Recall = numeric(),
    F1_Score = numeric(),
    Kappa = numeric()
  )
  
  set.seed(seedN) 
  
  
  train_data <-  data
  test_data <-  test_sf
  # test_sf$CLASS <- as.factor(test_sf$CLASS)
  
  cv_model <- gbm::gbm(formula, #as.formula(formula)
                           distribution = "bernoulli", 
                           data = train_data, 
                           n.trees = 100)
      
  prediction_probs <- predict(cv_model, newdata = st_drop_geometry(test_data), type = "response",  n.trees = 100) 
  predicted_classes <- ifelse(prediction_probs >= 0.5, 1, 0)
  
  accuracy <- Metrics::accuracy(test_data$CLASS,predicted_classes)
  mcc <- mccr::mccr(test_data$CLASS, predicted_classes)
  
  TP <- sum(test_data$CLASS == 1 & predicted_classes == 1)
  TN <- sum(test_data$CLASS == 0 & predicted_classes == 0)
  FP <- sum(test_data$CLASS == 0 & predicted_classes == 1)
  FN <- sum(test_data$CLASS == 1 & predicted_classes == 0)
  
  Precision <- TP / (TP + FP)
  Recall <- TP / (TP + FN)
  F1_Score <- 2 * (Precision * Recall) / (Precision + Recall)
  kappa <- caret::confusionMatrix(factor(predicted_classes, levels = 0:1),
                                  factor(test_data$CLASS, levels = 0:1))$overall["Kappa"][[1]]
  

  results <- rbind(results, data.frame(
    Accuracy = accuracy,
    MCC = mcc,
    TP = TP,
    TN = TN,
    FP = FP,
    FN = FN,
    Precision = Precision,
    Recall = Recall,
    F1_Score = F1_Score,
    Kappa = kappa))
 
  
  return(list( results, cv_model))
}

# gbm_spCV(train_sf, test_sf, formula, seedN)

######################
###################### ndsi
######################


ndsi <- function(train_sf = NULL, test_sf, formula = NULL, seedN, bandwidth = NULL) {
  
  data <- st_drop_geometry(train_sf)
  
  
  # data$CLASS <- as.factor(data$CLASS)
  
  results <- data.frame(
    Accuracy = numeric(),
    MCC = numeric(),
    TP = integer(),
    TN = integer(),
    FP = integer(),
    FN = integer(),
    Precision = numeric(),
    Recall = numeric(),
    F1_Score = numeric(),
    Kappa = numeric()
  )
  
  set.seed(seedN) 
  
  
  train_data <-  data
  test_data <-  test_sf
  # test_sf$CLASS <- as.factor(test_sf$CLASS)
  
  # cv_model <- gbm::gbm(formula, #as.formula(formula)
  #                      distribution = "bernoulli", 
  #                      data = train_data, 
  #                      n.trees = 100)
  
  # prediction_probs <- predict(cv_model, newdata = st_drop_geometry(test_data), type = "response",  n.trees = 100) 
  # predicted_classes <- ifelse(prediction_probs >= 0.5, 1, 0)
  
  # predicted_classes <- ifelse(test_data$ndsi >= 0.4 & test_data$ndsi <= 1, 1, 0)
  
  source("./funciones/geoprocesamientos/ndsi_function.R")
  
  predicted_classes <- ndsi_to_binary(test_data) # funcion para hace prediciones
  
  
  accuracy <- Metrics::accuracy(test_data$CLASS,predicted_classes)
  mcc <- mccr::mccr(test_data$CLASS, predicted_classes)
  
  TP <- sum(test_data$CLASS == 1 & predicted_classes == 1)
  TN <- sum(test_data$CLASS == 0 & predicted_classes == 0)
  FP <- sum(test_data$CLASS == 0 & predicted_classes == 1)
  FN <- sum(test_data$CLASS == 1 & predicted_classes == 0)
  
  Precision <- TP / (TP + FP)
  Recall <- TP / (TP + FN)
  F1_Score <- 2 * (Precision * Recall) / (Precision + Recall)
  kappa <- caret::confusionMatrix(factor(predicted_classes, levels = 0:1),
                                  factor(test_data$CLASS, levels = 0:1))$overall["Kappa"][[1]]
  
  
  results <- rbind(results, data.frame(
    Accuracy = accuracy,
    MCC = mcc,
    TP = TP,
    TN = TN,
    FP = FP,
    FN = FN,
    Precision = Precision,
    Recall = Recall,
    F1_Score = F1_Score,
    Kappa = kappa))
  
  
  return(list( results, cv_model = NULL))
  
  
}




######################
###################### WRAPPER
######################

####
#### PARAMETROS
####

aoi <- c("Blanca","Central","Huallanca",
         "Huayhuasha","Huaytapallana",
         "Raura","Urubamba",
         "Vilcabamba","Vilcanota")




formula <- CLASS ~ ndsi + DEM
seedN <- 123

# bandwidths_test <- read_rds("./funciones/bandwithds/bandwithds_test.RDS")


# bandwidth <- bandwidths[[1]]
# train_sf <- train_gpkg[[1]]
# test_sf <- test_gpkg[[1]]

# models <- c("gwr") #"ndsi" , ,

models <- c("ndsi", "lr", "kknn","ranger", "gbm") #, "lr",


test_wrapper <- function(train_gpkg, test_gpkg,
                         aoi, formula, seedN,
                         bandwidths, models) {
  
  
  # INPUT
  # train_sf: list
  # test_sf: list
  # bandwidths :list
  # aoi : vector
  # seedN ¨int
  # bandwidths: list
  # models : vector of models names strings
  # OUTPUT
  # results: dataframe
  
  
  
  results <- data.frame()
  trained_models <- list()
  
  
  for (i in seq_along(train_gpkg)) {
    
    train_sf <- train_gpkg[[i]]
    test_sf <-  test_gpkg[[i]]
    
    current_aoi <- aoi[i]
    bandwidth <- bandwidths[[i]]
    
      
      for (model in models) {
        
        cv_func <- get(model)
        cv_results <- cv_func(train_sf, test_sf, formula , seedN, bandwidth)
        
        results_df <- cv_results[[1]] # dataframe error
        fitted_model <- cv_results[[2]] # modelo ajustado
        
        
        results_df$aoi <- rep(current_aoi, nrow(results_df))
        results_df$model <- rep(model, nrow(results_df))
        
        results <- rbind(results, results_df)
        
        trained_models[[paste0(current_aoi, "-", model)]] <- fitted_model
         
        # trained_models[[paste0(current_aoi, "-", model)]] <- cv_results[[2]]
        
      }
  }
  
  return(list(results,trained_models))
  
}


results_test_wraper <- test_wrapper(train_gpkg, 
                                    test_gpkg,
                                    aoi, 
                                    formula,
                                    seedN,
                                    bandwidths_test,
                                    models)

#añadir ndsi errors

#resultados 

df_mcc <- results_test_wraper[[1]]
df_mcc <- df_mcc %>% dplyr::select(c("MCC","aoi","model"))


df_mcc$model <- as.factor(df_mcc$model) ## cmbiar nombres de modelos 
levels(df_mcc$model)  <- c("GBM", "KNN", "LR",
                            "NDSI","RF")




write.csv(df_mcc, "./output/test_samples/reference_mcc_test.csv")





# modelos 

trained_models <- results_test_wraper[[2]] # aqui todavia essta laraya y laviuda

write_rds(trained_models, "./output/models/trained_models.RDS")

# trained_models <- list()
# trained_models[[paste0(current_aoi, "-", model)]] <- cv_results$trained_model
# 
# return(list(results = results, trained_models = trained_models))


