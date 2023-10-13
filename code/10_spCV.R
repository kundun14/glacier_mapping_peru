
pacman::p_load(sf, sp, glmnet, ranger, kknn, gbm, spaMM, gstat,
               spgwr, spdep, caret , Metrics, mccr,
               geoR, raster, rgeos, sperrorest, ggplot2, gridExtra, tidyverse)

source("./funciones/moran_test.R")

######################
###################### zeroR
######################

# 
#  zeror_spCV <- function(sf, formula = NULL, repetitions, k, seedN, bandwidth = NULL) {
#   
#   
#   "
#   data es un sf object, tiene que tener la columna CLASS ***
#   
#   formula es un string, e.g. CLASS ~ BLUE + GREEN
#   repetition is a integer
#   k is an integer
#   seedN is integer to set.seed()
#   
#   "
#   coordinates <- st_coordinates(sf)
#   data <- st_drop_geometry(sf)
#   data$x <- coordinates[, "X"]
#   data$y <- coordinates[, "Y"]
#   
#   results <- data.frame(Repetition = integer(),
#                         Fold = integer(),
#                         Accuracy = numeric(),
#                         MCC = numeric(),
#                         TP = integer(),
#                         TN = integer(),
#                         FP = integer(),
#                         FN = integer(),
#                         Precision = numeric(),
#                         Recall = numeric(),
#                         F1_Score = numeric(),
#                         kappa = numeric(),
#                         Moran_I = numeric(),
#                         Moran_p = numeric())
#   
#   
#   
#   set.seed(seedN) 
#   
#   for (rep in seq_len(repetitions)) {
#     
#     kmeans_result <- kmeans(coordinates, centers = k)
#     data$Cluster <- kmeans_result$cluster
#     flds = unique(data$Cluster)
#     
#     
#     for (geoFold in 1:length(flds)){
#       
#       id <-  which(data$Cluster == levels(as.factor(flds))[geoFold]) # j=1 -> test cluster 1, j=2 -> test cluster 2 ...
#       
#       train_data <-  data[-id,]
#       test_data <-  data[id,]
#       
#       train_sp <- SpatialPointsDataFrame(train_data[, c("x", "y")],train_data) 
#       test_sp <- SpatialPointsDataFrame(test_data[, c("x", "y")],test_data)
#       
#       most_frequent_class <- names(sort(table(train_data$CLASS), decreasing = TRUE))[1]
#       
#       predicted_classes <- rep(most_frequent_class, nrow(test_data))
#       
#       accuracy <- Metrics::accuracy(test_data$CLASS, predicted_classes)
#       mcc <- mccr::mccr(test_data$CLASS, predicted_classes)
#       
#       
#       TP <- sum(test_sp$CLASS == 1 & predicted_classes == 1)
#       TN <- sum(test_sp$CLASS == 0 & predicted_classes == 0)
#       FP <- sum(test_sp$CLASS == 0 & predicted_classes == 1)
#       FN <- sum(test_sp$CLASS == 1 & predicted_classes == 0)
#       
#       Precision <- TP / (TP + FP)
#       Recall <- TP / (TP + FN)
#       F1_Score <- 2 * (Precision * Recall) / (Precision + Recall)
#       kappa <- caret::confusionMatrix(factor(predicted_classes), factor(test_sp$CLASS))$overall["Kappa"][[1]]
#       
#       # MORAN
#       
#       Moran <- calculate_moran_i(predicted_classes, test_sp$CLASS, coordinates(test_sp))
#       Moran_I <- Moran[1]
#       Moran_p <- Moran[2]
#       
#       results <- rbind(results, data.frame(Repetition = rep,
#                                            Fold = geoFold,
#                                            Accuracy = accuracy,
#                                            MCC = mcc,
#                                            TP = TP,
#                                            TN = TN,
#                                            FP = FP,
#                                            FN = FN,
#                                            Precision = Precision,
#                                            Recall = Recall,
#                                            F1_Score = F1_Score,
#                                            kappa = kappa,
#                                            Moran_I = Moran_I,
#                                            Moran_p = Moran_p))
#     }
#   }
#   
#   return(results)
# }


######################
###################### ndsi
######################


ndsi_spCV <- function(sf, formula = NULL, repetitions, k, seedN, bandwidth = NULL) {
  
  
  "
  data es un sf object, tiene que tener la columna CLASS ***
  
  formula es un string, e.g. CLASS ~ BLUE + GREEN
  repetition is a integer
  k is an integer
  seedN is integer to set.seed()
  
  "
  
  coordinates <- st_coordinates(sf)
  data <- st_drop_geometry(sf)
  data$x <- coordinates[, "X"]
  data$y <- coordinates[, "Y"]
  
  
  results <- data.frame(Repetition = integer(),
                        Fold = integer(),
                        Accuracy = numeric(),
                        MCC = numeric(),
                        TP = integer(),
                        TN = integer(),
                        FP = integer(),
                        FN = integer(),
                        Precision = numeric(),
                        Recall = numeric(),
                        F1_Score = numeric(),
                        Kappa = numeric(),
                        Moran_I = numeric(),
                        Moran_p = numeric())
  
  set.seed(seedN) 
  
  for (rep in seq_len(repetitions)) {
    
    kmeans_result <- kmeans(coordinates, centers = k)
    data$Cluster <- kmeans_result$cluster
    flds = unique(data$Cluster)
    
    
    for (geoFold in 1:length(flds)){
      
      id <-  which(data$Cluster == levels(as.factor(flds))[geoFold]) # j=1 -> test cluster 1, j=2 -> test cluster 2 ...
      
      train_data <-  data[-id,]
      test_data <-  data[id,] 
      
      train_sp <- SpatialPointsDataFrame(train_data[, c("x", "y")],train_data) 
      test_sp <- SpatialPointsDataFrame(test_data[, c("x", "y")],test_data)
      
      predicted_classes <- ifelse(test_data$ndsi >= 0.4 & test_data$ndsi <= 1, 1, 0)
      
      
      accuracy <- Metrics::accuracy(test_data$CLASS, predicted_classes)
      mcc <- mccr::mccr(test_data$CLASS, predicted_classes)
      
      TP <- sum(test_sp$CLASS == 1 & predicted_classes == 1)
      TN <- sum(test_sp$CLASS == 0 & predicted_classes == 0)
      FP <- sum(test_sp$CLASS == 0 & predicted_classes == 1)
      FN <- sum(test_sp$CLASS == 1 & predicted_classes == 0)
      
      Precision <- TP / (TP + FP)
      Recall <- TP / (TP + FN)
      F1_Score <- 2 * (Precision * Recall) / (Precision + Recall)
      kappa <- caret::confusionMatrix(factor(predicted_classes, levels = 0:1),
                                      factor(test_sp$CLASS, levels = 0:1))$overall["Kappa"][[1]]
      
      # MORAN
      
      Moran <- calculate_moran_i(predicted_classes, test_sp$CLASS, coordinates(test_sp))
      Moran_I <- Moran[1]
      Moran_p <- Moran[2]
      
      results <- rbind(results, data.frame(Repetition = rep,
                                           Fold = geoFold,
                                           Accuracy = accuracy,
                                           MCC = mcc,
                                           TP = TP,
                                           TN = TN,
                                           FP = FP,
                                           FN = FN,
                                           Precision = Precision,
                                           Recall = Recall,
                                           F1_Score = F1_Score,
                                           Kappa = kappa,
                                           Moran_I = Moran_I,
                                           Moran_p = Moran_p))
    }
  }
  
  return(results)
}




######################
###################### LOGISTIC REGRESION
######################


lr_spCV <- function(sf, formula, repetitions, k, seedN, bandwidth = NULL) {
  
  "
  data es un sf object, tiene que tener la columna CLASS de clase integer 1 y 0
  
  formula es un string, e.g. CLASS ~ BLUE + GREEN
  repetition is a integer
  k is an integer
  seedN is integer to set.seed()
  
  "
  coordinates <- st_coordinates(sf)
  data <- st_drop_geometry(sf)
  data$x <- coordinates[, "X"]
  data$y <- coordinates[, "Y"]
  
  data$CLASS <- as.factor(data$CLASS)
  
  results <- data.frame(Repetition = integer(),
                        Fold = integer(),
                        Accuracy = numeric(),
                        MCC = numeric(),
                        TP = integer(),
                        TN = integer(),
                        FP = integer(),
                        FN = integer(),
                        Precision = numeric(),
                        Recall = numeric(),
                        F1_Score = numeric(),
                        Kappa = numeric(),
                        Moran_I = numeric(),
                        Moran_p = numeric())
  
  set.seed(seedN) 
  
  for (rep in seq_len(repetitions)) {
    
    kmeans_result <- kmeans(coordinates, centers = k)
    data$Cluster <- kmeans_result$cluster
    flds = unique(data$Cluster)
    
    
    for (geoFold in 1:length(flds)){
      
      id <-  which(data$Cluster == levels(as.factor(flds))[geoFold]) # j=1 -> test cluster 1, j=2 -> test cluster 2 ...
      
      train_data <-  data[-id,]
      test_data <-  data[id,] 
      
      train_sp <- SpatialPointsDataFrame(train_data[, c("x", "y")],train_data) 
      test_sp <- SpatialPointsDataFrame(test_data[, c("x", "y")],test_data)
      
      cv_model <- glm(formula, data = train_data, family = "binomial") 
      
      prediction_probs <- predict(cv_model,test_data, type = "response")
      predicted_classes <- ifelse(prediction_probs >= 0.5, 1, 0)
      
      accuracy <- Metrics::accuracy(test_data$CLASS,predicted_classes)
      mcc <- mccr::mccr(test_data$CLASS,predicted_classes)
      
      TP <- sum(test_sp$CLASS == 1 & predicted_classes == 1)
      TN <- sum(test_sp$CLASS == 0 & predicted_classes == 0)
      FP <- sum(test_sp$CLASS == 0 & predicted_classes == 1)
      FN <- sum(test_sp$CLASS == 1 & predicted_classes == 0)
      
      Precision <- TP / (TP + FP)
      Recall <- TP / (TP + FN)
      F1_Score <- 2 * (Precision * Recall) / (Precision + Recall)
      kappa <- caret::confusionMatrix(factor(predicted_classes, levels = 0:1),
                                      factor(test_sp$CLASS, levels = 0:1))$overall["Kappa"][[1]]
      
      # MORAN
      
      Moran <- calculate_moran_i(predicted_classes, test_sp$CLASS, coordinates(test_sp))
      Moran_I <- Moran[1]
      Moran_p <- Moran[2]
      
      results <- rbind(results, data.frame(Repetition = rep,
                                           Fold = geoFold,
                                           Accuracy = accuracy,
                                           MCC = mcc,
                                           TP = TP,
                                           TN = TN,
                                           FP = FP,
                                           FN = FN,
                                           Precision = Precision,
                                           Recall = Recall,
                                           F1_Score = F1_Score,
                                           Kappa = kappa,
                                           Moran_I = Moran_I,
                                           Moran_p = Moran_p))
    }
  }
  
  return(results)
}

# lr_spCV(blanca_train_sf, formula, repetitions, k, seedN)
# 
# results <- lr_spCV(blanca_train_sf, formula, repetitions, k, seedN)
# x <- results %>% dplyr::filter(Accuracy != 1)
# 
# nrow(x)

######################
###################### GWR
###################### 



gwr_spCV <- function(sf, formula, repetitions, k, seedN, bandwidth) {
  
  "
  data es un sf object, tiene que tener la columna CLASS de clase integer 1 y 0
  
  formula es un string, e.g. CLASS ~ BLUE + GREEN
  repetition is a integer
  k is an integer
  seedN is integer to set.seed()
  
  "
  
  coordinates <- st_coordinates(sf)
  data <- st_drop_geometry(sf)
  data$x <- coordinates[, "X"]
  data$y <- coordinates[, "Y"]
  
  # data_sp <- SpatialPointsDataFrame(data[, c("x", "y")],data)
  # bandwidth <-  spgwr::ggwr.sel(CLASS ~ ndsi+DEM, data = data_sp, family = "binomial")
  
  results <- data.frame(Repetition = integer(),
                        Fold = integer(),
                        Accuracy = numeric(),
                        MCC = numeric(),
                        TP = integer(),
                        TN = integer(),
                        FP = integer(),
                        FN = integer(),
                        Precision = numeric(),
                        Recall = numeric(),
                        F1_Score = numeric(),
                        Kappa = numeric(),
                        Moran_I = numeric(),
                        Moran_p = numeric())
  
  set.seed(seedN) 
  
  for (rep in seq_len(repetitions)) {
    
    kmeans_result <- kmeans(coordinates, centers = k)
    data$Cluster <- kmeans_result$cluster
    flds = unique(data$Cluster)
    
    
    for (geoFold in 1:length(flds)){
      
      id <-  which(data$Cluster == levels(as.factor(flds))[geoFold]) # j=1 -> test cluster 1, j=2 -> test cluster 2 ...
      
      train_data <-  data[-id,]
      test_data <-  data[id,] 
      
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
      
      # MORAN
      
      Moran <- calculate_moran_i(predicted_classes, test_sp$CLASS, coordinates(test_sp))
      Moran_I <- Moran[1]
      Moran_p <- Moran[2]
      
      results <- rbind(results, data.frame(Repetition = rep,
                                           Fold = geoFold,
                                           Accuracy = accuracy,
                                           MCC = mcc,
                                           TP = TP,
                                           TN = TN,
                                           FP = FP,
                                           FN = FN,
                                           Precision = Precision,
                                           Recall = Recall,
                                           F1_Score = F1_Score,
                                           Kappa = kappa,
                                           Moran_I = Moran_I,
                                           Moran_p = Moran_p))
    }
  }
  
  return(results)
}


# gwr_spCV(blanca_train_sf, formula, repetitions, k, seedN, bandwidth = NULL)


######################
###################### KKNN
######################

kknn_spCV <- function(sf, formula, repetitions, k, seedN,bandwidth = NULL) {
  
  "
  data es un sf object, tiene que tener la columna CLASS de clase integer 1 y 0
  
  formula es un string, e.g. CLASS ~ BLUE + GREEN
  repetition is a integer
  k is an integer
  seedN is integer to set.seed()
  
  "
  coordinates <- st_coordinates(sf)
  data <- st_drop_geometry(sf)
  data$x <- coordinates[, "X"]
  data$y <- coordinates[, "Y"]
  
  data$CLASS <- as.factor(data$CLASS)
  
  results <- data.frame(Repetition = integer(),
                        Fold = integer(),
                        Accuracy = numeric(),
                        MCC = numeric(),
                        TP = integer(),
                        TN = integer(),
                        FP = integer(),
                        FN = integer(),
                        Precision = numeric(),
                        Recall = numeric(),
                        F1_Score = numeric(),
                        Kappa = numeric(),
                        Moran_I = numeric(),
                        Moran_p = numeric())
  
  set.seed(seedN) 
  
  for (rep in seq_len(repetitions)) {
    
    kmeans_result <- kmeans(coordinates, centers = k)
    data$Cluster <- kmeans_result$cluster
    flds = unique(data$Cluster)
    
    
    for (geoFold in 1:length(flds)){
      
      id <-  which(data$Cluster == levels(as.factor(flds))[geoFold]) # j=1 -> test cluster 1, j=2 -> test cluster 2 ...
      
      train_data <-  data[-id,]
      test_data <-  data[id,] 
      
      train_sp <- SpatialPointsDataFrame(train_data[, c("x", "y")],train_data) 
      test_sp <- SpatialPointsDataFrame(test_data[, c("x", "y")],test_data)
      
      cv_model <- kknn::kknn(formula, train = train_data, test = test_data,
                             k = 10, distance = 2, kernel = "gaussian" ) 
      
      predictions <- cv_model$prob
      predicted_classes <- ifelse(predictions[, 2] >= 0.5, 1, 0)
      
      accuracy <- Metrics::accuracy(test_data$CLASS, predicted_classes)
      mcc <- mccr::mccr(test_data$CLASS,predicted_classes)
      
      TP <- sum(test_sp$CLASS == 1 & predicted_classes == 1)
      TN <- sum(test_sp$CLASS == 0 & predicted_classes == 0)
      FP <- sum(test_sp$CLASS == 0 & predicted_classes == 1)
      FN <- sum(test_sp$CLASS == 1 & predicted_classes == 0)
      
      Precision <- TP / (TP + FP)
      Recall <- TP / (TP + FN)
      F1_Score <- 2 * (Precision * Recall) / (Precision + Recall)
      kappa <- caret::confusionMatrix(factor(predicted_classes, levels = 0:1),
                                      factor(test_sp$CLASS, levels = 0:1))$overall["Kappa"][[1]]
      
      # MORAN
      
      Moran <- calculate_moran_i(predicted_classes, test_sp$CLASS, coordinates(test_sp))
      Moran_I <- Moran[1]
      Moran_p <- Moran[2]
      
      results <- rbind(results, data.frame(Repetition = rep,
                                           Fold = geoFold,
                                           Accuracy = accuracy,
                                           MCC = mcc,
                                           TP = TP,
                                           TN = TN,
                                           FP = FP,
                                           FN = FN,
                                           Precision = Precision,
                                           Recall = Recall,
                                           F1_Score = F1_Score,
                                           Kappa = kappa,
                                           Moran_I = Moran_I,
                                           Moran_p = Moran_p))
    }
  }
  
  return(results)
}


# kknn_spCV(blanca_train_sf, formula, repetitions, k, seedN, bandwidth = NULL)


######################
###################### ranger_spCV
######################

ranger_spCV <- function(sf, formula, repetitions, k, seedN, bandwidth = NULL) {
  
  
  "
  data es un sf object, tiene que tener la columna CLASS ***
  
  formula es un string, e.g. CLASS ~ BLUE + GREEN
  repetition is a integer
  k is an integer means spatial clusters
  seedN is integer to set.seed()
  
  "
  coordinates <- st_coordinates(sf)
  data <- st_drop_geometry(sf)
  data$x <- coordinates[, "X"]
  data$y <- coordinates[, "Y"]
  
  
  results <- data.frame(Repetition = integer(),
                        Fold = integer(),
                        Accuracy = numeric(),
                        MCC = numeric(),
                        TP = integer(),
                        TN = integer(),
                        FP = integer(),
                        FN = integer(),
                        Precision = numeric(),
                        Recall = numeric(),
                        F1_Score = numeric(),
                        Kappa = numeric(),
                        Moran_I = numeric(),
                        Moran_p = numeric())
  
  
  
  set.seed(seedN) 
  
  for (rep in seq_len(repetitions)) {
    
    kmeans_result <- kmeans(coordinates, centers = k)
    data$Cluster <- kmeans_result$cluster
    flds = unique(data$Cluster)
    
    
    for (geoFold in 1:length(flds)){
      
      id <-  which(data$Cluster == levels(as.factor(flds))[geoFold]) # j=1 -> test cluster 1, j=2 -> test cluster 2 ...
      
      train_data <-  data[-id,]
      test_data <-  data[id,] 
      
      train_sp <- SpatialPointsDataFrame(train_data[, c("x", "y")],train_data) 
      test_sp <- SpatialPointsDataFrame(test_data[, c("x", "y")],test_data)
      
      cv_model <- ranger::ranger(formula , data = train_data, num.trees = 100, classification = TRUE ) #, probability = TRUE
      
      predictions  <- predict(cv_model, data = test_data)
      predicted_classes <- predictions$predictions
      
      accuracy <- Metrics::accuracy(test_data$CLASS,predicted_classes)
      mcc <- mccr::mccr(test_data$CLASS,predicted_classes)
      
      TP <- sum(test_sp$CLASS == 1 & predicted_classes == 1)
      TN <- sum(test_sp$CLASS == 0 & predicted_classes == 0)
      FP <- sum(test_sp$CLASS == 0 & predicted_classes == 1)
      FN <- sum(test_sp$CLASS == 1 & predicted_classes == 0)
      
      Precision <- TP / (TP + FP)
      Recall <- TP / (TP + FN)
      F1_Score <- 2 * (Precision * Recall) / (Precision + Recall)
      
      # u <- union(predicted_classes, test_data$CLASS)
      # t <- table(factor(predicted_classes, u), factor(test_data$CLASS, u))
      # kappa <- caret::confusionMatrix(t)$overall["Kappa"][[1]]
      
      # kappa <- caret::confusionMatrix(factor(predicted_classes), factor(test_sp$CLASS))$overall["Kappa"][[1]]
      kappa <- caret::confusionMatrix(factor(predicted_classes, levels = 0:1),
                                      factor(test_sp$CLASS, levels = 0:1))$overall["Kappa"][[1]]
      
      # MORAN
      
      Moran <- calculate_moran_i(predicted_classes, test_sp$CLASS, coordinates(test_sp))
      Moran_I <- Moran[1]
      Moran_p <- Moran[2]
      
      results <- rbind(results, data.frame(Repetition = rep,
                                           Fold = geoFold,
                                           Accuracy = accuracy,
                                           MCC = mcc,
                                           TP = TP,
                                           TN = TN,
                                           FP = FP,
                                           FN = FN,
                                           Precision = Precision,
                                           Recall = Recall,
                                           F1_Score = F1_Score,
                                           Kappa = kappa,
                                           Moran_I = Moran_I,
                                           Moran_p = Moran_p))
    }
  }
  
  return(results)
}


# repetitions <- 5
# k <- 10
# seedN <- 123
# formula <- CLASS ~ ndsi + DEM
# 
# results <- ranger_spCV(blanca_train_sf, formula, repetitions, k, seedN) 
# x <- results %>% dplyr::filter(Accuracy != 1)




######################
###################### gbm
######################


gbm_spCV <- function(sf, formula, repetitions, k, seedN, bandwidth = NULL) {
  
  
  "
  data es un sf object, tiene que tener la columna CLASS ***
  
  formula es un string, e.g. CLASS ~ BLUE + GREEN
  repetition is a integer
  k is an integer
  seedN is integer to set.seed()
  
  "
  
  coordinates <- st_coordinates(sf)
  data <- st_drop_geometry(sf)
  data$x <- coordinates[, "X"]
  data$y <- coordinates[, "Y"]
  
  
  results <- data.frame(Repetition = integer(),
                        Fold = integer(),
                        Accuracy = numeric(),
                        MCC = numeric(),
                        TP = integer(),
                        TN = integer(),
                        FP = integer(),
                        FN = integer(),
                        Precision = numeric(),
                        Recall = numeric(),
                        F1_Score = numeric(),
                        Kappa = numeric(),
                        Moran_I = numeric(),
                        Moran_p = numeric())
  
  set.seed(seedN) 
  
  for (rep in seq_len(repetitions)) {
    
    kmeans_result <- kmeans(coordinates, centers = k)
    data$Cluster <- kmeans_result$cluster
    flds = unique(data$Cluster)
    
    
    for (geoFold in 1:length(flds)){
      
      id <-  which(data$Cluster == levels(as.factor(flds))[geoFold]) # j=1 -> test cluster 1, j=2 -> test cluster 2 ...
      
      train_data <-  data[-id,]
      test_data <-  data[id,] 
      
      train_sp <- SpatialPointsDataFrame(train_data[, c("x", "y")],train_data) 
      test_sp <- SpatialPointsDataFrame(test_data[, c("x", "y")],test_data)
      
      cv_model <- gbm::gbm(as.formula(formula), 
                           distribution = "bernoulli", 
                           data = train_data, 
                           n.trees = 100)
      
      prediction_probs <- predict(cv_model,test_data, type = "response",  n.trees = 100) 
      predicted_classes <- ifelse(prediction_probs >= 0.5, 1, 0)
      
      accuracy <- Metrics::accuracy(test_data$CLASS,predicted_classes)
      mcc <- mccr::mccr(test_data$CLASS,predicted_classes)
      
      TP <- sum(test_sp$CLASS == 1 & predicted_classes == 1)
      TN <- sum(test_sp$CLASS == 0 & predicted_classes == 0)
      FP <- sum(test_sp$CLASS == 0 & predicted_classes == 1)
      FN <- sum(test_sp$CLASS == 1 & predicted_classes == 0)
      
      Precision <- TP / (TP + FP)
      Recall <- TP / (TP + FN)
      F1_Score <- 2 * (Precision * Recall) / (Precision + Recall)
      kappa <- caret::confusionMatrix(factor(predicted_classes, levels = 0:1),
                                      factor(test_sp$CLASS, levels = 0:1))$overall["Kappa"][[1]]
      
      # MORAN
      
      Moran <- calculate_moran_i(predicted_classes, test_sp$CLASS, coordinates(test_sp))
      Moran_I <- Moran[1]
      Moran_p <- Moran[2]
      
      results <- rbind(results, data.frame(Repetition = rep,
                                           Fold = geoFold,
                                           Accuracy = accuracy,
                                           MCC = mcc,
                                           TP = TP,
                                           TN = TN,
                                           FP = FP,
                                           FN = FN,
                                           Precision = Precision,
                                           Recall = Recall,
                                           F1_Score = F1_Score,
                                           Kappa = kappa,
                                           Moran_I = Moran_I,
                                           Moran_p = Moran_p))
    }
  }
  
  return(results)
}



# gbm_spCV(blanca_train_sf, formula, repetitions, k, seedN, bandwidth = NULL)




######################
###################### INDICATOR KRIGING 
###################### 

# 
# ik_spCV <- function(sf, formula=NULL, repetitions, k, seedN, bandwidth = NULL) {
#   
#   "
#   data es un sf object, tiene que tener la columna CLASS de clase integer 1 y 0
#   repetition is a integer
#   k is an integer
#   seedN is integer to set.seed()
#   
#   "
#   coordinates <- st_coordinates(sf)
#   data <- st_drop_geometry(sf)
#   data$x <- coordinates[, "X"]
#   data$y <- coordinates[, "Y"]
#   
#   results <- data.frame(Repetition = integer(),
#                         Fold = integer(),
#                         Accuracy = numeric(),
#                         MCC = numeric(),
#                         TP = integer(),
#                         TN = integer(),
#                         FP = integer(),
#                         FN = integer(),
#                         Precision = numeric(),
#                         Recall = numeric(),
#                         F1_Score = numeric(),
#                         Kappa = numeric(),
#                         Moran_I = numeric(),
#                         Moran_p = numeric())
#   
#   set.seed(seedN) 
#   
#   for (rep in seq_len(repetitions)) {
#     
#     kmeans_result <- kmeans(coordinates, centers = k)
#     data$Cluster <- kmeans_result$cluster
#     flds = unique(data$Cluster)
#     
#     
#     for (geoFold in 1:length(flds)){
#       
#       id <-  which(data$Cluster == levels(as.factor(flds))[geoFold]) # j=1 -> test cluster 1, j=2 -> test cluster 2 ...
#       
#       train_data <-  data[-id,]
#       test_data <-  data[id,] 
#       
#       train_sp <- SpatialPointsDataFrame(train_data[, c("x", "y")],train_data) 
#       test_sp <- SpatialPointsDataFrame(test_data[, c("x", "y")],test_data)
#       
#       emp_var <- variogram(I(CLASS == 1) ~ 1, 
#                            train_sp)
#       
#       var_model <- vgm(psill = 0.15, 
#                        model = "Mat", # "Exp", "Sph", "Gau", "Mat"
#                        range = 4000, 
#                        nugget = 0.05)
#       
#       var_model <- fit.variogram(emp_var, var_model)
#       
#       predicted_values = krige(I(CLASS == 1) ~ 1,
#                                locations = train_sp,
#                                newdata = test_sp,
#                                model = var_model)
#       
#       predicted_classes <- ifelse(predicted_values$var1.pred >= 0.5, 1, 0)
#       
#       accuracy <- Metrics::accuracy(test_sp$CLASS, predicted_classes)
#       mcc <- mccr::mccr(test_sp$CLASS, predicted_classes) 
#       
#       TP <- sum(test_sp$CLASS == 1 & predicted_classes == 1)
#       TN <- sum(test_sp$CLASS == 0 & predicted_classes == 0)
#       FP <- sum(test_sp$CLASS == 0 & predicted_classes == 1)
#       FN <- sum(test_sp$CLASS == 1 & predicted_classes == 0)
#       
#       Precision <- TP / (TP + FP)
#       Recall <- TP / (TP + FN)
#       F1_Score <- 2 * (Precision * Recall) / (Precision + Recall)
#       kappa <- caret::confusionMatrix(factor(predicted_classes, levels = 0:1),
#                                       factor(test_sp$CLASS, levels = 0:1))$overall["Kappa"][[1]]
#       
#       # MORAN
#       
#       Moran <- calculate_moran_i(predicted_classes, test_sp$CLASS, coordinates(test_sp))
#       Moran_I <- Moran[1]
#       Moran_p <- Moran[2]
#       
#       results <- rbind(results, data.frame(Repetition = rep,
#                                            Fold = geoFold,
#                                            Accuracy = accuracy,
#                                            MCC = mcc,
#                                            TP = TP,
#                                            TN = TN,
#                                            FP = FP,
#                                            FN = FN,
#                                            Precision = Precision,
#                                            Recall = Recall,
#                                            F1_Score = F1_Score,
#                                            Kappa = kappa,
#                                            Moran_I = Moran_I,
#                                            Moran_p = Moran_p))
#     }
#   }
#   
#   return(results)
# }



# ik_spCV(blanca_train_sf, repetitions, k, seedN)












########
######## wraper
########



# spCV_wrapper <- function(sf, formula, repetitions, k, seedN) {
#   
#   models <- c("ranger", "gbm", "kknn", "lr", "gwr", "ik")
#   results <- data.frame()
#   
#   for (model in models) {
#     cv_func <- get(paste0(model, "_spCV"))
#     cv_results <- cv_func(sf, formula, repetitions, k, seedN)
#     cv_results$model <- paste0(model, "_sp")
#     results <- rbind(results, cv_results)
#   }
#   
#   return(results)
# }


# repetitions <- 5
# k <- 10
# seedN <- 123
# formula <- CLASS ~ ndsi + DEM
# 
# results_spCV <- spCV_wrapper(blanca_train_sf, formula, repetitions, k, seedN)
# 
# results_spCV <- results_spCV %>%
#   mutate(spatial = 1)


# juntar sp vs nsp



# results_spCV + results_nspCV


# results <- bind_rows(results_nspCV, results_spCV)
# x <- results %>% dplyr::filter(Accuracy != 1)
# x$model <- as.factor(x$model)
# x$spatial <- as.factor(x$spatial)

# # results <- lr_spCV(blanca_train_sf, formula, repetitions, k, seedN)
# x <- results_spCV %>% dplyr::filter(Accuracy != 1)
# x$model <- as.factor(x$model)


# 
# ggplot(x, aes(x = MCC, y = model)) +
#   geom_boxplot(alpha = 0, width = .5, colour = "black") +
#   geom_jitter(alpha = 0.3, color = "tomato", width=0.2, height=0, size = 2.0) +
#   ylab("MCC") +
#   xlab("modelo sp CV") + 
#   ggtitle("  ",  
#           subtitle = " ") +
#   theme(plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5)) +
#   theme(axis.text=element_text(size=8),
#         axis.title=element_text(size=11)) +
#   theme_bw()


########
########
########

# ggplot(x, aes(x = MCC, y = model, fill = spatial)) +
#   geom_boxplot(alpha = 0, width = .5, colour = "black") +
#   # geom_jitter(alpha = 0.3, color = "tomato", width=0.2, height=0, size = 2.0) +
#   geom_jitter(aes(color = model), alpha = 0.3, width = 0.2, height = 0, size = 2.0) +
#   # scale_fill_manual(values = c("red", "blue")) +
#   # scale_color_manual(values = c("tomato", "blue")) +
#   ylab("MCC") +
#   xlab("modelo sp CV") + 
#   ggtitle("  ",  
#           subtitle = " ") +
#   theme(plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5)) +
#   theme(axis.text=element_text(size=8),
#         axis.title=element_text(size=11)) +
#   theme_bw()
# 
# # GRID PLOT
# multiplot <- do.call(gridExtra::grid.arrange, c(mapping_plots, ncol = 3))
# print(multiplot)
# 


########
########
########


# repetitions <- 5
# set.seed(123) 
# 
# for (rep in seq_len(repetitions)) {
#   
#   # cat("Rep", rep, "\n")
#   
#   kmeans_result <- kmeans(coordinates, centers = 10)
#   data$Cluster <- kmeans_result$cluster
#   flds = unique(data$Cluster)
#   
#   
#   for (j in 1:length(flds)){
#     
#     id <-  which(data$Cluster == levels(as.factor(flds))[j]) # j=1 -> test cluster 1, j=2 -> test cluster 2 ...
#     
#     fold_col <- paste0("ValidationFold_", j, "_", repetition)
#     data[[fold_col]] <- ifelse(row.names(data) %in% id, 1, 0)
#     
# 
#     
#   }
# }
# 
# 
# colnames(data)
# 
# 
# 
# ggplot(data[,36:45]) +
#   geom_point() +
#   facet_wrap(vars(starts_with("ValidationFold")))
#   

