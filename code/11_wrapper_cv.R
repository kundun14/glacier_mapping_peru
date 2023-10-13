########
######## librerias
########

pacman::p_load(sf, sp, glmnet, ranger, kknn, gbm, spaMM, gstat,
               spgwr, spdep, caret , Metrics, mccr,
               geoR, raster, rgeos, sperrorest, ggplot2, gridExtra, tidyverse)


source("./funciones/nspCV_conf.R")
source("./funciones/spCV_conf.R")


########
######## INPUT DATA
########

list_files <- list.files(path = "./data/sampling_points_split/",
                         recursive = TRUE,
                         full.name = TRUE,
                         pattern = ".*_train\\.gpkg$")  # Only include files ending with "_train.gpkg"

list_gpkg <- lapply(list_files, FUN = sf::read_sf)

bandwidths <- read_rds("bandwidths.RDS")

########
######## ARGUEMENTS
########

aoi <- c("Blanca","Central","Huallanca",
         "Huayhuasha","Huaytapallana","LaRaya",
         "LaViuda","Raura","Urubamba",
         "Vilcabamba","Vilcanota")


# names(list_gpkg) <- cordilleras

k <- 5 # 10 esta bien 
repetitions <- 10 # idealmente 100 pero basta con 10, serian 100 ajustes por algoritmo/data set
# aoi_ <- aoi[1:2]
# list_data <- list_gpkg[1:2]
# band_list <- bandwidths[1:2]
formula <- CLASS ~ ndsi + DEM
seedN <- 123



#################
################# CV_wrapper
#################



CV_wrapper <- function(cv_type , sf_list, aoi, formula, repetitions, k, seedN, bandwidths) {
  
  models <- c("ndsi" ,"lr", "gwr" , "kknn", "ranger", "gbm")
  results <- data.frame()
  
  for (i in seq_along(sf_list)) {
    
    sf <- sf_list[[i]]
    current_aoi <- aoi[i]
    bandwidth <- bandwidths[[i]]
    
    
    if (cv_type == "nonspatial") {
      
      for (model in models) {
        cv_func <- get(paste0(model, "_nspCV"))
        cv_results <- cv_func(sf, formula, repetitions, k, seedN, bandwidth)
        cv_results$model <- paste0(model, "_nsp")
        cv_results$aoi <- rep(current_aoi, nrow(cv_results))
        cv_results$spatial <- rep("Non spatial CV", nrow(cv_results))
        results <- rbind(results, cv_results)
      }
    }
    
    else if (cv_type == "spatial") {
      
      for (model in models) {
        cv_func <- get(paste0(model, "_spCV"))
        cv_results <- cv_func(sf, formula, repetitions, k, seedN, bandwidth)
        cv_results$model <- paste0(model, "_sp")
        cv_results$aoi <- rep(current_aoi, nrow(cv_results))
        cv_results$spatial <- rep("Spatial CV", nrow(cv_results))
        results <- rbind(results, cv_results)
      }
    }
  }
  
  return(results)
}

########
######## RUN WRAPPERS
########



results_nspCV <- CV_wrapper(cv_type = "nonspatial", list_gpkg, aoi, formula, repetitions, k, seedN, bandwidths = bandwidths)
results_spCV <- CV_wrapper(cv_type = "spatial", list_gpkg, aoi, formula, repetitions, k, seedN, bandwidths = bandwidths)


########
######## POST PROCESING FOR PLOTTING
########

results_full <- bind_rows(results_nspCV, results_spCV)
results_full <- results_full %>% dplyr::filter(F1_Score < 1)
results_full <- results_full %>% dplyr::filter(Accuracy != 1)

results_full$model <- as.factor(results_full$model)
results_full$spatial <- as.factor(results_full$spatial)
results_full$aoi <- as.factor(results_full$aoi)


write.csv(results_full, "./resultados/benchmarks/benchmarks_5x10.csv")
# results_full <- read_csv("./resultados/benchmarks/benchmarks_5x10.csv")

# TABLA RESUMEN 

results_full

# agrupar por modelo
# agrupar por validacion 
# agrupar por aoi

# ACCURACY

results_summary <- results_full %>%
  group_by(model, aoi, spatial ) %>%
  summarize(mean_accuracy = mean(Accuracy),
            mean_MCC = mean(MCC) ,
            mean_F1 = mean(F1_Score),
            mean_moranI = mean(Moran_I)
            )

#resumen de resultados  

results_summary_agg_mcc <- results_summary %>% arrange(aoi, desc(mean_MCC))
results_summary_agg_f1 <- results_summary %>% arrange(aoi, desc(mean_F1))


write.csv(results_summary_agg_mcc, "./resultados/benchmarks/benchmarks_medias_mcc.csv")
write.csv(results_summary_agg_f1, "./resultados/benchmarks/benchmarks_medias_f1.csv")


# WRITE 
results_summary_agg_mcc$model <- as.factor(results_summary_agg_mcc$model)
results_summary_agg_mcc %>% arrange(aoi)




models_names <- c("Gradient Boosting Machine with non spatial CV",
                  "Gradient Boosting Machine with spatial CV",
                  "Geographically Weighted Logistic Regression with non spatial CV",
                  "Geographically Weighted Logistic Regression with spatial CV",
                  "K-Nearest Neighbor with non spatial CV",
                  "K-Nearest Neighbor with spatial CV",
                  "Logistic Regression with non spatial CV",
                  "Logistic Regression with spatial CV", 
                  "NDSI threshold with non spatial CV",
                  "NDSI threshold with spatial CV",
                  "Random Forest with non spatial CV",
                  "Random Forest with spatial CV")

levels(results_summary_agg_mcc$model) <- models_names


aois <- c("Blanca","Central","Huallanca",
         "Huayhuasha","Huaytapallana","LaRaya",
         "LaViuda","Raura","Urubamba",
         "Vilcabamba","Vilcanota")

for (i in seq(aois)) {
  aoidata <- results_summary_agg_mcc %>%
    ungroup() %>%  
    filter(aoi == aois[i]) %>% 
    select(!c("aoi","spatial")) %>% 
    select(c("model","mean_MCC","mean_F1","mean_accuracy"), everything())
  writexl::write_xlsx(aoidata, paste0("./resultados/benchmarks/benchmarks_",aois[i],".xlsx"))
}

# paste0("./resultados/benchmarks/benchmarks_",aois[1],".csv")



########
######## PLOT RESULTS BAR
########

# factor(results_summary$model, 
#        levels = result$model[order(result$mean_accuracy, decreasing = TRUE)])


# ggplot(results_summary, aes(x = model, y = mean_accuracy, fill = model)) +
#   geom_point(position = position_dodge(width = 0.5), size = 2, alpha = 0.3) +
#   geom_errorbar(aes(ymin = mean_accuracy - sd_accuracy, ymax = mean_accuracy + sd_accuracy),
#                 width = 0.2, position = position_dodge(width = 0.5)) +
#   facet_wrap(~ aoi) +
#   labs(x = "Model", y = "Mean Accuracy") +
#   theme_bw()
# 
# 
# results_summary$model <- factor(results_summary$model, levels = results_summary$model[order(results_summary$mean_accuracy, decreasing = TRUE)])

# Plotting

# ggplot(result, aes(x = model, y = mean_accuracy, fill = model)) +
#   geom_point(position = position_dodge(width = 0.5), size = 3) +
#   geom_errorbar(aes(ymin = mean_accuracy - sd_accuracy, ymax = mean_accuracy + sd_accuracy),
#                 width = 0.2, position = position_dodge(width = 0.5)) +
#   facet_wrap(~ aoi, nrow = 1) +
#   labs(x = "Model", y = "Mean Accuracy") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))



########
######## PLOT RESULTS SCATTER
########

# ACCURACY PLOT 
ggplot(results_full, aes(x = F1_Score, y = model, fill = spatial)) +
  geom_boxplot(alpha = 0, width = .5, colour = "black") +
  # geom_jitter(alpha = 0.3, color = "tomato", width=0.2, height=0, size = 2.0) +
  geom_jitter(aes(color = spatial), alpha = 0.2, width = 0.2, height = 0, size = 2.0) +
  # scale_fill_manual(values = c("red", "blue")) +
  scale_color_manual(values = c("tomato", "blue")) +
  ylab("Models") +
  xlab("CV F1_Score") + 
  ggtitle("  ",  
          subtitle = " ") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=11)) +
  facet_wrap(~aoi)+
  theme_bw()


# F1 PLOT 
ggplot(results_full, aes(x = F1_Score, y = model, fill = spatial)) +
  geom_boxplot(alpha = 0, width = .5, colour = "black") +
  # geom_jitter(alpha = 0.3, color = "tomato", width=0.2, height=0, size = 2.0) +
  geom_jitter(aes(color = spatial), alpha = 0.2, width = 0.2, height = 0, size = 2.0) +
  # scale_fill_manual(values = c("red", "blue")) +
  scale_color_manual(values = c("tomato", "blue")) +
  ylab("Models") +
  xlab("CV F1_Score") + 
  ggtitle("  ",  
          subtitle = " ") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=11)) +
  facet_wrap(~aoi)+
  theme_bw()

#MCC
ggplot(results_full, aes(x = MCC, y = model, fill = spatial)) +
  geom_boxplot(alpha = 0, width = .5, colour = "black") +
  # geom_jitter(alpha = 0.3, color = "tomato", width=0.2, height=0, size = 2.0) +
  geom_jitter(aes(color = spatial), alpha = 0.2, width = 0.2, height = 0, size = 2.0) +
  # scale_fill_manual(values = c("red", "blue")) +
  scale_color_manual(values = c("tomato", "blue")) +
  ylab("Models") +
  xlab("CV MCC") + 
  ggtitle("  ",  
          subtitle = " ") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=11)) +
  facet_wrap(~aoi)+
  theme_bw()





####
#### CLASIFICATION REGIONS
####


# PARA SPATIAL AND NON SPATIAL MODELS


####
####
####
####

# classes <- ifelse(x^2 + y^2 > 60^2, "blue", "orange")
# classes.test <- ifelse(x.test^2 + y.test^2 > 60^2, "blue", "orange")
# 
# grid <- expand.grid(x=1:100, y=1:100)
# classes.grid <- knn(train.df, grid, classes, k=25, prob=TRUE)  # note last argument
# prob.grid <- attr(classes.grid, "prob")
# prob.grid <- ifelse(classes.grid == "blue", prob.grid, 1 - prob.grid)
# 
# # plot the boundary
# contour(x=1:100, y=1:100, z=matrix(prob.grid, nrow=100), levels=0.5,
#         col="grey", drawlabels=FALSE, lwd=2)
# # add points from test dataset
# points(test.df, col=classes.test)
# 
# 
# ## PASOS
# 
# 
# REENTRENAR LOS MODELOS EN LA DATA TEST DATA
# USAR LOS MODELOS RENTRENADO EN UN AMLLA DE PREDICCION CON DEM Y NDSI
# ESTIMAR BOUNDARY 
# PLOT
# 





