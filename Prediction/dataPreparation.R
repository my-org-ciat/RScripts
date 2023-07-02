# ------------------------------------------------------------------------------
# Reads csv & raster data to predict fertilizer
# predictFertilizer <- function(rast_path, csv_path, csv, column){

# clear working environment
 rm(list = ls())

  library(rgdal)
  library(raster)
  library(caret)
  library(ranger)
  library(dplyr)
  library(e1071)
  library(sf)
  library(randomForest)
  library(spatial.tools)
  library(sp)
  library(Metrics)
  
# ------------------------------------------------------------------------------ 
# list raster files
  setwd("D:/pred_test/cov")
  rfiles <- list.files(path = ".", pattern = ".tif$", all.files = T)
  rasters <- lapply(rfiles, raster)

# ------------------------------------------------------------------------------ 
# sync and stack rasters
  for(i in 1:length(rasters)){
    rasters[[i]] <-
      spatial_sync_raster(rasters[[i]], rasters[[6]], method = 'bilinear') # dem as reference
  }
  grid <- stack(rasters)
  
# ------------------------------------------------------------------------------ 
# rasterize aoi and Mask rasters
  aoi <- readOGR(dsn = "D:/pred_test", layer = "habru")
  r <- raster()
  r <- spatial_sync_raster(r, grid)
  # aoi <- spTransform(aoi, crs(grid))
  aoi <- rasterize(aoi, r)
  grid2 <- mask(grid, aoi)
  
  #save the mask grid
  save(grid2, file = paste0("D:/pred_test/output/","masked_covariateStack", ".rda"))
  
  grid2 <- as(grid, "SpatialGridDataFrame") # used later for prediction
  grid2$landform <- as.factor(grid2$landform)
# ------------------------------------------------------------------------------
# read csv data and convert to spatial
  points <- read.csv(file = "D:/pred_test/test_data.csv", header = T, sep = ",")
  # ------------------------------------------------------------------------------
  # create a data frame that contains N fertilizer and covariates
  imp_csv_data_N <- dplyr::select(points, "N")
  
  coordinates(points) <- ~x+y
  # gridded(grid3) <- ~s1+s2
  crs(points) <- crs(grid)
  # proj4string(points) <- CRS("+init=epsg:4326")

# ------------------------------------------------------------------------------
# extracting values by points
  grid_val <- raster::extract(grid, points)
  
# renaming columns
  tmax_data <- dplyr::select(grid_val, contains("tmax"))
  for(i in 1:ncol(tmax_data)){
    colnames(tmax_data)[i] <- paste0("tmax", i)
  }
  
  tmin_data <- dplyr::select(grid_val, contains("tmin"))
  for(i in 1:ncol(tmin_data)){
    colnames(tmax_data)[i] <- paste0("tmin", i)
  }
  
  prcp_data <- dplyr::select(grid_val, contains("prcp"))
  for(i in 1:ncol(prcp_data)){
    colnames(prcp_data)[i] <- paste0("prcp", i)
  }
  
  srad_data <- dplyr::select(grid_val, contains("srad"))
  for(i in 1:ncol(srad_data)){
    colnames(srad_data)[i] <- paste0("srad", i)
  }
  other_data <- # other than climatic data
    dplyr::select(data,-(contains("prcp"))) %>% 
    select(-(contains("tmin"))) %>% select(-(contains("srad"))) %>% 
    select(-(contains("tmax")))
  clim_data <- data.frame(tmax_data, tmin_data, prcp_data, srad_data)
  grid_val <- data.frame(other_data, clim_data)
  colnames(grid_val)
  
  save(grid_val, file = paste0("D:/pred_test/output/","extract_points", ".rda"))  
  data_pred_N <- cbind(points@coords, imp_csv_data_N, grid_val)
  data_pred_N$N <- as.numeric(data_pred_N$N)
  data_pred_N$landform <- as.factor(data_pred_N$landform)
  grid_data_N <- unique(na.omit(data_pred_N[, 3:ncol(data_pred_N)])) #removing NAs and duplicates

# ------------------------------------------------------------------------------
# Create training and testing dataset. it is different for different response
# variables
inTrain_N <- createDataPartition(y = grid_data_N$N, p = .75, list = FALSE)
training_N <- grid_data_N[inTrain_N,]
testing_N <- grid_data_N[-inTrain_N,]

# ------------------------------------------------------------------------------ 
# fertilizers for model calibration and validation
  
  set.seed(7)
  
  
  rf_fitControl <- trainControl(method = "repeatedcv",
                             number = 5,
                             repeats = 3)
# the error in tunegrid is solved by 
# install.packages("caret", dependencies = c("Depends", "Suggests"))
  
  rf_tuneGrid <- expand.grid(.mtry = c(8:12),
                             .splitrule =  "maxstat",
                             .min.node.size = c(20))
  # mtry <- c(18, 20, 22) 
  # rf_tuneGrid <- expand.grid(.mtry = mtry,
  #                            .splitrule =  "maxstat", 
  #                            .min.node.size = c(20))
  set.seed(7)
  mod_fit_N <- train(
    N ~ .,
    data = training_N,
    method = "ranger",
    trControl = rf_fitControl,
    importance = 'impurity',
    tuneGrid = rf_tuneGrid,
    preProcess = c('scale', 'center')
  )
  
  mod_fit_N$finalModel
  summary(mod_fit_N)
  saveRDS(mod_fit_N, "mod_fit_N.rds")
# Plot var importance
# ------------------------------------------------------------------------------
  var_imp <- varImp(mod_fit_N)
  plot(var_imp)


# ------------------------------------------------------------------------------  
# Prediction for validation
  pred_test_N <- predict(mod_fit_N, testing_N)
  val <- data.frame(testing_N, pred_test_N)
  
#checking rmse
  error <- rmse(testing_N$N, pred_test_N)

# predict using grid dataframe
  pred_grid_N <- predict(mod_fit_N, grid2)
  
# Predict on the raster stack and write as a file
  pred_N_grid <-
    predict(
      grid,
      mod_fit_N,
      progress = "window",
      overwrite = T,
      filename = "N_pred.tif"
    )
#}