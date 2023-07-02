# ------------------------------------------------------------------------------
# Reads csv & raster data to predict fertilizer
# predictFertilizer <- function(rast_path, csv_path, csv, column){
  library(rgdal)
  library(raster)
  library(caret)
  library(ranger)
  library(dplyr)
  library(e1071)
  library(sf)
  library(randomForest)
  library(spatial.tools)
  
# ------------------------------------------------------------------------------ 
# list raster files
  rfiles <- list.files(path = ".", pattern = ".tif$", all.files = T)
  rasters <- lapply(rfiles, raster)

# ------------------------------------------------------------------------------ 
# sync and stack rasters
  for(i in 1:length(rasters)){
    rasters[[i]] <-
      spatial_sync_raster(rasters[[i]], rasters[[5]], method = 'bilinear') # dem as reference
  }
  grid <- stack(rasters)
  
# ------------------------------------------------------------------------------ 
# Mask rasters
  eth_mask <- readOGR(dsn = ".", layer = "eth")
  # mask <- raster("D:/Test/cov/eth_raster/eth.tif") 
  # m_raster <- raster()
  # extent(m_raster) <- extent(m)
  # mask = rasterize(m, r)
  
  grid2 <- crop(grid, eth_mask)
  grid3 <- as(grid2, "SpatialGridDataFrame") # used later for prediction
  # grid4 <- as(grid3, Class = "data.frame")
  #grid3 <- as.data.frame(grid3) # Error: cannot allocate vector of size 181.2 Mb
# ------------------------------------------------------------------------------
# read csv data and convert to spatial
  data <- read.csv(file = "D:/Test/test_data.csv", header = T, sep = ",")
  # ------------------------------------------------------------------------------
  # create a data frame that contains N fertilizer and covariates
  imp_csv_data_N <- dplyr::select(data, "N")
  imp_csv_data_S <- dplyr::select(data, "S")
  imp_csv_data_P <- dplyr::select(data, "P")
  imp_csv_data_K <- dplyr::select(data, "K")
  imp_csv_data_yield <- dplyr::select(data, "Yield")
  
  coordinates(data) <- ~x+y
  # gridded(grid3) <- ~s1+s2
  crs(data) <- crs(grid3)

# ------------------------------------------------------------------------------
# extracting values by points
  grid_val <- raster::extract(grid2, data)
  grid_val <- as.data.frame(grid_val)
  grid_val$landform <- as.factor(grid_val$landform)
  
  data_pred_N <- cbind(data@coords, imp_csv_data_N, grid_val)
  data_pred_N$N <- as.numeric(data_pred_N$N)
  data_pred_P <- cbind(data@coords, imp_csv_data_P, grid_val)
  data_pred_P$P <- as.numeric(data_pred_P$P)
  data_pred_K <- cbind(data@coords, imp_csv_data_K, grid_val)
  data_pred_K$K <- as.numeric(data_pred_K$K)
  data_pred_S <- cbind(data@coords, imp_csv_data_S, grid_val)
  data_pred_S$S <- as.numeric(data_pred_S$S)
  data_pred_yield <- cbind(data@coords, imp_csv_data_yield, grid_val)
  data_pred_yield$yield <- as.numeric(data_pred_yield$yield)
  

  #removing NAs and duplicates
  
  grid_data_N <- unique(na.omit(data_pred_N[, 3:ncol(grid_data_N)]))
  grid_data_P <- unique(na.omit(data_pred_P[, 3:ncol(grid_data_P)]))
  grid_data_K <- unique(na.omit(data_pred_K[, 3:ncol(grid_data_K)]))
  grid_data_S <- unique(na.omit(data_pred_S[, 3:ncol(grid_data_S)]))
  grid_data_yield <- unique(na.omit(data_pred_yield[, 3:ncol(grid_data_yield)]))

# ------------------------------------------------------------------------------
# Create training and testing dataset. it is different for different response
# variables
inTrain_N <- createDataPartition(y = grid_data_N$N, p = .75, list = FALSE)
inTrain_P <- createDataPartition(y = grid_data_P$P, p = .75, list = FALSE)
inTrain_K <- createDataPartition(y = grid_data_K$K, p = .75, list = FALSE)
inTrain_S <- createDataPartition(y = grid_data_S$S, p = .75, list = FALSE)


training_N <- grid_data_N[inTrain_N,]
training_P <- grid_data_P[inTrain_P,]
training_K <- grid_data_K[inTrain_K,]
training_S <- grid_data_S[inTrain_S,]


testing_N <- grid_data_N[-inTrain_N,]
testing_P <- grid_data_P[-inTrain_P,]
testing_K <- grid_data_K[-inTrain_K,]
testing_S <- grid_data_S[-inTrain_S,]


# ------------------------------------------------------------------------------ 
# fertilizers for model calibration and validation
  
  set.seed(7)
  
  
rf_fitControl <- trainControl(method = "repeatedcv",
                             number = 5,
                             repeats = 1)

  rf_tuneGrid <- expand.grid(.mtry = c(3, 5, 10),
                             .splitrule =  "maxstat",
                             .min.node.size = c(20))
  # mtry <- c(18, 20, 22) 
  # rf_tuneGrid <- expand.grid(.mtry = mtry,
  #                            .splitrule =  "maxstat", 
  #                            .min.node.size = c(20))
  
  mod_fit_N <- train(
    N ~ .,
    data = training_N,
    method = "ranger",
    trControl = rf_fitControl,
    importance = 'impurity',
    tuneGrid = rf_tuneGrid,
    preProcess = c('scale', 'center')
  )

  mod_fit_P <- train(
    P ~ .,
    data = training_P,
    method = "ranger",
    trControl = rf_fitControl,
    importance = 'impurity',
    tuneGrid = rf_tuneGrid,
    preProcess = c('scale', 'center')
  )

  mod_fit_K <- train(
    K ~ .,
    data = training_K,
    method = "ranger",
    trControl = fitControl,
    importance = 'impurity',
    tuneGrid = rf_tuneGrid,
    preProcess = c('scale', 'center')
  )

  mod_fit_S <- train(
    S ~ .,
    data = training_S,
    method = "ranger",
    trControl = fitControl,
    importance = 'impurity',
    tuneGrid = rf_tuneGrid,
    preProcess = c('scale', 'center')
  )
  
# Plot var importance
# ------------------------------------------------------------------------------
  
  
# ------------------------------------------------------------------------------  
# Prediction 
  pred_N <- predict(mod_fit_N, data = grid_data_N)
  
  
  
  # pred_P <- predict(mod_fit_P, data = grid_data_P)
  # pred_K <- predict(mod_fit_K, data = grid_data_K)
  # pred_S <- predict(mod_fit_S, data = grid_data_S)
  
  
# ------------------------------------------------------------------------------  
# Yield
  grid_data_yield <- data.frame(grid_data, pred_N, pred_P, pred_K,  pred_S)
  grid_data_yield2 <- data.frame(grid_data_yield, pred_N, pred_P, pred_K,  pred_S)
  grid_data_yield3 <- unique(na.omit(grid_data_yield2))
  inTrain_yield <- createDataPartition(y = grid_data_yield3$yield, p = .3, list = FALSE)
  training_yield <- grid_data_yield3[inTrain_yield,]
  testing_yield <- grid_data_yield3[-inTrain_yield,]
  
  mod_fit_yield <- train(
    Yield ~ .,
    data = grid_data_yield,
    method = "rf",
    trControl = fitControl,
    importance = 'impurity',
    tuneGrid = rf_tuneGrid,
    preProcess = c('scale', 'center')
  )
  
  dir.create("result", showWarnings = F)
  writeRaster(
    resp_col,
    file = paste("./result", resp_col, sep = "/"),
    format = "GTiff",
    overwrite = T
  )
#}