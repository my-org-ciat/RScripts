# ------------------------------------------------------------------------------
# Reads csv & raster data to predict fertilizer
predictFertilizer <- function(rast_path, csv_path, csv, column){
  library(rgdal)
  library(raster)
  library(caret)
  library(e1071)
  library(randomForest)
  library(spatial.tools)
  
# ------------------------------------------------------------------------------ 
# list raster files
  flist <- list.files(path = rast_path, pattern = ".tif$", all.files = T)
  rasters <- lapply(flist, raster)

# ------------------------------------------------------------------------------ 
# sync and stack rasters
  for(i in 1:length(rasters)){
    rasters[[i]] <-
      spatial_sync_raster(rasters[[i]], rasters[[3]], method = 'bilinear') # dem as reference
  }
  grid <- stack(rasters)

# ------------------------------------------------------------------------------
# read csv data and convert to spatial
  data <- read.csv(paste0(csv_path, csv), header = T, sep = ",")
  coordinates(data) <- ~Northing + Easting
  crs(data) <- crs(grid)

# ------------------------------------------------------------------------------
# extracting values by points
  grid_val <- raster::extract(grid, data, sp = T)
  data <- as.data.frame(grid_val)
  
# ------------------------------------------------------------------------------
# create a data frame that contains N fertilizer and covariates
data_pred <- cbind(data$N, grid)
grid_data <- unique(na.omit(grid_data))
  # grid_data <- cbind.data.frame(grid_val, data) 
  # grid_data <- unique(na.omit(grid_data)) # remove na and lat&long duplicates
  # pred_data <- dplyr::select(grid_data, -c(x, y))

  
# ------------------------------------------------------------------------------
# Create training and testing dataset 
inTrain <- createDataPartition(y = grid_data$N, p = .3, list = FALSE)
training <- grid_data[ inTrain,]
testing <- grid_data[-inTrain,]

# ------------------------------------------------------------------------------ 
# fertilizers for model calibration and validation
  
  set.seed(7)
  
  fitControl <- trainControl(method = "repeatedcv",
                             number = 5,
                             repeats = 1)
  
  rf_tuneGrid <- expand.grid(.mtry = c(18, 20, 22 ),
                             .splitrule =  "maxstat", 
                             .min.node.size = c(20, 30, 40))
  
  rf_tuneGrid <- expand.grid(.mtry = c(16),
                             .splitrule =  "maxstat", 
                             .min.node.size = c(20))
  
  mod_fit <- train(
    N ~ .,
    data = data_cal,
    method = "rf",
    trControl = fitControl,
    importance = 'impurity',
    tuneGrid = rf_tuneGrid,
    preProcess = c('scale', 'center')
  )

# ------------------------------------------------------------------------------  
# Fit Random Forest models 
  rf <- randomForest(x = grid_cal, y = resp_col, importance = TRUE) #log(resp_col)

# ------------------------------------------------------------------------------
# Variable Importance plot
  varImp(rf)
  varImpPlot(rf, sort = TRUE, main = "RF model Variable Importance")

# ------------------------------------------------------------------------------ 
# correlation_plot
  plot(rf$predicted, rf$y, xlab="Predicted", ylab="Observed")
  abline(0,1)

# ------------------------------------------------------------------------------ 
# validation_stats
# MSE
  round(rf$mse[rf$ntree],2)

# ------------------------------------------------------------------------------
# r-squared
  round(rf$rsq[rf$ntree],2)

# ------------------------------------------------------------------------------ 
# RMSE
  round(sqrt(rf$mse[rf$ntree]),1)

# ------------------------------------------------------------------------------
# cumulative_plots
  plot(rf$mse, xlab = "trees", ylab = "MSE")
  plot(rf$rsq, xlab = "trees", ylab = "Rsquared")

# ------------------------------------------------------------------------------  
# spatial prediction and exporting
  resp_col <- predict(grid, rf)
  # resp_col <- exp(resp_col) # Back transform to orginal values
  
  plot(resp_col)

  dir.create("result", showWarnings = F)
  writeRaster(
    resp_col,
    file = paste("./result", resp_col, sep = "/"),
    format = "GTiff",
    overwrite = T
  )
}