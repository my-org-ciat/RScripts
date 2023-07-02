# ------------------------------------------------------------------------------
# Reads csv & raster data to predict fertilizer
predictFertilizer <- function(rast_path, csv_path, csv, column){
  library(rgdal)
  library(raster)
  # library(caret)
  library(randomForest)
  library(spatial.tools)
  
# ------------------------------------------------------------------------------ 
# list raster files
  flist <- list.files(path = rast_path, pattern = ".tif$")
  rasters <- lapply(flist, raster)

# ------------------------------------------------------------------------------ 
# sync and stack rasters
  for(i in 1:length(rasters)) {
    rasters[[i]] <-
      spatial_sync_raster(rasters[[i]], rasters[[3]], method = 'bilinear') # dem as reference
  }
  grid <- stack(rasters)

# ------------------------------------------------------------------------------
# read csv data and convert to spatial
  data <- read.csv(paste0(csv_path, csv), header = T, sep = ",")
  coordinates(data) <- ~lat+long
  new_crs <- crs(grid)
  crs(data) <- new_crs

# ------------------------------------------------------------------------------
# extracting values by points
  grid_val <- extract(grid, data)
  data <- as.data.frame(data)
  grid_data <- cbind.data.frame(grid_val, data) 
  grid_data <- unique(na.omit(grid_data)) # remove na and lat&long duplicates
  # pred_data <- dplyr::select(grid_data, -c(x, y))

# ------------------------------------------------------------------------------
# Create training and testing dataset 
  set.seed(321)
  training <- sample(nrow(grid_data), 0.8 * nrow(grid_data))
  data_cal <- grid_data[training, ]
  data_val <- grid_data[-training, ]

# ------------------------------------------------------------------------------ 
# create a response variable
  resp_col <- data_cal$column

# ------------------------------------------------------------------------------ 
# fertilizers for model calibration and validation
  grid_cal <- data_cal[c(data_cal$N, data_cal$P, data_cal$K,data_cal$S)]
  grid_val <- data_val[c(data_cal$N, data_cal$P, data_cal$K,data_cal$S)]

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