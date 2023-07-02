# ------------------------------------------------------------------------------
# Reads csv & raster data to predict fertilizer
# predictFertilizer <- function(rast_path, csv_path, csv, resp_col){

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
  library(ggplot2)
  
# ------------------------------------------------------------------------------ 
# list raster files
  setwd("D:/pred_test/cov")
  rfiles <- list.files(path = ".", pattern = ".tif$", all.files = T)
  rasters <- lapply(rfiles, raster)

# ------------------------------------------------------------------------------ 
# sync and stack rasters
  for(i in 1:length(rasters)){
    if("dem" %in% names(rasters[[i]])){ # setting dem as a reference
      dem <- rasters[[i]] 
    }
    }
  
  for(i in 1:length(rasters)){
    rasters[[i]] <-
      spatial_sync_raster(rasters[[i]], dem, method = 'bilinear')
  }
  grid <- stack(rasters)
  
# ------------------------------------------------------------------------------ 
# Mask rasters
  aoi <- readOGR(dsn = "D:/pred_test", layer = "habru")
  new_crs <- proj4string(grid)
  aoi <- spTransform(aoi, new_crs)
  grid2 <- mask(grid, aoi)
  grid2 <- crop(grid, aoi)
  
#save the mask grid
  save(grid2, file = paste0("./output/","masked_covariateStack", ".rda"))
  
  # load("D:/pred_test/output/masked_covariateStack.rda")
  
  grid3 <- as(grid2, "SpatialGridDataFrame") #used later for prediction
  grid4 <- as.data.frame(grid3)
  grid4$landform <- as.factor(grid4$landform)

# ------------------------------------------------------------------------------
# rename columns of spatial grid data frame
  tmax_data_sgdf <- dplyr::select(grid4, contains("tmax"))
  for(i in 1:ncol(tmax_data_sgdf)){
    colnames(tmax_data_sgdf)[i] <- paste0("tmax", i)
  }
  
  tmin_data_sgdf <- dplyr::select(grid4, contains("tmin"))
  for(i in 1:ncol(tmin_data_sgdf)){
    colnames(tmin_data_sgdf)[i] <- paste0("tmin", i)
  }
  
  prcp_data_sgdf <- dplyr::select(grid4, contains("prcp"))
  for(i in 1:ncol(prcp_data_sgdf)){
    colnames(prcp_data_sgdf)[i] <- paste0("prcp", i)
  }
  
  srad_data_sgdf <- dplyr::select(grid4, contains("srad"))
  for(i in 1:ncol(srad_data_sgdf)){
    colnames(srad_data_sgdf)[i] <- paste0("srad", i)
  }
  
# Combine climatic data of new variable names
  clim_data_sgdf <-
    data.frame(tmax_data_sgdf,
               tmin_data_sgdf,
               prcp_data_sgdf,
               srad_data_sgdf)
  
# other than climatic data - sgdf
  other_data_sgdf <- grid4 %>% 
    dplyr::select(-(contains(c("prcp","tmin", "srad", "tmax")))) 

# Creating binary variables from factor variables - from landform
  bin_var <- model.matrix(as.formula(paste0("~","landform","+0")), other_data_sgdf)
  bin_var2 <- as.data.frame(bin_var)
  bin_var3 <- lapply(bin_var2, FUN = factor)
  glimpse(bin_var3)
  bin_var_final <- as.data.frame(bin_var3)
  
  sgdf <- data.frame(other_data_sgdf, clim_data_sgdf, bin_var_final)# spatial grid data frame
  sgdf <- dplyr::select(sgdf, -"landform") # remove original landform
  glimpse(sgdf)
  
# ------------------------------------------------------------------------------
# Rearrange the columns
  sgdf_s1s2 <- dplyr::select(sgdf, "s1", "s2")
  sgdf_other <- dplyr::select(sgdf, -c("s1", "s2"))
  sgdf_final <- data.frame(sgdf_s1s2, sgdf_other)
  sgdf_final <- na.omit(sgdf_final)
  glimpse(sgdf_final)
  
  save(sgdf_final, file = paste0("./output/","sgdf_final_output", ".rda"))
  
# Convert to back to Spatial Grid Data frame
  gridded(sgdf_final) <- ~s1+s2
  proj4string(sgdf_final) <- new_crs
  class(sgdf_final)
  
# ------------------------------------------------------------------------------
# read csv data and convert to spatial
  points <- read.csv(file = "D:/pred_test/test_data.csv", header = T, sep = ",")
# ------------------------------------------------------------------------------
# create a data frame that contains N fertilizer and covariates
  imp_csv_data_N <- dplyr::select(points, "N")
  
  coordinates(points) <- ~x+y
  proj4string(points) <- new_crs

# ------------------------------------------------------------------------------
# extracting values by points
  grid_val <- over(points, sgdf_final) #overlay
  grid_val2 <- as.data.frame(grid_val)
  glimpse(grid_val2)
  
  save(grid_val2, file = paste0("./output/","extracted_values_pts", ".rda"))
  
# ------------------------------------------------------------------------------
# binding response variable and predictor data
  data_pred_N$N <- as.numeric(data_pred_N$N)
  data_pred_N <- cbind(points@coords, imp_csv_data_N, grid_val2)
  glimpse(data_pred_N)
  grid_data_N <- unique(na.omit(data_pred_N[, 3:ncol(data_pred_N)])) #removing NAs and duplicates
  
  save(grid_data_N, file = paste0("./output/","regression_matrix", ".rda"))

# ------------------------------------------------------------------------------
# Create training and testing data set
inTrain_N <- createDataPartition(y = grid_data_N$N, p = 0.75, list = FALSE)
training_N <- grid_data_N[inTrain_N,]
testing_N <- grid_data_N[-inTrain_N,]

# ------------------------------------------------------------------------------ 
# model calibration and validation
  set.seed(7)
  
  
  rf_fitControl <- trainControl(method = "repeatedcv",
                             number = 5,
                             repeats = 3)
  
  rf_tuneGrid <- expand.grid(.mtry = c(10:15),
                             .splitrule =  "maxstat",
                             .min.node.size = c(20))
  
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
  
# Define rfe control using a random forest selection function
  control <- rfeControl(functions = rfFuncs, # random forest
                        method = "repeatedcv", # repeated cv
                        repeats = 5, # number of repeats
                        number = 10) # number of folds
  result_rfe1 <- rfe(x = grid_data_N[inTrain_N,], 
                     y = grid_data_N[inTrain_N], 
                     sizes = c(1:34),
                     rfeControl = control)
  
# Plot var importance
# ------------------------------------------------------------------------------
  var_imp <- varImp(mod_fit_N)
  plot(var_imp)
  
  ggsave(
    paste0("./output/", "variable_importance", ".png"),
    plot = var_imp,
    dpi = 100
  )
# ------------------------------------------------------------------------------  
# Prediction for validation
  pred_test_N <- predict(mod_fit_N, testing_N)
  val <- data.frame(testing_N, pred_test_N)
  
#checking rmse
  error <- rmse(testing_N$N, pred_test_N)

# predict using spatial grid data frame
  pred_grid_N <- predict(mod_fit_N, sgdf_final)
  
# converting to spatial grid data frame Exporting the prediction as raster
  pred_grid_N_sp <-
    data.frame(x = sgdf_final$s1, y = sgdf_final$s2, pred_grid_N)
  gridded(pred_grid_N_sp) <- ~x+y
  proj4string(pred_grid_N_sp) <- new_crs
  
  writeGDAL(
    pred_grid_N_sp,
    fname = paste0("./output/", "predicted_N.tif"),
    drivername = "Gtiff",
    type = "Float32",
    options = "COMPRESS=DEFLATE"
  )