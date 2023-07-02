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
  grid2 <- crop(grid, aoi)#removes NA values
  
#save the mask grid
  dir.create("output2")
  save(grid2, file = paste0("./output2/","masked_covariateStack", ".rda"))
  
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
# excluding zero & near zero variance variables
  nzv <- nearZeroVar(sgdf, saveMetrics = TRUE) #df of predictor info returned
  head(nzv)
  summary(nzv$zeroVar)
  summary(nzv$nzv)
  sgdf <- sgdf[,!nzv[,4]]#nzv stored in the 4th column of nzv - rmove
  colnames(sgdf)
# ------------------------------------------------------------------------------
# Rearrange the columns
  sgdf_s1s2 <- dplyr::select(sgdf, "s1", "s2")
  sgdf_other <- dplyr::select(sgdf, -c("s1", "s2"))
  sgdf_final <- data.frame(sgdf_s1s2, sgdf_other)
  sgdf_final <- unique(na.omit(sgdf_final))
  glimpse(sgdf_final)
  
  save(sgdf_final, file = paste0("./output2/","sgdf_final_output", ".rda"))
  
# Convert to back to Spatial Grid Data frame
  gridded(sgdf_final) <- ~s1+s2
  proj4string(sgdf_final) <- new_crs
  class(sgdf_final)
  
# ------------------------------------------------------------------------------
# read csv data and convert to spatial
  points <- read.csv(file = "D:/pred_test/test_data.csv", header = T, sep = ",")
  
# ------------------------------------------------------------------------------
# 
  
  coordinates(points) <- ~x+y
  proj4string(points) <- new_crs

# ------------------------------------------------------------------------------
# extracting values by points
  grid_val <- over(points, sgdf_final) #overlay
  grid_val2 <- as.data.frame(grid_val)
  glimpse(grid_val2)
  
  save(grid_val2, file = paste0("./output2/","extracted_values_pts", ".rda"))
  
# ------------------------------------------------------------------------------
# binding response variables and predictor data
  npks <- dplyr::select(as.data.frame(points), c("N","P","K","S"))
  for(i in 1:ncol(npks)){
    npks[,i] <- as.numeric(npks[,i]) #convert to numeric
  }
  data_pred_npks <- cbind(points@coords, npks, grid_val2)
  data_pred_npks <- unique(na.omit(data_pred_npks))
  glimpse(data_pred_npks)
  cov_npks <- data_pred_npks[, 7:ncol(data_pred_npks)]
  save(cov_npks, file = paste0("./output2/","regression_matrix", ".rda"))

# ------------------------------------------------------------------------------
# Create training and testing data set
  mtry <- as.integer((ncol(cov_npks))/3)
  mtry <- c((mtry-2):(mtry+2))

# ------------------------------------------------------------------------------ 
# model calibration and validation
  set.seed(7)
  
  
  rf_fitControl <- trainControl(method = "repeatedcv",
                             number = 5,
                             repeats = 3)
  
  rf_tuneGrid <- expand.grid(.mtry = mtry,
                             .splitrule =  "maxstat",
                             .min.node.size = c(20))
  
  for(i in 1:ncol(npks)){
    set.seed(7)
    resp_var <- npks[,i]
    data_pred <- cbind(resp_var, cov_npks)
    inTrain <- createDataPartition(y = resp_var, p = 0.75, list = FALSE)
    training <- data_pred[inTrain,]
    testing <- data_pred[-inTrain,]
    
      mod_fit <- train(
        resp_var ~ .,
        data = training,
        method = "ranger",
        trControl = rf_fitControl,
        importance = 'impurity',
        tuneGrid = rf_tuneGrid,
        preProcess = c('scale', 'center')
      )
    
  
  mod_fit$finalModel
  summary(mod_fit)
  
  saveRDS(mod_fit, paste0(paste("mod_fit", colnames(npks)[i], sep = "_"),".rds"))
  
# Plot var importance
# ------------------------------------------------------------------------------
  var_imp <- varImp(mod_fit)
  # var_imp <- plot(var_imp)
  ggplot(var_imp)
  ggsave(filename = 
    paste0("./output2/", paste("variable_importance", colnames(npks)[i], sep = "_"), ".png")
  )
  
# ------------------------------------------------------------------------------  
# Prediction for validation
  pred_test <- predict(mod_fit, testing)
  val <- data.frame(testing$resp_var, pred_test)
  write.csv(val, file = paste0("./output2/", paste("validation", colnames(npks)[i], sep = "_"), ".csv"), row.names = F, col.names = T)
  
#checking rmse
  error <- rmse(testing, pred_test)

# predict using spatial grid data frame
  pred_grid <- predict(mod_fit, sgdf_final)
  
# converting to spatial grid data frame Exporting the prediction as raster
  pred_grid_sp <-
    data.frame(x = sgdf_final$s1, y = sgdf_final$s2, pred_grid)
  gridded(pred_grid_sp) <- ~x+y
  proj4string(pred_grid_sp) <- new_crs
  
  writeGDAL(
    pred_grid_sp,
    fname = paste0("./output2/", paste(colnames(npks)[i], "Predicted", sep = "_"),".tif"),
    drivername = "Gtiff",
    type = "Float32",
    options = "COMPRESS=DEFLATE"
  )
  }
  