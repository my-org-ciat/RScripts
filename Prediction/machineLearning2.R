# ------------------------------------------------------------------------------
# Reads rda output to predict fertilizer nutrient
predictFertilizer <- function(rda_file_path){
  require(rgdal)
  require(raster)
  require(caret)
  require(ranger)
  require(dplyr)
  require(e1071)
  require(randomForest)
  require(sp)
  require(Metrics)
  require(ggplot2)
  
  dir.create("predict_output", showWarnings = F)
  
# ------------------------------------------------------------------------------
# loading the rda outputs and adding to the global env
  rda_files <- list.files(path = rda_file_path, pattern = ".rda")
  lapply(rda_files,load,.GlobalEnv)
  
  new_crs <- "+proj=longlat +datum=WGS84 +no_defs"
  
# ------------------------------------------------------------------------------
# Create training and testing data set
  mtry <- as.integer((ncol(cov_npks))/3)
  mtry <- seq(mtry-20, mtry+20, by = 3)

# ------------------------------------------------------------------------------ 
# model calibration and validation
  set.seed(7)
  
  
  rf_fitControl <- trainControl(method = "repeatedcv",
                             number = 5,
                             repeats = 3)
  
  rf_tuneGrid <- expand.grid(.mtry = mtry,
                             .splitrule =  "maxstat",
                             .min.node.size = c(20))
# Normalization
  # yield <- stack() 
  vals <- c(seq(0, 200, by=10))
  d <- data.frame(matrix(ncol = 21, nrow = nrow(cov_npks)))
  d_pred <- data.frame(matrix(ncol = 21, nrow = nrow(cov_npks)))
  for(i in 1:length(vals)){
    names(d)[i] <- paste0("N_Rate",vals[i])
    d[,i] <- vals[i]
  }
  
  for(i in 1:ncol(d)){
    set.seed(7)
    data_pred <- cbind(d[,i], cov_npks)
    inTrain <- createDataPartition(y =  data_pred$yield, p = 0.75, list = FALSE)
    training <- data_pred[inTrain,]
    testing <- data_pred[-inTrain,]
    
      mod_fit <- train(
        yield ~ .,
        data = training,
        method = "ranger",
        trControl = rf_fitControl,
        importance = 'impurity',
        tuneGrid = rf_tuneGrid,
        preProcess = c('scale', 'center')
      )
    
  
  mod_fit$finalModel
  summary(mod_fit)
  
  saveRDS(mod_fit, paste0("./predict_output/", paste("mod_fit", colnames(npks)[i], sep = "_"),".rds"))
  
# Plot and save variable importance
# ------------------------------------------------------------------------------
  var_imp <- varImp(mod_fit)
  # var_imp <- plot(var_imp)
  ggplot(var_imp)
  
  ggsave(filename = 
    paste0("./predict_output/", paste("variable_importance", colnames(npks)[i], sep = "_"), ".png")
  )
  
# ------------------------------------------------------------------------------  
# Prediction for validation
  pred_test <- predict(mod_fit, testing)
  names(d_pred)[i] <- paste0("N_Rate",vals[i])
  d_pred [,i] <- pred_test
   
  val <- data.frame(testing$resp_var, pred_test)
  # write.csv(val, file = paste0("./predict_output/", paste("validation", colnames(npks)[i], sep = "_"), ".csv"), row.names = F, col.names = T)
  
#checking rmse
  # error <- rmse(testing, pred_test)

  # generATE OPTIMAL NUITRIENT
  # SOME FUNCTION OR PACKAGE
  
# predict using spatial grid data frame
  pred_grid <- predict(mod_fit, sgdf_final)
  
# converting to spatial grid data frame Exporting the prediction as raster
  pred_grid_sp <-
    data.frame(x = sgdf_final$s1, y = sgdf_final$s2, pred_grid)
  gridded(pred_grid_sp) <- ~x+y
  proj4string(pred_grid_sp) <- new_crs
  
  yield <- addLayer(yield, pred_grid_sp)
  Optimal_yiled <- max(yield)
  
  writeGDAL(
    Optimal_yiled,
    fname = paste0("./predict_output/", paste(colnames(npks)[i], "Predicted", sep = "_"),".tif"),
    drivername = "Gtiff",
    type = "Float32",
    options = "COMPRESS=DEFLATE"
  )
  }
}

#Example
predictFertilizer("./output/")
  