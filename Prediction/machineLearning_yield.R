# ------------------------------------------------------------------------------
# Reads rda output to predict fertilizer nutrient
# predictFertilizer <- function(rda_file_path){
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
  rda_files <- list.files(path = ".", pattern = ".rda")
  lapply(rda_files,load,.GlobalEnv)
  
  new_crs <- "+proj=longlat +datum=WGS84 +no_defs"
  
# ------------------------------------------------------------------------------
# Create training and testing data set
  mtry <- as.integer((ncol(covs_yield))/3)
  mtry <- seq(mtry-2, mtry+2, by = 3)

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
  dir.create("tmp", showWarnings = F)
  vals <- c(seq(0, 200, by=10))
  d <- data.frame(matrix(ncol = length(vals), nrow = nrow(covs_yield)))
  d_test <- data.frame(matrix(ncol = length(vals), nrow = nrow(covs_yield)))
  for(i in 1:length(vals)){
    names(d)[i] <- paste0("N_Rate",vals[i])
    d[,i] <- vals[i]
  }
  
  for(i in 1:ncol(d)){
    set.seed(7)
    N_rate <- d[,i]
    # data_pred <- paste0(d, 5)
    data_pred <- cbind(covs_yield, N_rate)
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
  
  # saveRDS(mod_fit, paste0("./predict_output/", paste("mod_fit", colnames(npks)[i], sep = "_"),".rds"))
  
# Plot and save variable importance
# ------------------------------------------------------------------------------
  var_imp <- varImp(mod_fit)
  # var_imp <- plot(var_imp)
  ggplot(var_imp)
  
  # ggsave(filename =
  #   paste0("./predict_output/", paste("variable_importance", colnames(npks)[i], sep = "_"), ".png")
  # )

# ------------------------------------------------------------------------------  
# Prediction for validation
  pred_test <- predict(mod_fit, testing)
  names(d_pred)[i] <- paste0("N_Rate",vals[i])
  d_test <- data.frame(matrix(ncol = length(vals), nrow = nrow(testing)))
  colnames(d_test)[i] <- paste0("N_rate",i)
  d_test [,i] <- pred_test
   
  val <- data.frame(testing$yield, pred_test)
  # write.csv(val, file = paste0("./predict_output/", paste("validation", colnames(npks)[i], sep = "_"), ".csv"), row.names = F, col.names = T)
  
#checking rmse
  # error <- rmse(testing, pred_test)

  # generATE OPTIMAL NUITRIENT
  # SOME FUNCTION OR PACKAGE
  
# predict using spatial grid data frame
  data <- data.frame(matrix(c(d[1,i]), ncol = 1, nrow = nrow(sgdf_final)))
  # data <- paste0(data, 1)
  colnames(data) <- "N_rate"
  data <- cbind(sgdf_final,data)
  pred_grid <- predict(mod_fit, data)
  
# converting to spatial grid data frame Exporting the prediction as raster
  pred_grid_sp <-
    data.frame(x = data$s1, y = data$s2, pred_grid)
  gridded(pred_grid_sp) <- ~x+y
  proj4string(pred_grid_sp) <- new_crs
 
  writeGDAL(
    pred_grid_sp,
    fname = paste0("./tmp/", paste0("N_predicted", i),".tif"),
    drivername = "Gtiff",
    type = "Float32",
    options = "COMPRESS=DEFLATE"
  )
}#end of for loop
# out of the loop
  setwd("tmp")
  yield <- list.files(path = ".", pattern = ".tif$", all.files = T)
  yield_ras <- lapply(yield, raster)
  yield_stack <- stack(yield_ras)
  optimal_yield <- max(yield_stack)
  writeRaster(optimal_yield, filename = "optimal_yield", format = "GTiff", overwrite = TRUE)
  # }

#Example
predictFertilizer("./output/")
  