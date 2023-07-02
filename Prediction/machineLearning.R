# ------------------------------------------------------------------------------
# Reads csv & raster data to predict fertilizer
# predictFertilizer <- function(rast_path, csv_path, csv, column){

# clear working environment
 rm(list = ls())
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