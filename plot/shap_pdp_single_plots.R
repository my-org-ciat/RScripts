
packages <- c("elasticnet", "partykit", "rpart", "rpart.plot", "randomForest", "caret", "caretEnsemble", "iml", "tidyverse", "dplyr")
invisible(lapply(packages, function(x) suppressMessages(require(x, character.only=T, quietly=T, warn.conflicts=F))))

setwd("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\rfe_fertilizer\\model_with_38_vars\\new_model_colrename\\data\\workspace")

model <- readRDS("model_colrename.rds")

training <- read.csv("training_colrename.csv", header = T, sep = ",")
regmat <- read.csv("regmat_colrename.csv", header = T, sep = ",")
testing <- read.csv("testing_colrename.csv", header = T, sep = ",")

model_imp <- Predictor$new(model, data=training[,-39], y=training$cc_yield)
setwd("../../shap/")
saveRDS(model_imp, "model_imp.rds")

#pdp single
pdp_n <-
  FeatureEffect$new(model_imp, feature=c("n_rate"), method="pdp") %>% plot()
pdp_p <-
  FeatureEffect$new(model_imp, feature=c("p_rate"), method="pdp") %>% plot()
plot(pdp_p) + theme_bw()

pdp_k <-
  FeatureEffect$new(model_imp, feature = c("k_rate"), method="pdp") %>% plot()

plot(pdp_k) + theme_bw()

pdp_cdd1 <-
  FeatureEffect$new(model_imp, feature = c("cdd1"), method="pdp") %>% plot()

plot(pdp_cdd1) + theme_bw()

pdp_cdd3 <-
  FeatureEffect$new(model_imp, feature = c("cdd3"), method="pdp") %>% plot()
plot(pdp_cdd3) + theme_bw()

pdp_srad3 <-
  FeatureEffect$new(model_imp, feature = c("srad3"), method="pdp") %>% plot()
plot(pdp_srad3) + theme_bw()

pdp_ctrl_yld <-
  FeatureEffect$new(model_imp, feature = c("ctrl_yld"), method="pdp") %>% plot()
plot(pdp_ctrl_yld) + theme_bw()

pdp_ec <-
  FeatureEffect$new(model_imp, feature = c("EC"), method="pdp") %>% plot()
plot(pdp_ec) + theme_bw()

pdp_soilp <-
  FeatureEffect$new(model_imp, feature = c("soil_P"), method="pdp") %>% plot()
plot(pdp_soilp) + theme_bw()

pdp_s <-
  FeatureEffect$new(model_imp, feature = c("S"), method="pdp") %>% plot()
plot(pdp_s) + theme_bw()

pdp_dem <-
  FeatureEffect$new(model_imp, feature = c("dem"), method="pdp") %>% plot()
plot(pdp_dem) + theme_bw()

pdp_tpi <-
  FeatureEffect$new(model_imp, feature = c("tpi"), method="pdp") %>% plot()
plot(pdp_tpi) + theme_bw()

pdp_tri <-
  FeatureEffect$new(model_imp, feature = c("tri"), method="pdp") %>% plot()
plot(pdp_tri) + theme_bw()

#pdp two interactions
int_var <- c("p_rate", "k_rate", "cdd1", "cdd3", "srad3", "ctrl_yld",
             "EC", "soil_P", "S", "dem", "tpi", "tri")
for(i in 1:length(int_var)){
  print(i)
  pdp <-
    FeatureEffect$new(model_imp, c("n_rate", int_var[i]), method = "pdp")
    saveRDS(pdp, file = paste0("n_", int_var[i], ".rds"))
}
