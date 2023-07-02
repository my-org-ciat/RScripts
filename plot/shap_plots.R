library(kernelshap)
library(shapviz)
library(ggplot2)
library(dplyr)
library(doFuture)
library(caret)

rm(list = ls())
registerDoFuture()
plan(multisession, workers = 50)  # Windows
# plan(multicore, workers = 50)   # Linux, macOS, Solaris


setwd("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\rfe_fertilizer\\model_with_38_vars\\")
model <- readRDS("model_38vars.rds")
#sv_importance(model, kind = "bee", show_numbers = TRUE)

data <- read.csv("rgemat_after_rfe.csv", header = T, sep = ",")
training <- read.csv("training_data.csv", header = T, sep = ",")
testing <- read.csv("testing_data.csv", header = T, sep = ",")


# X <- training %>% select(-"gr_yield")
# X <- X[sample(nrow(X), 1000), ] # the explianing variables
# bg_X <- training[sample(nrow(training), 250), ]

X <- training %>% select(-"gr_yield")
bg_X <- training


X <- data.matrix(X[sample(nrow(X), 100), ])

# Step 2: Crunch SHAP values
shap <- shap.prep(model, X_train = X)
#shap_lm <- kernelshap(model, X, bg_X = bg_X)

shap_values_new <- kernelshap(
  mod_fit, X, bg_X = bg_X, parallel = T, parallel_args = list(.packages = "splines")
)
saveRDS(shap_values_new, "shap_result_new.rds")
sv_rf <- shapviz(shap_rf)
sv_importance(sv_rf)
sv_importance(sv_rf, show_numbers = TRUE) +
  ggtitle("SHAP Feature Importance")

sv_importance(sv_rf, kind = "bee", show_numbers = TRUE)
sv_importance(sv_rf, kind = "bar")
sv_importance(sv_rf, kind = "both", alpha = 0.2, width = 0.2)
sv_dependence(sv_rf, c("cdd1","n", "p", "k")) 

setwd("./new_model_colrename/shap")
shap_rf2 <- readRDS("shap_result_new.rds")
sv_rf2 <- shapviz(shap_rf2)
sv_importance(sv_rf2, show_numbers = TRUE)
sv_importance(sv_rf2, kind = "bee", show_numbers = F, fill = "#fca50a")
sv_dependence(sv_rf2, c("n","cdd1","cdd3", "p")) 

#all data
shap_final <- readRDS("shap_result_new.rds")
sv_final <- shapviz(shap_final)
vimp <- sv_importance(sv_final, show_numbers = F, max_display = 38, viridis_args = list())
vimp+theme(axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme_bw()

vimp + theme_bw()
imp <- sv_importance(sv_final, kind = "beeswarm", show_numbers = F, max_display = 38,
                     shapviz.viridis_args)

#imp+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
imp + theme_bw()

sv_dependence(sv_final, c("n"), interactions = F) 
sv <- shapviz(shap_final)
sv_waterfall(sv, 1)
#Loop over dependence plots in decreasing importance
for (i in 1:length(names(shap_final$X))) {
  p <- sv_dependence(shap_final, names(shap_final$X)[i])
  jpeg(file = paste0(names(shap_final$X)[i], ".jpeg"))
  dev.off()
}

for (v in shap.importance(shap_final, names_only = TRUE)) {
  p <- shap.plot.dependence(shap_final, v, color_feature = "auto",
                            alpha = 0.5, jitter_width = 0.1) +
    ggtitle(v)
  print(p)
}
colnames(shap_final$X) <- colnames(shap_final$SE) <- colnames(shap_final$S)
 c( "n_rate", "k_rate","ctrl_yld" ,"tpi", "p_rate" ,"EC","Ca", 
    "prcp2","Mg", "soil_K", "srad2", "S","dtr2",     
 "Co", "soil_P", "Mo","CaCO3", "Zn","soil_N", "Mn",    
 "cdd1", "MnAl","dem","prcp3", "OM", "pH","spei3",    
 "Cu", "srad1", "dtr3","spei2", "tri", "srad3","cdd3",     
 "CEC","Si","slope","B", "gr_yield")
 
