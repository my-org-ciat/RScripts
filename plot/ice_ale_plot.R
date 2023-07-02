setwd("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\final_prediction6\\models\\03model_wout_soilgrids")
load("model_wout_soilgrids.RData")
load("reg_mat_wout_soilgrids.RData")
load("train_data_wout_soilgrids.RData")
pred_fun <- function(X.model, newdata) {
  predict(X.model, newdata)
}
X <- dplyr::select(train_data, -c("gr_yield"))
ALEPlot_1 <- ALEPlot(X = X, X.model = mod_fit, J = 48, pred.fun = pred_fun, NA.plot = T)
plot(ALEPlot_1$x.values, ALEPlot_1$f.values, type="l", xlab="n", ylab="ALE for n")


#ice plot
iceplot1 = ice(object = mod_fit,
              X = train_data,
              y = train_data$gr_yield,
              predictor = "n")

iceplot2 = ice(object = mod_fit,
               X = train_data,
               y = train_data$gr_yield,
               predictor = "p")

#clustering iceplots with mean
clusterICE(iceplot, nClusters = 5, plot_pdp = T,avg_lwd = 1, centered = T, colorvec = c("blue", "red","black", "green", "brown"))

grid.arrange(iceplot, iceplot2)
plot(iceplot,
     frac_to_plot = 1,
     centered = F,
     plot_orig_pts_preds = F, x_quantile = FALSE , ylab = 'yield (kg/ha)')
