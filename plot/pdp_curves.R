library(pdp)
library(ggplot2)
setwd("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\final_prediction6\\models\\03model_wout_soilgrids")
load("model_wout_soilgrids.RData")
load("reg_mat_wout_soilgrids.RData")
load("train_data_wout_soilgrids.RData")

#single
n <- partial(mod_fit, pred.var = c("n"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(n, xlab = "n", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

p <- partial(mod_fit, pred.var = c("p"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(p, xlab = "p", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

k <- partial(mod_fit, pred.var = c("k"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(k, xlab = "k", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

cdd1 <- partial(mod_fit, pred.var = c("cdd1"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(cdd1, xlab = "consecutive dry days", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

cdd2 <- partial(mod_fit, pred.var = c("cdd2"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(cdd2, xlab = "consecutive dry days", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

cdd3 <- partial(mod_fit, pred.var = c("cdd3"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(cdd3, xlab = "consecutive dry days", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

tpi <- partial(mod_fit, pred.var = c("tpi"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(tpi, xlab = "topographic position index", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

ctrl_yld <- partial(mod_fit, pred.var = c("ctrl_yld"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(ctrl_yld, xlab = "control yield", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

ec <- partial(mod_fit, pred.var = c("EC_rf"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(ec, xlab = "exchangable cation", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

ec <- partial(mod_fit, pred.var = c("EC_rf"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(ec, xlab = "exchangable cation", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

rf <- partial(mod_fit, pred.var = c("prcp1"), levelplot = TRUE, palette = c("viridis"), contour = T, plot.engine = "ggplot2")
plotPartial(rf, xlab = "precipitation", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

rf2 <- partial(mod_fit, pred.var = c("prcp2"), levelplot = TRUE, palette = c("viridis"), contour = T, plot.engine = "ggplot2")
plotPartial(rf2, xlab = "precipitation", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

rf3 <- partial(mod_fit, pred.var = c("prcp3"), levelplot = TRUE, palette = c("viridis"), contour = T, plot.engine = "ggplot2")
plotPartial(rf3, xlab = "precipitation", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

tmax1 <- partial(mod_fit, pred.var = c("tmax1"), levelplot = TRUE, palette = c("viridis"), contour = T, plot.engine = "ggplot2")
plotPartial(tmax1, xlab = "max temp", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

tmax2 <- partial(mod_fit, pred.var = c("tmax2"), levelplot = TRUE, palette = c("viridis"), contour = T, plot.engine = "ggplot2")
plotPartial(tmax2, xlab = "max temp", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

tmax3 <- partial(mod_fit, pred.var = c("tmax3"), levelplot = TRUE, palette = c("viridis"), contour = T, plot.engine = "ggplot2")
plotPartial(tmax3, xlab = "max temp", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

tmin1 <- partial(mod_fit, pred.var = c("tmin1"), levelplot = TRUE, palette = c("viridis"), contour = T, plot.engine = "ggplot2")
plotPartial(tmin1, xlab = "min temp", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

tmin2 <- partial(mod_fit, pred.var = c("tmin2"), levelplot = TRUE, palette = c("viridis"), contour = T, plot.engine = "ggplot2")
plotPartial(tmin2, xlab = "min temp", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

tmin3 <- partial(mod_fit, pred.var = c("tmin3"), levelplot = TRUE, palette = c("viridis"), contour = T, plot.engine = "ggplot2")
plotPartial(tmin3, xlab = "min temp", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

srad1 <- partial(mod_fit, pred.var = c("srad1"), levelplot = TRUE, palette = c("viridis"), contour = T, plot.engine = "ggplot2")
plotPartial(srad1, xlab = "solar radiation", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

srad2 <- partial(mod_fit, pred.var = c("srad2"), levelplot = TRUE, palette = c("viridis"), contour = T, plot.engine = "ggplot2")
plotPartial(srad2, xlab = "solar radiation", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

srad3 <- partial(mod_fit, pred.var = c("srad3"), levelplot = TRUE, palette = c("viridis"), contour = T, plot.engine = "ggplot2")
plotPartial(srad3, xlab = "solar radiation", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

cwd1 <- partial(mod_fit, pred.var = c("cwd1"), levelplot = TRUE, palette = c("viridis"), contour = T, plot.engine = "ggplot2")
plotPartial(cwd1, xlab = "consecutive wet days", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

cwd2 <- partial(mod_fit, pred.var = c("cwd2"), levelplot = TRUE, palette = c("viridis"), contour = T, plot.engine = "ggplot2")
plotPartial(cwd2, xlab = "consecutive wet days", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

cwd3 <- partial(mod_fit, pred.var = c("cwd3"), levelplot = TRUE, palette = c("viridis"), contour = T, plot.engine = "ggplot2")
plotPartial(cwd3, xlab = "consecutive wet days", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

ins_n <- partial(mod_fit, pred.var = c("INS_N"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(ins_n, xlab = "INS N", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

ins_p <- partial(mod_fit, pred.var = c("INS_P"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(ins_p, xlab = "INS P", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

ins_k <- partial(mod_fit, pred.var = c("INS_K"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(ins_k, xlab = "INS K", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

slope <- partial(mod_fit, pred.var = c("slope"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(slope, xlab = "slope", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

tri <- partial(mod_fit, pred.var = c("tri"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(tri, xlab = "topographic rougness index", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

ins_n <- partial(mod_fit, pred.var = c("INS_N"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(ins_n, xlab = "INS N", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

s <- partial(mod_fit, pred.var = c("S_rf"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(s, xlab = "sulphur", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

caco3 <- partial(mod_fit, pred.var = c("CaCO3_rf"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(caco3, xlab = "caco3", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

dtr1 <- partial(mod_fit, pred.var = c("dtr1"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(dtr1, xlab = "diurnal temperature range", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

dtr2 <- partial(mod_fit, pred.var = c("dtr2"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(dtr2, xlab = "diurnal temperature range", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

dtr3 <- partial(mod_fit, pred.var = c("dtr3"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(dtr3, xlab = "diurnal temperature range", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

spei1 <- partial(mod_fit, pred.var = c("spei1"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(spei1, xlab = "standardised precipitation-evapotranspiration index", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

spei2 <- partial(mod_fit, pred.var = c("spei2"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(spei2, xlab = "standardised precipitation-evapotranspiration index", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

spei3 <- partial(mod_fit, pred.var = c("spei3"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(spei3, xlab = "standardised precipitation-evapotranspiration index", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

dem <- partial(mod_fit, pred.var = c("dem"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(dem, xlab = "standardised precipitation-evapotranspiration index", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

ph <- partial(mod_fit, pred.var = c("pH_rf"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(ph, xlab = "PH", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

mo <- partial(mod_fit, pred.var = c("Mo_rf"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(mo, xlab = "molbidium", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

co <- partial(mod_fit, pred.var = c("Co_rf"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(co, xlab = "cobalt", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

mag <- partial(mod_fit, pred.var = c("Mg_sat_rf"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(mag, xlab = "magnisium", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

om <- partial(mod_fit, pred.var = c("OM_pct_rf"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(om, xlab = "organic matter", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

cu <- partial(mod_fit, pred.var = c("Cu_rf"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(cu, xlab = "copper", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

mn <- partial(mod_fit, pred.var = c("Mn_rf"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(mn, xlab = "manganese", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

ca <- partial(mod_fit, pred.var = c("Ca_sat_rf"), levelplot = TRUE, palette = c("viridis"), contour = T)
pp = plotPartial(ca, xlab = "calcium", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

zn <- partial(mod_fit, pred.var = c("Zn_rf"), levelplot = TRUE, palette = c("viridis"), contour = T)
plotPartial(zn, xlab = "zinc", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

#two variables
np <- partial(mod_fit, pred.var = c("n","p"))
np_plot <- autoplot(np, contour.color = "red", plot.pdp = TRUE, pdp.linetype = 3)
plot(np_plot)

nk <- partial(mod_fit, pred.var = c("n","k"))
nk_plot <- autoplot(nk, contour.color = "red", plot.pdp = TRUE, pdp.linetype = 3)
plot(nk_plot)

pk <- partial(mod_fit, pred.var = c("p","k"))
pk_plot <- autoplot(pk, contour.color = "red", plot.pdp = TRUE, pdp.linetype = 3)
plot(pk_plot)

#three variables
npk <- partial(mod_fit, pred.var = c("n","p","k"))
np_plot <- autoplot(np, contour.color = "red", plot.pdp = TRUE, pdp.linetype = 3)
plot(np_plot)


plotPartial(np, pdp.lwd = 3, pdp.lty = 2, palette = c("viridis"), alpha = 1)

library(ggplot2)  # required to use autoplot
library(randomForest)
data (boston)  # load the boston housing data
set.seed(101)  # for reproducibility
boston.rf <- randomForest(cmedv ~ ., data = boston)
boston.rf %>%
  partial(pred.var = "lstat") %>%
  plotPartial(rug = TRUE, train = boston)
boston.rf %>%
  partial(pred.var = c("lstat", "rm"), chull = TRUE, progress = "text") %>%
  plotPartial(contour = TRUE, legend.title = "rm")

for(i in 52:ncol(mod_fit$trainingData)){
  print(names(mod_fit$trainingData[i]))
  pl <- partial(mod_fit, pred.var = c("k", names(mod_fit$trainingData[i])), plot.engine = c("ggplot2"), trim.outliers = T, legend.title = 'yield (kg/ha)')
  pplot <- autoplot(pl, ylab = names(mod_fit$trainingData[i]), xlab = "k")
  ggsave(filename = paste0(names(mod_fit$trainingData[i]),".png"), plot = pplot)
}

var_imp <- varImp(mod_fit)
ggplot(var_imp)

#single

for(i in 2:ncol(mod_fit$trainingData)){
  print(names(mod_fit$trainingData[i]))
  pl <- partial(mod_fit, pred.var = c(names(mod_fit$trainingData[i])), trim.outliers = 'T')
  pplot <- autoplot(pl, xlab = names(mod_fit$trainingData[i]), ylab = "yield (kg/ha)")
  #pplot <- plotPartial(pl, names(mod_fit$trainingData[i]), ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)
  ggsave(filename = paste0(names(mod_fit$trainingData[i]),".png"), plot = pplot)
}

tmin3 <- partial(mod_fit, pred.var = c("tmin3"), levelplot = TRUE, palette = c("viridis"), contour = T, plot.engine = "ggplot2")
plotPartial(tmin3, xlab = "min temp", ylab = "yield (kg/ha)", pdp.lwd = 3, pdp.lty = 2, palette = c("plasma"), alpha = 1)

#plot pridicted vs actual
load("train_data_wout_soilgrids.RData")
data <- data.frame(actual = training$gr_yield, predicted = predict(mod_fit))
head(data)

ggplot(data, aes(y= predicted, x= actual)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red",
              size = 1.5) +
  labs(y ='Predicted Values', x ='Actual Values')

i_scores <- varImp(mod_fit)
i_scores <- varImp(model, conditional=TRUE)

