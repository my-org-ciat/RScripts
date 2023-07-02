
library(ggplot2)
library(pdp)

setwd("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\final_prediction6\\models\\08model_scenario\\normal/")
list.files()
normal <- readRDS("model_normal.rds")
top5 <- topPredictors(normal, n = 5)

p_normal <- partial(normal, pred.var = "p")
p_normal[, 3] <- "normal"

setwd("../above")
list.files()
above <- readRDS("model_above.rds")

p_above <- partial(above, pred.var = "p")
p_above[, 3] <- "above"

setwd("../below")
list.files()
below <- readRDS("model_below.rds")
p_below <- partial(below, pred.var = "p")
p_below[, 3] <- "below"

p <- rbind(p_normal, p_above, p_below)
colnames(p)[3] <- "yhat.id"
colnames(p)[2] <- "yield"

ggplot(p, aes(p, yield, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())
