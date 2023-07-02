setwd("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\final_prediction6\\models\\08model_scenario\\quantile_model")
list.files()
above <- readRDS("model_above.rds")
normal <- readRDS("model_normal.rds")
below <- readRDS("model_below.rds")

n_normal <- partial(normal, pred.var = "n")
n_normal[, 3] <- "normal"

n_above <- partial(above, pred.var = "n")
n_above[, 3] <- "above"

n_below <- partial(below, pred.var = "n")
n_below[, 3] <- "below"

n <- rbind(n_normal, n_above,n_below)
colnames(n)[3] <- "yhat.id"
colnames(n)[2] <- "yield"

ggplot(n, aes(n, yield, color = yhat.id)) +
  geom_line() +
  theme(legend.title = element_blank())

