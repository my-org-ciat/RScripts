library(ggplot2)
setwd("./interaction_plot_rds/")
rds <- list.files(pattern = ".rds$", all.files = T) %>% lapply(readRDS)

#dependence plots 
for(i in 1:length(rds)) {
  rds1 <- rds[[i]]$results
  names(rds1)[3] <- "yield"
  plot <- ggplot(data = rds1, aes(y = rds1[, 2], x = rds1[, 1])) +
    geom_raster(aes(fill = yield)) +
    labs(y = colnames(rds1)[2], x = colnames(rds1)[1]) +
    scale_fill_viridis_c()
  plot + theme_bw()
  ggsave(
    filename = paste0("nrate_", i, ".png"),
    plot = last_plot(),
    units = "px",
    dpi = 500
  )
  try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
}
