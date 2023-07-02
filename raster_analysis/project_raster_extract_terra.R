library(terra)
library(dplyr)

rm(list = ls())
setwd("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\Dr.Feyera/Dr.Feyera_maize/output/")

train_data <- read.csv("all_data.csv", header = T, sep = ",")

setwd("C:\\Users\\ATilaye\\Documents\\01My_Docs\\GIS_Data\\EthioSIS_Data_fromATI")
r <- list.files(pattern = ".tif$", all.files = T) %>% lapply(rast)
stack <- c(r) %>% rast()

pts <- vect(train_data, geom=c("lon", "lat"), crs="epsg:4326")
stack_prj <- rast()
for(i in 1:nlyr(stack)){
  st <- project(x = stack[[i]], y = "epsg:4326", method = "bilinear")
  stack_prj <- c(stack_prj, st)
}

pts_value <- extract(stack_prj, pts, xy = T) %>% select(-c("ID", "x", "y"))
train_data_soil <- cbind(train_data[,-1], pts_value)
write.csv(train_data_soil, "all_data_with_soil.csv", col.names = T, row.names = F)

dir.create("EThiosis_wgs_gcs")
setwd("EThiosis_wgs_gcs")
for(i in 1:nlyr(stack_prj)){
  writeRaster(stack_prj[[i]], filename = paste0(names(stack_prj[[i]]), ".tif"), filetype = "GTiff")
}

