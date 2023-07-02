#fill na values of a raster using the modal (dominant) values
#for geoserver upload

library(raster)
library(rgdal)
library(maptools)
library(dplyr)
library(terra)

rm(list = ls())
setwd("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\prediction_2023/data/final\\dominant\\final")

n <- rast("wheat_n_dominant.tif")
p <- rast("wheat_p_dominant.tif")

n2 <- focal(n, 31, "modal", na.policy="only", na.rm=TRUE)
p2 <- focal(p, 31, "modal", na.policy="only", na.rm=TRUE)

plot(n2)
writeRaster(n2,"wheat_n_dominant.tif", filetype = "GTiff")
writeRaster(p2,"wheat_p_dominant.tif", filetype = "GTiff")

n4 <- focal(n2, 31, "modal", na.policy="only", na.rm=TRUE)
