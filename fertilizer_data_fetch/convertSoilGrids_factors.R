# ------------------------------------------------------------------------------
# converting geo-tiff files using conversion factors
# ------------------------------------------------------------------------------
# library(terra)
library(raster)
library(dplyr)
rlist <- list.files(path = ".", pattern = ".tif$")
nitrogen <- raster(rlist[[1]])
nitrogen <- (nitrogen*100)
writeRaster(nitrogen, file = paste("D:/cov/","nitrogen.tif"), filetype = "GTiff", overwrite = T)
