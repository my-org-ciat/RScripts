# ------------------------------------------------------------------------------
# stacks rasters
# ------------------------------------------------------------------------------
stackRasters <- function(sou_path, dest_path = NULL){
  # library(terra)
  library(raster)
  # library(dplyr)
  library(rgdal)
  setwd(sou_path) 
  rasterlist <- list.files(path = ".", pattern = ".tif$")
  rasters <- lapply(rasterlist, raster)
  rstack <- stack(rasters)
  writeRaster(
    rstack,
    filename = paste(dest_path,"raster_stack", sep = "/"),
    format = "GTiff",
    overwrite = T
  )
}
