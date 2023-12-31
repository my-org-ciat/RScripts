# ------------------------------------------------------------------------------
# converting list of SoilGrid geo-tiff files using conversion factors
# ------------------------------------------------------------------------------
convertSoilGrids <- function(sou_path, dest_path = NULL){
  # library(terra)
  require(raster)
  # library(dplyr)
  require(rgdal)
  setwd(sou_path)
  if(is.null(dest_path)){
    dest_path = getwd()
  }
  else{
    dest_path = dest_path
  }
    rasterlist <- list.files(path = ".", pattern = ".tif$")
    rasters <- lapply(rasterlist, raster)
    rasterlist_names <-
      c(unlist(lapply(
        strsplit(rasterlist, "[.]"),
        FUN = function(x) {
          x[1]
        }
      ))) 
    for(i in 1:length(rasterlist_names) ) { 
      assign(rasterlist_names[i], raster(rasterlist[i])) 
      if(rasterlist_names[i] == ("nitrogen") ||
         rasterlist_names[i] == ("bdod")) {
        out <- rasters[[i]] * 0.01
      }else{
        out <- rasters[[i]] * 0.1
      }
      rout_name <-rasterlist_names[i]
      writeRaster(
        out,
        filename = paste(dest_path, rout_name, sep = "/"),
        format = "GTiff",
        overwrite = T
      )
     
  }
  
}

#Example - locating the path for downloaded soil covariates
# sou_path -> source path
# dest_path -> destination path

convertSoilGrids("D:/Test")
convertSoilGrids(sou_path = "D:/Test", dest_path = "D:/output")