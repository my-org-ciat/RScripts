clip_raster <- function(s_path, d_path){
  library(raster)
  setwd(s_path)
  rfiles <- list.files(path = ".", pattern = ".tif$", full.names = T)
  rasters <- lapply(rfiles, raster)
  stack <- stack(rasters)
  setwd("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\Data\\01Filtered_Final_Data\\new_data_extent_final")
  aoi <- readOGR("extent_wgs__water_final.shp")
  setwd(d_path)
  for(i in 1:nlayers(stack)){
    r <- stack[[i]] %>% raster::crop(aoi) %>% raster::mask(aoi)
    writeRaster(r, filename = basename(rfiles[[i]]), format = "GTiff")
  }
  print("completed")
}

clip_raster("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\final_prediction5\\04geoserverUploads\\vcompost2", "C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\final_prediction5\\04geoserverUploads\\clipped_by_waterextent\\vcompost2")
