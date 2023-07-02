library(raster)
library(sp)
library(rgdal)
stack_wd <- "C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\08RothC\\Ethiopia_for_ld\\data\\input\\stack"
raster_wd <- "C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\08RothC\\Ethiopia_for_ld\\data\\input\\raster"
aoi_wd <- "C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\08RothC\\Ethiopia_for_ld\\data\\input\\aoi"
setwd(stack_wd)
stacks <- list.files(path = ".", pattern = ".tif$", all.files = T)
stacks2 <- lapply(stacks, stack)
setwd(raster_wd)
rasters <- list.files(path = ".", pattern = ".tif$", all.files = T)
rasters2 <- lapply(rasters, raster)
ocs <- rasters2[[4]]
# Synchronize the rasters for stacking
for(i in 1:length(rasters2)){
  rasters2[[i]] <- spatial_sync_raster(rasters2[[i]],rasters2[[4]], method = "bilinear")
}
names(rasters2) <- c("clay", "land_cover", "NPP_Mean_81_00", "ocs")
r2_stack <- stack(rasters2)
setwd(aoi_wd)
aoi <- readOGR("eth.shp")

names <- c("ndvi", "PET_Stack_00_19", "PET_Stack_81_00", "Prec_Stack_00_19","Prec_Stack_81.00", "temp_av_Stack_81.00")
for(i in 1:nlayers(r2_stack)){
writeRaster(r2_stack[[i]], filename = names(r2_stack[[i]]), byLayer = T, format = "GTiff")
}

for(i in 1:length(stacks2)){
  stacks2[[i]] <- spatial_sync_raster(stacks2[[i]],ocs, method = 'bilinear')
}

for(i in 1:length(stacks2)){
  writeRaster(stacks2[[i]], filename = paste0(names[i], ".tif"), format = "GTiff")
}
rstack <- stack(rasters)

rasters2[[2]] <- spatial_sync_raster(rasters2[[2]],rasters2[[4]], method = 'ngb')
writeRaster(rasters2[[2]], filename = "land_cover.tif", format = "GTiff", overwrite = T)

setwd("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\08RothC\\Ethiopia_for_ld\\data\\input")
temp_stack <- stack("temp_av_Stack_81-00_TC.tif")

for(i in 1:nlayers(temp_stack)){
  temp_stack[[i]] <- temp_stack[[i]] crop(aoi) %>% mask(aoi)
  temp_stack[[i]] <- spatial_sync_raster(temp_stack[[i]], ocs, method = 'bilinear')
}
