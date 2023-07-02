library(dplyr)
library(rgdal)
library(sp)
library(terra)

#read the vector files
rm(list = ls())
setwd("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\prediction_2023\\db_shapefiles")
kebele <- vect("kebele.shp")
plot(kebele)
class(kebele)

setwd("../geoserver_upload/01scenario/")
dirs <- list.dirs(recursive = F)
for(i in 1:length(dirs)){
  if(i == 1){
    setwd(dirs[i])
  }else{
    dir_name <- strsplit(dirs[i], "./")
    setwd(paste0("../", dir_name[[1]][2]))
  }
  r <- list.files(pattern = ".tif$", all.files = T) %>% lapply(rast)
  stack <- c(r) %>% rast()
  names(stack) <- c("wheat_compost", "wheat_n", "wheat_nps", "wheat_optimal_yield",
                   "wheat_p", "wheat_urea", "wheat_vcompost")
  keb_val <- extract(stack, kebele, fun = mean, na.rm = T)
  for(j in 2:ncol(keb_val)){
    final_val <- cbind(kebele, as.data.frame(keb_val[,j]))
    names(final_val)[ncol(final_val)] <- "value"
    writeVector(final_val, filename = paste0(names(stack[[j-1]]), ".shp"), filetype = "ESRI Shapefile", overwrite = T)
  }
}
