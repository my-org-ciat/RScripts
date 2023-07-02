library(terra)
library(dplyr)

rm(list = ls())

setwd("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\prediction_2023\\geoserver_upload")
dirs <- list.dirs(recursive = F)
for(i in 1:length(dirs)){
  if(i == 1){
    setwd(dirs[i])
  }else{
    name <- strsplit(dirs[i], "./")
    setwd(paste0("../", name[[1]][2]))
  }
  r <- list.files(pattern = ".tif$") %>% lapply(rast)
  stack <- c(r) %>% rast()
  comp <- (stack[[1]] * 100 / 0.8) / 1000
  v_comp <- (stack[[1]] * 100 / 1) / 1000
  nps <- stack[[2]] * 100 / (16.59)
  urea <- (stack[[1]] - (nps * 19 / 100)) * 100 / 46
  name <- strsplit(dirs[i], "./")
  
  writeRaster(comp, file = paste0("wheat_compost", "_",name[[1]][2], ".tif"), filetype = "GTiff", overwrite = T)
  writeRaster(v_comp, file = paste0("wheat_v_compost", "_",name[[1]][2], ".tif"), filetype = "GTiff", overwrite = T)
  writeRaster(nps, file = paste0("wheat_nps", "_",name[[1]][2], ".tif"), filetype = "GTiff", overwrite = T)
  writeRaster(urea, file = paste0("wheat_urea", "_",name[[1]][2], ".tif"), filetype = "GTiff", overwrite = T)
  stack <- c()
  }


#done one by one 
rm(list = ls())
setwd("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\prediction_2023\\geoserver_upload/dominant/")
list.files()
n_dominant <- rast("wheat_n_dominant.tif")
p_dominant <- rast("wheat_p_dominant.tif")

comp_dominant <- (n_dominant * 100 / 0.8) / 1000
v_comp_dominant <- (n_dominant * 100 / 1) / 1000
nps_dominant<- p_dominant * 100 / (16.59)
urea_dominant <- (n_dominant - (nps_dominant* 19 / 100)) * 100 / 46

plot(comp_dominant)
plot(v_comp_dominant)
plot(nps_dominant)
plot(urea_dominant)

writeRaster(comp_dominant, file = "wheat_compost_dominant.tif", filetype = "GTiff", overwrite = T)
writeRaster(v_comp_dominant, file = "wheat_v_compost_dominant.tif", filetype = "GTiff", overwrite = T)
writeRaster(nps_dominant, file = "wheat_nps_dominant.tif", filetype = "GTiff", overwrite = T)
writeRaster(urea_dominant, file = "wheat_urea_dominant.tif", filetype = "GTiff", overwrite = T)
