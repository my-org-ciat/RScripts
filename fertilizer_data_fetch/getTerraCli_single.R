#install the packages
#remotes::install_github("mikejohnson51/AOI")
#remotes::install_github("mikejohnson51/climateR")
library(AOI)
library(climateR)
library(sf)
library(raster)
# library(rasterVis)
library(dplyr)

#AOI Layer
shape <- st_read("D:/tigray/tigray.shp")

#Rainfall
precp = getTerraClim(shape, param = "prcp", startDate = "2019-02-01")#single month
precp2 = getTerraClim(shape, param = "prcp", startDate = "2019-02-01", endDate = "2019-03-01")#two months

#tmax
temp_max = getTerraClim(shape, param = "tmax", startDate = "2019-02-01", endDate = "2019-03-01")#single month

#tmin
temp_min = getTerraClim(shape, param = "tmin", startDate = "2019-02-01", endDate = "2019-03-01")#single month

#downward short wave radiation flux
srad = getTerraClim(shape, param = "srad", startDate = "2019-02-01", endDate = "2019-03-01")#single month

#select the raster stack from the list
srad = srad$terraclim_srad
temp_max = temp_max$terraclim_tmax
temp_min = temp_min$terraclim_tmin
precp2 = precp2$terraclim_prcp

# project the layers with adjusting the resolution
newcrs <- "+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs"
precp <- projectRaster(from = precp2, res = 250, crs = newcrs)
temp_max <- projectRaster(from = temp_max, res = 250, crs = newcrs)
temp_min <- projectRaster(from = temp_min, res = 250, crs = newcrs)
srad <- projectRaster(from = srad, res = 250, crs = newcrs)

# unstack the layers, extract second layer from the raster stack/brick
# if only one layer, it will raise an error
precp_2019_2 <- raster(precp, layer = 1)
temp_max1 <- raster(temp_max, layer = 1)
# export individual layers
writeRaster(temp_max, 'tmax.tif', format = "GTiff", overwrite = TRUE)
writeRaster(temp_min, 'tmin.tif',format = "GTiff",  overwrite = TRUE)
writeRaster(precp, 'precp.tif', format = "GTiff", overwrite = TRUE)
writeRaster(srad, 'srad.tif', overwrite = TRUE)


##############################################################################
aoi <- st_read("C:\\Users\\ATilaye\\Documents\\02Prediction_Test\\cleaned_data_eth\\eth.shp")
srad2 <- getTerraClim(aoi, param = "srad", startDate = "2019-06-15", endDate = "2019-10-15")
srad2 <- srad2$terraclim_srad
writeRaster(
  srad2,
  filename = paste("srad_2019", 
                   for (i in 1:nlayers(srad2)) { i
                   }, sep = "_"),
  bylayer = T,
  format = "GTiff",
  overwrite = TRUE
)
