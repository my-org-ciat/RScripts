# ------------------------------------------------------------------------------
# Fetches climatic data from TerraClim using and area of interest
# exports the layers as a tiff
# The projection of the aoi should be in gcs_wgs_84
# ------------------------------------------------------------------------------
fetchTerraClimate <-
                  function(param,
                           aoi,
                           aoi_path = NULL,
                           start_date,
                           end_date = NULL) {
  library(AOI)
  library(rgdal)
  library(climateR)
  library(sf)
  library(raster)
  library(dplyr)
  
# ------------------------------------------------------------------------------
# aoi Layer and end date
  file_name <- as.character(param)
  # date_1 <- as.Date(start_date)
  # date_2 <- as.Date(end_date)
  # dates <- seq(from = date_1, to = date_2, by = 'month')
  if (is.null(aoi_path)){
    path <- getwd()
    aoi_path <- setwd(path)
    aoi <- st_read(paste(dsn = aoi_path, layer = aoi, sep = "/"))
  }
  else {
    aoi_path <- aoi_path
    aoi <- st_read(paste(dsn = aoi_path, layer = aoi, sep = "/"))
  }
  # aoi <- st_read(paste(dsn = aoi_path, layer = aoi, sep = "/"))
  
# ------------------------------------------------------------------------------
# parameter
  param <-
    getTerraClim(
      AOI = aoi,
      param = param,
      startDate = start_date,
      endDate = end_date
    )
  param <- param[[1]]
  # newcrs <- "+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs"
  # param <- projectRaster(from = param, res = 250, crs = newcrs)
  rname <- c(names(param))
  # names(param) <- dates
  
# ------------------------------------------------------------------------------
# write raster
  for (i in 1:length(rname)) {
    writeRaster(
      param,
      filename = paste(file_name, rname[i], sep = "_"),
      bylayer = TRUE,
      format = "GTiff",
      overwrite = T
    )
  }
}

#Example
fetchTerraClimate("tmin", aoi ="eth.shp","D:/Test","2014-02-01","2014-05-01")
fetchTerraClimate("tmax", aoi ="eth.shp","D:/Test","2014-02-01","2014-05-01")
fetchTerraClimate("prcp", aoi ="eth.shp","D:/Test","2014-02-01","2014-05-01")
fetchTerraClimate("srad", aoi ="eth.shp","D:/Test","2014-02-01","2014-05-01")

