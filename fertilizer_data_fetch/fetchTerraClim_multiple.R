fetchTerraClimate <- function(param, AOI, start_date, AOI_path, end_date = NULL){
  library(AOI)
  library(rgdal)
  # library(gdalUtils)
  library(climateR)
  library(sf)
  library(raster)
  library(dplyr)
  #AOI Layer
  AOI <- st_read(dsn = AOI_path, layer = AOI, sep = "/")
  param <-
    getTerraClim(AOI = AOI, param = param, startDate = start_date, endDate = end_date)
  param <- param[[1]]
  # old_crs <- "EPSG:4326"
  newcrs <- "+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs"
  # newcrs <- "EPSG:32637"
  param <- projectRaster(from = param, res = 250, crs = newcrs)
  # param <- spTransform(param, crs = newcrs)
  rname <- c(names(param))
  for (i in 1:length(rname)) {
    writeRaster(
      param,
      filename = paste(rname[i]),
      bylayer = TRUE,
      format = "GTiff",
      overwrite = T
    )
  }
}
fetchTerraClimate("tmax", "tigray_prj","2014-02-01", "D:/tigray","2014-04-01")
