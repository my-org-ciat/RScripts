# ------------------------------------------------------------------------------
# Fetches elevation data from Amazon Web Services using and area of interest
# Projects to a 2nd (WGS 1984 UTM Zone 37N) calculates slope and aspect in 
# degree and exports the layers as a tiff
# The projection of the aoi should be in gcs_wgs_84
# ------------------------------------------------------------------------------ 
fetchElevation <- function(aoi, aoi_path = NULL){
  library(raster)
  library(sf)
  library(rgl)
  library(rgdal)
  library(elevatr)
  
# ------------------------------------------------------------------------------
# read area of interest 
  if (is.null (aoi_path)){
    path <- getwd()
    aoi_path <- setwd(path)
    aoi <- st_read(dsn = aoi_path, layer = aoi)
  }
  else {
    aoi_path <- aoi_path
    aoi <- st_read(dsn = aoi_path, layer = aoi)
  }
  
# ------------------------------------------------------------------------------
# fetches data using elevatr package 
  dem <- get_elev_raster(aoi, z = 8)
  utm <- "+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs"
  dem <- projectRaster(dem, crs = utm, res = 250)
  
# ------------------------------------------------------------------------------
# derive slope and aspect
  slope <- terrain(dem,opt='slope', unit='degrees') #slop
  aspect <- terrain(dem,opt='aspect',unit='degrees') #aspect
  aoi <- st_transform(aoi, utm)
  dem <- crop(dem, aoi)
  slope <- crop(slope, aoi)
  aspect <- crop(aspect, aoi)

# ------------------------------------------------------------------------------
# export raster 
  writeRaster(
    dem,
    filename = "dem",
    bylayer = TRUE,
    format = "GTiff",
    overwrite = T
  )
  writeRaster(
    slope,
    filename = "slope",
    bylayer = TRUE,
    format = "GTiff",
    overwrite = T
  )
  writeRaster(
    aspect,
    filename = "aspect",
    bylayer = TRUE,
    format = "GTiff",
    overwrite = T
  )
}
fetchElevation(AOI = "bg_prj", "D:/bg")
fetchElevation(AOI = "bg_prj")
