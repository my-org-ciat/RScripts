# ------------------------------------------------------------------------------
# Fetches elevation data from Amazon Web Services using and area of interest
# ------------------------------------------------------------------------------ 

fetchElevation <- function(aoi, aoi_path = NULL){
  require(raster)
  require(sf)
  require(rgl)
  require(rgdal)
  require(elevatr)
  require(dplyr)
  require(sp)
  
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
  
  dem <- get_elev_raster(aoi, z = 9) %>% crop(aoi) %>% mask(aoi)
  print(noquote("Croping and masking rasters"))
  slope <-
    terrain(dem, opt = 'slope', unit = 'degrees') %>% crop(aoi) %>% mask(aoi)
  aspect <-
    terrain(dem, opt = 'aspect', unit = 'degrees') %>% crop(aoi) %>% mask(aoi)
  tpi <- terrain(dem, opt = 'TPI') %>% crop(aoi) %>% mask(aoi)
  tri <- terrain(dem, opt = 'TRI') %>% crop(aoi) %>% mask(aoi)
  
  ter_stack <- stack(dem, slope, aspect, tpi, tri)
  rname <-  c("dem", "slope", "aspect", "tpi", "tri")
  
  for (i in 1:nlayers(ter_stack)) {
    print(noquote(paste("Writing", rname[i], "raster", sep = " ")))
    writeRaster(ter_stack[[i]],
                filename = paste(rname[i], "tif", sep = "."),
                format = "GTiff", overwrite = T)
  }
}

#Example
fetchElevation(aoi = "eth", aoi_path = "C:\\Users\\ATilaye\\Documents\\01My_Docs\\script_test\\input")
fetchElevation(aoi = "eth")
