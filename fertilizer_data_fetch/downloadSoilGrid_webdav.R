# ------------------------------------------------------------------------------
# Install gdal driver for windows by downloading from
# https://www.gisinternals.com/query.html?content=filelist&file=release-1911-x64-gdal-2-4-4-mapserver-7-4-3.zip
# ------------------------------------------------------------------------------
library(XML)
library(rgdal)
library(gdalUtils)
library(raster)
library(sf)
library(dplyr)
library(RColorBrewer)
library(leaflet.opacity)
library(leaflet)
library(leaflet.opacity)
library(mapview)
setwd("D:/cov")
url = "https://files.isric.org/soilgrids/latest/data/"
voi = "nitrogen" # variable of interest
depth = "15-30cm"
quantile = "mean"
(variable = paste(url, voi, sep=""))
(layer = paste(variable,depth,quantile, sep="_"))
(vrt_layer = paste(layer, '.vrt', sep=""))
nitro = raster("https://files.isric.org/soilgrids/latest/data/nitrogen/nitrogen_15-30cm_mean.vrt")
nitro
tigray <- st_read("D:/tigray/tigray.shp")
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
tig_igh <- st_transform(boyaca, igh)
tig_igh <- st_transform(tigray, igh)

# ------------------------------------------------------------------------------
# setting bounding box
# ------------------------------------------------------------------------------
(bbox <- st_bbox(tig_igh))
ulx = bbox$xmin
uly = bbox$ymax
lrx= bbox$xmax
lry = bbox$ymin
(bb <- c(ulx, uly, lrx, lry))
sg_url="/vsicurl/https://files.isric.org/soilgrids/latest/data/"
datos = 'nitrogen/nitrogen_5-15cm_mean.vrt'
lfile = "./nitro_ntest_igh_5_15.tif"
gdal_translate(paste0(sg_url,datos), lfile ,
               tr=c(250,250),
               projwin=bb,
               projwin_srs =igh,
               verbose=TRUE)
