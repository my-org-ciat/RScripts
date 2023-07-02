require(XML)
require(sf)
require(rgdal)
require(gdalUtils)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
aoi = st_read("amhara.shp")
aoi_igh <- st_transform(aoi, igh)
am_extent <- st_bbox(aoi_igh)
x_min <- am_extent$xmin
y_max <- am_extent$ymax
x_max <- am_extent$xmax
y_min <- am_extent$ymin
voi <-"clay"
depth <- "0-5cm"
quantile <- "Q0.5"
wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to the WCS. See maps.isric.org
wcs_service = "SERVICE=WCS"
wcs_version = "VERSION=2.0.0"
wcs_request = "REQUEST=GetCoverage"
voi_layer = paste0("COVERAGEID=", paste(voi,depth,quantile, sep="_")) # layer of interest
format = "FORMAT=GEOTIFF_INT16"
subsetx="SUBSET=X(3912601.1,4455127)"
subsety="SUBSET=Y(970128.5 ,1532086)"
subset_crs = "SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/152160"
output_crs = "OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/152160"

wcs = paste(wcs_path,
            wcs_service,
            wcs_version,
            wcs_request,
            voi_layer,
            format,
            subsetx,
            subsety,
            subset_crs,
            output_crs,
            sep = "&")

download.file(wcs,  'silt', overwrite=TRUE, quiet = F)


# bb=c(-337500.000,1242500.000,152500.000,527500.000)
