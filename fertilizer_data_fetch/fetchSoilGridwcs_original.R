fetchSoilGrid <-
  function(voi, depth, quantile, xmin, ymin, xmax, ymax) {
    library(XML)
    library(rgdal)
    library(gdalUtils)
    #voi = "nitrogen" # variable of interest
    #depth = "5-15cm"
    #quantile = "Q0.5"
    bbox <- c(xmin, ymin, xmax, ymax)
    proj_name <- '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
    voi_layer <-
      paste(voi, depth, quantile, sep = "_") # layer of interest
    wcs_path <-
      paste0("https://maps.isric.org/mapserv?map=/map/", voi, ".map") # Path to the WCS. See maps.isric.org
    wcs_service <- "SERVICE=WCS"
    wcs_version <-
      "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.
    wcs_request <- "DescribeCoverage"
    wcs <-
      paste(wcs_path, wcs_service, wcs_version, wcs_request, sep = "&")
    l1 <- newXMLNode("WCS_GDAL")
    l1.s <- newXMLNode("ServiceURL", wcs, parent = l1)
    l1.l <- newXMLNode("CoverageName", voi_layer, parent = l1)
    xml.out <- paste(voi,".xml")
   saveXML(l1, file = xml.out)
    file.out <- paste(paste(voi,depth,quantile,sep = "_"),'.tif')
   gdal_translate(
      xml.out,
      file.out,
      tr = c(250, 250),
      projwin = bbox,
      projwin_srs = proj_name,
      co = c(
        "TILED=YES",
        "COMPRESS=DEFLATE",
        "PREDICTOR=2",
        "BIGTIFF=YES",
        "GDAL_HTTP_UNSAFESSL=YES"
      ),
      verbose = TRUE
    )
  }
