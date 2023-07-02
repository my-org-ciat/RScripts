# ------------------------------------------------------------------------------   
# Function to access and download ISRIC Soil grids of 250m resolution
# Parameters to be added in the function - voi (text), depth(soil depth -text)
# xmin, ymin, xmax and ymax all numbers
# ------------------------------------------------------------------------------

fetchSoilGrid <-
  function(voi, depth, quantile, xmin, ymin, xmax, ymax) {
    library(XML)
    library(rgdal)
    library(gdalUtils)
    library(lattice)
    # library(GSIF)
    library(aqp)
# ------------------------------------------------------------------------------
# GDAL path
    
    if(.Platform$OS.type == "windows"){
      gdal.dir <- shortPathName("C:/Program Files/GDAL")
      gdal_translate <- paste0(gdal.dir, "/gdal_translate.exe")
      gdalwarp <- paste0(gdal.dir, "/gdalwarp.exe") 
      gdalinfo <- paste0(gdal.dir, "/gdalinfo.exe")
      gcs.csv <- paste0(gdal.dir, "/gdal-data/gcs.csv")
    } else {
      gdal_translate = "gdal_translate"
      gdalwarp = "gdalwarp"
      gdalinfo = "gdalinfo"
    }

    bbox <- c(205791.768375, 1646964.804734, 596753.252454, 1355082.168275)
    # igh <- '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
    utm <- "+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs"
    # wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    
    # ------------------------------------------------------------------------------   
    # layer of interest
    voi_layer <-
      paste("nitrogen", "0-5cm", "mean", sep = "_") 
    
    # ------------------------------------------------------------------------------  
    # Path to the WCS. See maps.isric.org
    wcs_path <-
      paste0("https://maps.isric.org/mapserv?map=/map/", "nitrogen", ".map") 
    wcs_service <- "SERVICE=WCS"
    
    # ------------------------------------------------------------------------------
    # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.
    wcs_version <-
      "VERSION=2.0.1" 
    wcs_request <- "DescribeCoverage"
    wcs <-
      paste(wcs_path, wcs_service, wcs_version, wcs_request, sep = "&")
    l1 <- newXMLNode("WCS_GDAL")
    l1.s <- newXMLNode("ServiceURL", wcs, parent = l1)
    l1.l <- newXMLNode("CoverageName", voi_layer, parent = l1)
    xml.out <- paste("nitrogen",".xml")
    saveXML(l1, file = xml.out)
    file.out <- paste(voi_layer,'.tif', sep = "_")
    gdal_translate(
      xml.out,
      file.out,
      tr = c(250, 250),
      projwin = bbox,
      projwin_srs = utm,
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
