# ------------------------------------------------------------------------------   
# Function to access and download ISRIC Soil grids of 250m resolution
# Parameters to be added in the function - voi (variable of interset text), 
# depth(soil depth - text); xmin, ymin, xmax and ymax all numbers
# voi - "bdod", "cec", "cfvo", "clay", "nitrogen", "phh2o","sand", "silt", "soc"
# depth - "0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm"
# ------------------------------------------------------------------------------
fetchSoilGrid <-
  function(voi, depth, quantile, xmin, ymin, xmax, ymax) {
    library(XML)
    library(rgdal)
    library(gdalUtils)
    bbox <- c(xmin, ymin, xmax, ymax)
    # utm <- "+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs"
    igh <- "+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs"
    
# ------------------------------------------------------------------------------   
# layer of interest
    voi_layer <-
      paste(voi, depth, quantile, sep = "_") 
    
# ------------------------------------------------------------------------------  
# Path to the WCS
    wcs_path <-
      paste0("https://maps.isric.org/mapserv?map=/map/", voi, ".map") 
    wcs_service <- "SERVICE=WCS"

# ------------------------------------------------------------------------------
# This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.
    wcs_version <- "VERSION=2.0.1" 
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
      # srcwin = c(0,0),
      projwin = bbox,
      projwin_srs = igh,
      # a_srs = igh, #assign srs
      co = c(
        "TILED=YES",
        "COMPRESS=DEFLATE",
        "PREDICTOR=2",
        "BIGTIFF=YES",
        "GDAL_HTTP_UNSAFESSL=YES"
      ),
      verbose = TRUE
    )
    gdalwarp(file.out, file.out,
    s_src = utm, 
    t_srs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
    of = "GTiff",
    overwrite = TRUE
)
  }

fetchSoilGrid ("bdod","0-5cm","mean", -337500.000,1242500.000,152500.000,527500.000)
fetchSoilGrid ("bdod","0-5cm","mean", 205791.768375,1646964.804734,596753.252454 ,1355082.168275)
