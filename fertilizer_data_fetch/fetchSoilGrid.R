.extractSGLayerProperties <- function(jsonres, x, scaling.factor = 1) {
  out <-  jsonres$properties$layers[jsonres$properties$layers$name == x,]$depths[[1]]
  
  # rescale integer values to common units
  sgvalues <-  out[, "values"][, c("Q0.05", "Q0.5", "Q0.95", "mean")] * scaling.factor
  
  # uncertainty is always scaled by a factor of 10, even for bdod and nitrogen (whose data are scaled by 100)
  uncertainty <- out[, "values"][, "uncertainty"] * 0.1
  
  out[["values"]] <- cbind(sgvalues, uncertainty)
  
  # fix names and labels for downstream
  out <- out[,colnames(out)[grep("range", colnames(out), invert = TRUE)]]
  out <- data.frame(label = gsub("cm", "", out$label), values = out$values)
  colnames(out) <- gsub("\\.Q0\\.", "Q", colnames(out))
  colnames(out) <- gsub("Q5", "Q50", colnames(out))
  colnames(out) <- gsub("values", x, colnames(out))
  colnames(out) <- gsub("\\.", "", colnames(out))
  
  return(out)
}
fetchSoilGrids <- function(x,
                           loc.names = c("id", "lat", "lon"),
                           verbose = FALSE,
                           progress = FALSE) {
  
  locations <- x
  spatial_input <- FALSE
  
  if (inherits(locations, 'sf') || inherits(locations, 'Spatial')) {
    if (requireNamespace("sf")) {
      if (inherits(locations, 'Spatial')) {
        # convert sp -> sf
        locations <- sf::st_as_sf(locations)
      }
      
      # only supporting POINT geometry for now
      if (inherits(sf::st_geometry(locations), 'sfc_POINT')) {
        if (is.na(sf::st_crs(locations)$wkt)) {
          message("CRS is missing; assuming WGS84 decimal degrees (EPSG:4326)")
          sf::st_crs(locations) <- sf::st_crs(4326)
        }
        locations <- data.frame(id = 1:nrow(locations), 
                                do.call('rbind', sf::st_geometry(locations)))
        spatial_input <- TRUE
        colnames(locations) <- c("id", "lon", "lat")
        loc.names <- c("id", "lat", "lon")
      } else {
        stop("only POINT geometries are supported as input", call. = FALSE) 
      }
    }
  }
  
  if (is.null(loc.names)) {
    loc.names <- c("id", "lat", "lon")
  }
  
  if (length(loc.names) != 3 | any(!loc.names %in% colnames(locations))) {
    stop("argument `loc.names` must contain three column names: site ID, latitude and longitude", call. = FALSE)
  }
  
  if (!is.numeric(locations[[loc.names[2]]]) || 
      !is.numeric(locations[[loc.names[3]]])) {
    stop(sprintf("latitude (%s) and longitude (%s) must be numeric values in decimal degrees", loc.names[2], loc.names[3]), call. = FALSE)
  }
  
  if (progress) {
    pb <- txtProgressBar(min = 0, max = nrow(locations), style = 3)
  }
  
  res <- vector('list', nrow(locations))
  locsplit <- split(locations, f = locations[[loc.names[1]]])
  for (i in seq_along(res)) {
    yd <- locsplit[[i]]
    id <- as.character(yd[[loc.names[1]]])
    lat <- as.numeric(yd[[loc.names[2]]])
    lon <- as.numeric(yd[[loc.names[3]]])
    
    if (any(length(lat) == 0, length(lon) == 0)) {
      if (verbose)
        message(sprintf("skipped site ID (%s); 0-length coordinates", id))
      res[[i]] <- NULL    
    }
    
    response <- try(httr::GET(sprintf("https://rest.isric.org/soilgrids/v2.0/properties/query?lat=%s&lon=%s", lat, lon)), silent = !verbose)
    
    if (inherits(response, 'try-error')) {
      if (verbose)
        message(sprintf("SoilGrids API request failed for %s, %s", lat, lon))
      res[[i]] <- NULL    
    }
    
    r.content <- httr::content(response, as = "text", encoding = "UTF-8")
    jres <- jsonlite::fromJSON(r.content)
    
    # create new horizon data, merge in each property using standard depth labels
    depth.intervals <-  c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
    hz.data <- data.frame(id = id, latitude = lat, longitude = lon, label = depth.intervals, stringsAsFactors = FALSE)
    
    # values returned for each layer include the following properties
    data.types <- c("bdod", "cec", "cfvo", "clay", "nitrogen", "phh2o", "sand", "silt", "soc")
    
    # numeric values are returned as integers that need to be scaled to match typical measurement units
    data.factor <- c(0.01, 0.1, 0.1, 0.1, 0.01, 0.1, 0.1, 0.1, 0.1)
    
    for (d in 1:length(data.types)) {
      hz.data <- merge(hz.data, .extractSGLayerProperties(jres, data.types[d], data.factor[d]), by = "label")
    }
    
    rownames(hz.data) <- NULL
    
    if (progress) {
      setTxtProgressBar(pb, i)
    }
    
    res[[i]] <- hz.data
  }
  
  # combine horizon data together
  spc <- do.call('rbind', res)
  
  # calculate top and bottom depths from label
  labelsplit <- strsplit(as.character(spc$label), split = "-")
  
  spc$hzdept <- as.numeric(lapply(labelsplit, function(x) x[1]))
  spc$hzdepb <- as.numeric(lapply(labelsplit, function(x) x[2]))
  
  # promote to SoilProfileCollection
  depths(spc) <- id ~ hzdept + hzdepb
  
  # move location information to site
  site(spc) <- ~ longitude + latitude
  coordinates(spc) <- ~ longitude + latitude
  proj4string(spc) <- "EPSG:4326"
  
  # merge the rest of the sf object into the site table 
  if (spatial_input) {
    site(spc) <- cbind(id = 1:nrow(x), sf::st_drop_geometry(x))
  }
  
  return(spc)
}

