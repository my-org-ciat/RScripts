# ------------------------------------------------------------------------------
# Fetches climate data from TerraClim
# ------------------------------------------------------------------------------

fetchTerraClimate <-
  function(param,
           aoi,csv, 
           aoi_path,
           csv_path) {
    
    require(AOI)
    require(rgdal)
    require(climateR)
    require(sf)
    require(raster)
    require(dplyr)
    
    file_name <- as.character(param)
# ------------------------------------------------------------------------------   
# Read shape and csv data
    file_name <- as.character(param)
    aoi <- st_read(paste(dsn = aoi_path, layer = aoi, sep = "/"))
    data <- read.csv(file = paste(csv_path, csv, sep = "/"), header = TRUE, sep = ",")
    data2 <-
      dplyr::select(data,-c("District", "Data..Source", "Elevation", "Soil"))
    data2$Year <-
      as.character(data2$Year) %>% paste("01", "01", sep = "-") %>% as.Date()
    data3 <-
      data2 %>% mutate(
        plating_date2 = Year + Planting_date - 10,
        harvesting_date2 = Year + Harvesting_date + 10
      ) %>% unique.data.frame()
    
    start_date <- min(data3$plating_date2) # assuming the same year 
    end_date <- max(data3$harvesting_date2) # for planing & harvesting date
# ------------------------------------------------------------------------------   
# Access teraclim data
    pout = getTerraClim(aoi,
                      param = param,
                      startDate = start_date,
                      endDate = end_date)
    
# ------------------------------------------------------------------------------    
# Export individual layers
    pout = pout[[1]]
    writeRaster(
      pout,
      filename = paste(file_name, 
          for (i in 1:nlayers(pout)) { i
      }, sep = "_"),
      bylayer = T,
      format = "GTiff",
      overwrite = TRUE
    )
  }

#example
fetchTerraClimate (param, aoi, csv, aoi_path, csv_path)