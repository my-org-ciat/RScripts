library(dplyr)
library(tidyverse)
library(AOI)
library(climateR)
library(sf)
library(raster)
data <- read.csv(file = "D:/extract_TerraClim_data/test_data.csv", header = TRUE, sep = ",")
data2 <- 
  dplyr::select (data,
                 "Region",
                 "District",
                 "Site",
                 "Year",
                 "Planting_date",
                 "Harvesting_date")
data2$Year <- as.character(data2$Year)
data2$Year <- paste(data2$Year, "01", "01", sep = "-")
data2$Year <- as.Date(data2$Year)
data3 <-
  data2 %>% mutate(
    plating_date2 = Year + Planting_date - 10,
    harvesting_date2 = Year + Harvesting_date + 10
  )
data4 <- unique.data.frame(data3)
start_date <- data4$plating_date2
end_date <- data4$harvesting_date2
shape <- st_read("D:/tigray/tigray.shp")
tmax2 <- list()
for(i in 1:length(start_date)){
  tmax = getTerraClim(shape,
                        param = "tmax",
                        startDate = start_date[i],
                        endDate = end_date[i])
  tmax <- tmax[[1]]
  tmax2  <- append(tmax2, tmax)
  for(j in 1:length(tmax2)){
    tmax <- tmax2[[j]]
    writeRaster(
    tmax,
    filename = paste(
      paste("tmax", as.character(start_date[i]), sep = "_"),
      as.character(end_date[i]),
      sep = "_"
    ),
    format = "GTiff",
    overwrite = TRUE
   )
  }
}

