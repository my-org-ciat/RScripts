library(dplyr)
library(sf)
require(climateR)
library(AOI)

sites <- read.csv("C:\\Users\\ATilaye\\Documents\\02Prediction_Test\\cleaned_data_eth\\data.csv", header = T, sep = ",")
sites$date1 <- as.Date(sites$date1)
sites$date2 <- as.Date(sites$date2)
sites <- unique(na.omit(sites))
srad_data <- matrix(ncol = 6)
srad_data <- data.frame(srad_data)
colnames(srad_data) <- paste0("srad", seq(1, 6, 1))
for(i in 1:nrow(sites)){ 
  sites_shp <- st_as_sf(sites[i,], coords = c("lon", "lat"), crs = 4326)
  sites_stack = getTerraClim(
    AOI   = sites_shp,
    param = "srad",
    startDate = sites_shp$date1,
    endDate   = sites_shp$date2
  )
  sites_stack_t <- t(sites_stack$srad) #transpose srad
  sites_stack_t <- as.data.frame(sites_stack_t)
  colnames(sites_stack_t) <- paste0("srad", seq(1, ncol(sites_stack_t), 1))
  srad_data <- bind_rows(srad_data, sites_stack_t)
}
srad_data <- srad_data[-which(is.na(srad_data$srad1)),] # remove the first na
srad_data_final <- cbind.data.frame(sites, srad_data)

t1 <- as.data.frame(sites_wide$terraclim_srad)

t2 <- t(sites_wide$terraclim_srad) # transpose the data frame
colnames(t1) <- t1[1,] # changing the colnames
t2 <- t1[-1,] # removing the first row
