print(noquote("Attaching packages ..."))
library(raster)
library(dplyr)
library(sf)
require(climateR)
library(AOI)

print(noquote("Reading rasters ..."))
planting_date <- raster("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\Growing_period_mask\\Wheat_Meher_Sowing Window Start Dekad.img")
planting_date[planting_date >= 24] <- NA
planting_date2 <- planting_date * 10 
growing_length <- raster("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\Growing_period_mask\\Wheat_Meher_Cycle_Length.img")
growing_length2 <- growing_length * 10
grow_period_stack<-stack(round(planting_date2-10), round(growing_length2 + growing_length2))
csv_data <-
  read.csv(file = "C:\\Users\\ATilaye\\Documents\\02Prediction_Test\\amhara_prediction_test\\csv_data216_225.csv", header = T, sep = ",")
new_crs <- proj4string(planting_date)
coordinates(csv_data) <- ~lon+lat
proj4string(csv_data) <- new_crs
growing_period <- extract(grow_period_stack, csv_data)
growing_period <- as.data.frame(growing_period)
colnames(growing_period) <- c("planting_date", "harvesting_date")
csv_data$year <-
  as.character(csv_data$year) %>% paste("01", "01", sep = "-") %>% as.Date()
final_csv <- cbind(csv_data, growing_period)
final_csv$planting_date <- final_csv$year + final_csv$planting_date
final_csv$harvesting_date <- final_csv$year  + final_csv$harvesting_date
final_csv <- as.data.frame(final_csv)
sites <- unique(na.omit(final_csv))
# write.csv(final_csv, file = "C:\\Users\\ATilaye\\Documents\\02Prediction_Test\\cleaned_data_eth\\data.csv")
head(sites)
tail(sites)

# sites <- read.csv("C:\\Users\\ATilaye\\Documents\\02Prediction_Test\\cleaned_data_eth\\data.csv", header = T, sep = ",")
# sites$date1 <- as.Date(sites$date1)
# sites$date2 <- as.Date(sites$date2)

tmin_data <- matrix(ncol = 6)
tmin_data <- data.frame(tmin_data)
colnames(tmin_data) <- paste0("tmin", seq(1, 6, 1))
for(i in 1:nrow(sites)){ 
  sites_shp <- st_as_sf(sites[i,], coords = c("lon", "lat"), crs = 4326)
  sites_stack = getTerraClim(
    AOI   = sites_shp,
    param = "tmin",
    startDate = sites_shp$planting_date,
    endDate   = sites_shp$harvesting_date
  )
  sites_stack_t <- t(sites_stack$tmin) #transpose tmin
  sites_stack_t <- as.data.frame(sites_stack_t)
  colnames(sites_stack_t) <- paste0("tmin", seq(1, ncol(sites_stack_t), 1))
  tmin_data <- bind_rows(tmin_data, sites_stack_t)
}
setwd("C:\\Users\\ATilaye\\Documents\\02Prediction_Test\\amhara_prediction_test\\climate_data")
tmin_data <- tmin_data[-which(is.na(tmin_data$tmin1)),] # remove the first na
#tmin_data_final <- cbind.data.frame(sites, tmin_data)
write.csv(tmin_data, file = "tmin216-225.csv")
gc()
