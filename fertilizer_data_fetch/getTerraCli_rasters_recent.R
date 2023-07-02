library(raster)
library(dplyr)
planting_date <- raster("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\Growing_period_mask\\Wheat_Meher_Sowing Window Start Dekad.img")
planting_date[planting_date >= 24] <- NA
planting_date2 <- planting_date * 10 
growing_length <- raster("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\Growing_period_mask\\Wheat_Meher_Cycle_Length.img")
growing_length2 <- growing_length * 10
grow_period_stack<-stack(round(planting_date2-10), round(growing_length2 + growing_length2))
csv_data <- read.csv(file = "C:\\Users\\ATilaye\\Documents\\02Prediction_Test\\cleaned_data_eth\\data_wheat.csv", header = T, sep = ",")
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
write.csv(final_csv, file = "C:\\Users\\ATilaye\\Documents\\02Prediction_Test\\cleaned_data_eth\\data.csv")
head(final_csv)
tail(final_csv)