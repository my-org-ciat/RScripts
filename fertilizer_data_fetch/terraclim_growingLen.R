library(raster)
growing_length_raster <- raster("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\Growing_period_mask\\Wheat_Meher_Cycle_Length.img")
# grid <- as(growing_length, "SpatialGridDataFrame")
data <- read.csv("C:\\Users\\ATilaye\\Documents\\02Prediction_Test\\covariates\\data.csv", header = T, sep = ",")
coordinates(data) <- ~lon+lat
new_crs <- crs(growing_length_raster)
crs(data) <- new_crs
growing_length <- extract(growing_length_raster, data)
pointValues <- as.data.frame(data, growing_length)
data2 <-
  dplyr::select(pointValues, c("Region", "Wereda", "Kebele", "Crop","Varity", "Year", "growing_length"))
data2$Year <-
  as.character(data2$Year) %>% paste("01", "01", sep = "-") %>% as.Date()
data3 <-
  data2 %>% mutate(
    growing_period = Year + growing_length) %>%
   filter(growing_period = distinct(growing_period)) %>% unique.data.frame()

# loop for selecting min and max dates as loop





start_date <- min(data3$growing_period)
end_date <- max(data3$growing_period)

unq_date <- unique(isoyear(data3$growing_period))#selects unique year
for(i in 1:length(unq_date)){
  if(data3$growing_period %in% unq_date[i])
  start_date[i] <- max(date, year = unq_date[i])
}


