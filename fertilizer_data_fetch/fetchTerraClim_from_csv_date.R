library(dplyr)
library(tidyverse)
data <- read.csv(file = "test_data.csv", header = TRUE, sep = ",")
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
for(i in length(start_date)){
  startdate <- start_date[i]
  enddate <- end_date[i]
}

