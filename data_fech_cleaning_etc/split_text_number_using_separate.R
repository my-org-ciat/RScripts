library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)


#setwd("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\digital_green\\geoserver\\shape")
setwd("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\digital_green\\final_june_22\\shape_dissolved")
#aoi <- readOGR("eth_wheat__fertilizer_recommendation_normal_final.shp")

rm(list = ls())
aoi <- readOGR("siya_debir.shp")

head(aoi@data)
nrow(aoi@data)
data <- aoi@data
npsb_ur <- data$NPSB_Ur %>% as.data.frame()
names(npsb_ur) <- ("npsb_ur")
npsb_ur2 <- npsb_ur %>% separate(npsb_ur, c("NPSB1", "Urea1"), sep = "&") %>%
  mutate(NPSB = parse_number(NPSB1), Urea = parse_number(Urea1)) %>% select(-c("NPSB1", "Urea1"))
data2 <- cbind(data, npsb_ur2)
nps_ur <- data$NPSBZ_Ur %>% as.data.frame()
names(nps_ur) <- ("nps_ur")
nps_ur2 <- nps_ur %>% separate(nps_ur, c("NPS1", "Urea1"), sep = "&") %>%
  mutate(NPSBZn = parse_number(NPS1), Urea2 = parse_number(Urea1)) %>% select(-c("NPS1", "Urea1"))
data3 <- cbind(data2, nps_ur2)
aoi@data <- data3 %>% select(-c("Urea2"))
writeOGR(aoi, dsn = ".", layer = "siya_debir2", driver = "ESRI Shapefile")
