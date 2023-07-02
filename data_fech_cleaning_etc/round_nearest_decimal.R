library(sp)
library(dplyr)
library(plyr)

rm(list = ls())
setwd("C:\\Users\\Amsalu\\Desktop\\Recom_units_11_june_2022")
mareko <- readOGR("Mareko.shp")
plot(mareko)
head(mareko@data)
NPS <- as.integer(mareko@data$NPS) %>% round_any(10)
mareko@data$NPS <- NPS
NPSB <- as.integer(mareko@data$NPSB) %>% round_any(10)
mareko@data$NPSB <- NPSB
urea <- as.integer(mareko@data$Urea) %>% round_any(10)
mareko@data$Urea <- urea
head(mareko@data)
x <- paste0("  ", as.character(mareko@data$NPS), "NPS"," & ", as.character(mareko@data$Urea), "Urea")
mareko@data$NPS_Urea <- x
y <- paste0("  ", as.character(mareko@data$NPSB), "NPSB"," & ", as.character(mareko@data$Urea), "Urea")
mareko@data$NPSB_Urea <- y
head(mareko)
setwd("recom_unit_edit")
writeOGR(mareko, dsn =".", layer = "mareko", driver = "ESRI Shapefile", overwrite_layer = T)

#for csv
setwd("C:\\Users\\ATilaye\\Downloads")
csv <- readxl::read_xlsx("Fertlizer_rate.xlsx", sheet = "Sheet1")
NPS <- as.integer(csv$NPS2) %>% round_any(10)
csv$NPS <- NPS
urea <- as.integer(csv$Urea2) %>% round_any(10)
csv$Urea <- urea
x <- paste0( as.character(csv$NPS), "NPS"," & ", as.character(csv$Urea), "Urea")
csv$NPS_Urea <- x
y <- paste0( csv$Compost, "," , csv$Vermi_comp)
csv$Organic <- y

csv2 <- csv[,c(1:3,6:10)] %>% csv()
write.csv(csv2, "fertilizer2.csv", col.names = T, row.names = F)