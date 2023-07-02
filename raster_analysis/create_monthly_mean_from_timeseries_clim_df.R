#calculate monthly climatic data
rm(list = ls())
setwd("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\08RothC\\Bethel\\data\\workspace")
clim_csv <- list.files(pattern = ".csv$")

pet1 <- read.csv("pet_01_22.csv", header = T, sep = ",")
rf1 <- read.csv("rf_01_22.csv", header = T, sep = ",")
temp1 <- read.csv("temp_01_22.csv", header = T, sep = ",")

dir.create("final", recursive = F)
setwd("./final")

#pet
pet_mean_data <- c()
for (i in 1:12){
  data_year <- c()
  k <- i
  for(j in 1:(ncol(pet1)/12)){
    print(k)
    ele <- pet1[,k]
    data_year <- append(data_year, ele)
    k <- k + 12
  }
  mean <- mean(data_year)
  pet_mean_data <- append(pet_mean_data, mean)
}