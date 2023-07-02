library(ggplot2)
library(plyr)

setwd("C:\\Users\\ATilaye\\Downloads")
#Mohammed
#wheat grain yield density plot across all locations
filename <- "Wheat Advisory Data_2021-2023.csv"
dataset <- read.csv(filename, header=TRUE, sep = ",") 
#read.csv("Mohammed.csv", header = TRUE) 
head(dataset)

cols <- c("#5DADE2", "#884EA0", "#2ECC71", "#F1C40F", "#283747", "#7B241C") # set color to fill the density plot
plot16 <- ggplot(dataset, aes(x=GYton, fill=advisory))+
  geom_density(alpha = 0.5)+
    labs(title = " ",x = "bio yield, t/ha",y = "Density")
plot16

#To add mean by group, calculate the mean of each group
mu <- ddply(dataset, "advisory", summarise, grp.mean=mean(GYton))
head(mu)
#theme
#plot16 + theme(axis.line = element_line(colour = "black"))

#plot the vertical line mean with the density plot
plot16 + geom_vline(data = mu, aes(xintercept=grp.mean, color=advisory),
                    linetype="dashed", size=1)
