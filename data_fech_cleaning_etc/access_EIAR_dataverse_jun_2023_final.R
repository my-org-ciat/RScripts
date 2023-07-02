remotes::install_github("iqss/dataverse-client-r")
library(dataverse)
library(dplyr)
library(tibble)
library(stringr)
library(readxl)
library(ggplot2)
library(ggpubr) 
library(reshape)

data <- 
  get_dataframe_by_name(
  filename   = "Fertilizer data.tab",
  dataset    = "doi:10.7910/DVN/M8FCSL",
  server     = "dataverse.harvard.edu",
  original   = TRUE,
   .f         = readxl::read_xlsx) %>% as.data.frame()

#view the structure of data
colnames(data)
str(data)
summary(data)

#view the distribution of fertilizers
fert <- data[,c(22,24,26,28)] 
ggplot(data = melt(fert), 
       aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=variable))+
  theme_bw()

#check the mean productivity of farmers by plot size
prod <- data[,c(17,20)] 
ggplot(data = melt(prod), 
       aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=variable))+
  theme_bw()

#categorical variables
ggplot(data = data, 
       aes(x = data[,19], fill = data[,19])) +
  geom_bar(fill = "#E67E22")+
  theme_bw()

my_df %>% 
  summarise_all(~sum(is.na(.)))
