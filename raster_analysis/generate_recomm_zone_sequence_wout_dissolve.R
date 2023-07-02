#This scripts rounds nps, urea, compost, v_compost; conecatenets and finally
#add a recommendation cluster number

library(dplyr)
library(plyr)

#remove objects from R env't
rm(list = ls())

#set working directory and read csv
setwd("C:\\Users\\ATilaye\\Downloads")
csv <- read.csv("fertilizer3.csv", header = T, sep = ",") 
colnames(csv)

#round NPS & Urea to the nearest ten
NPS2 <- as.integer(csv$NPS) %>% round_any(10)
csv$NPS <- NPS2
urea2 <- as.integer(csv$Urea) %>% round_any(10)
csv$Urea <- urea2

comp2 <- as.integer(csv$Compost)
csv$Compost <- comp2
vcomp2 <- as.integer(csv$Vermi_comp)
csv$Vermi_comp <- vcomp2

#create a column and concatenate NPS_Urea & Comp_V-Comp
x <- paste0( as.character(csv$NPS), "NPS"," & ", as.character(csv$Urea), "Urea")
csv$NPS_Urea <- x
y <- paste0( csv$Compost, "," , csv$Vermi_comp)
csv$Organic <- y

# create a recommendation unit column
colnames(csv)
unq_kebele <- csv %>% na.omit() %>% distinct(RK_NAME) 
all_data <- data.frame()
for(i in 1:nrow(unq_kebele)){
  df <- csv %>% filter(RK_NAME == unq_kebele[i,]) 
  df2 <- transform(df, ID = as.numeric(factor(NPS_Urea)))
  df$recomm_id <- df2[,ncol(df2)]
  if(ncol(all_data) == 0){
    all_data = df
  } else{
    all_data = rbind(all_data, df)
  }
}

#check the column names and write the data
colnames(all_data)
write.csv(all_data, "fertilizer333.csv", row.names = F)

