install.packages("readxl", dependencies = T, type = "source")
install.packages("stringr", dependencies = T, type = "source")

library(readxl)
library(stringr)
library(plyr)
library(dplyr)

setwd("C:\\Users\\ATilaye\\Documents\\mame")
data <- readxl::read_xlsx("sample multiple resposen.xlsx", col_names = T, sheet = "Sheet1")

data$col[data$col == 888] <- "a"
data[c("none","governemnt_extension","researchers","farmer.group_cooperative",	
       "kebele_group","NGO_CBO","agro_dealers","relative_neighbour.friend",	
       "formal_education","other_specify")] <- str_split_fixed(data$col, '|', 10)

del <- colwise(function(x) str_replace_all(x, '|', ""))
x <- del(data) %>% select(-c(col))

# suppressWarnings(for(i in 1:ncol(x)){
#   x[,i] <- as.numeric(ifelse(x[,i] == "|", NA, x[,i]))
# })

suppressWarnings(for(i in 1:ncol(x)){
  x[,i] <- ifelse(x[,i] == "|", NA, x[,i])
})

dim(x)

r <- x
for(i in 1:ncol(r)){
  for(j in 1:nrow(r)){
    y <- r[j,i]
    if(is.na(y) || y == 0 || y == ""){
      r[j, i] <- 0
    }
    if(y > 0 & !is.na(y)){
      r[j,i] <- y
    }
  }
}

#with number
s <- r
for(i in 1:ncol(s)){
  for(j in 1:nrow(s)){
    k <- s[j,i]
    if(!is.na(as.integer(k)) == TRUE){
      z <- as.integer(s[j,i])
      if(z == 0){
        s[j, i] <- 0
      }
     else if(z == i-1){
        r[j,i] <- s[j,i]
      }
      # if(z > i+1){
      else{
        s[j, z+1] <- z
        s[j, i] <- 0
       }
    }
  else{
    s[j, ncol(s)] <- k
    #s[j, i] <- 0
  }
  }
}





