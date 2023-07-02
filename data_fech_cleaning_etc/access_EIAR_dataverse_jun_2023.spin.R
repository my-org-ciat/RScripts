#Accessing cow dataset from EIAR datahub
#The whole data should be in one doi, the user selects the columns he wants 
#to download, but there should be a notification email for downloading
#the data accessing should be
#1. Access the data by its doi
#2. make exploratory data analysis
#3. Download subset or all of the data
 rm(list = ls())
#install and load dataverse package 
remotes::install_github("iqss/dataverse-client-r")
library(dataverse)
library(dplyr)
library(tibble)
library(stringr)
library(readxl)

#set the dataverse server you want to work with
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")

#search for data that mentions ecological inference
search_result <- dataverse_search("ecological inference")

search_result <- dataverse_search("Fertilizer use by farmers of Indo-Gangetic Plains of India and its determinants")
#to view the serach result
#view column names of the search
names(search_result) 
search_result[,c(1,3,13,12)]

#select major information for the search result
search_result %>% select(c("name", "file_type", "file_persistent_id", "fileCount", 
                           "majorVersion", "minorVersion","authors", "size_in_bytes", 
                           "description"))

#count the number of files in the search based on category
search_result %>% group_by(file_type) %>% summarise(n = n())

#select those files with no doi
sum(is.na(search_result$file_persistent_id))


#to access a datasets
#select tab delimited datesets from the list of files in the search
df <- search_result %>% select(c("name", "dataset_persistent_id","file_type" )) %>%
              filter(file_type == "Tab-Delimited")

#reading the first file in the dataset
df_tab <-
  get_dataframe_by_doi(
    filedoi      = str_split(df$dataset_persistent_id,":")[[1]][2]
  )

dataset <- get_dataset("doi:10.7910/DVN/M8FCSL", server = "dataverse.harvard.edu")
str(dataset_metadata("10.7910/DVN/ARKOTI", server = "dataverse.harvard.edu"), 
    max.level = 2)
get_dataframe_by_doi("10.7910/DVN/ARKOTI", server = "dataverse.harvard.edu")

energy <- get_dataframe_by_name(
  filename = "comprehensiveJapanEnergy.tab",
  dataset = "10.7910/DVN/ARKOTI")

dta <- get_dataframe_by_name(
  filename = "02. ET_data_June2017.tab",
  dataset = "10.7910/DVN/RKUMXB/8KATPA",
  server = "dataverse.harvard.edu",
  original = TRUE)

read.delim(file, header = TRUE, sep = "\t")

dta <- get_dataframe_by_name(
  filename = "Estimating the effects of fertilizer use in Indian agriculture",
  dataset = "10.7910/DVN/M8FCSL",
  server = "dataverse.harvard.edu",
  original = TRUE)

nlsw <-
  get_dataframe_by_doi(
    filedoi     = "10.7910/DVN/M8FCSL",
    .f = readr::read_tsv
  )
