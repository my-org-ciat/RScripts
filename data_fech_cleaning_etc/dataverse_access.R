#searching data
library(dataverse)
library(tibble) # to see dataframes in tidyverse-form
library(readxl)

Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
#searching dataset
dataverse_search("Gary King")[c("name")]
dataverse_search("ecological inference")[c("name", "type", "description")]
dataverse_search("Gary King", start = 6, per_page = 20)[c("name")]
ei <- dataverse_search(author = "Gary King", title = "Ecological Inference", type = "dataset", per_page = 20)

# fields returned
names(ei)
# names of datasets
ei$name
# Retrieving plain text data
# We will download public data files and examine them directly in R 
# using the dataverse package.
# For downloading a public dataset, no API Key is needed.

#access metadata - dataset is a collection of files
dataset <- get_dataset("doi:10.7910/DVN/ARKOTI", server = "dataverse.harvard.edu")

#show the files within the doi 
meta <- dataset[c("datasetId","versionNumber", "versionMinorNumber","lastUpdateTime","license")]
meta2 <- dataset$files[c("filename","version", "contentType", "creationDate", "filesize")]
head(meta2)

d <- get_dataset(
  "10.7910/DVN/SV37LH", 
  server = "dataverse.harvard.edu"
)

#access file from meta data list
head(d$files$filename)
d_file <- get_dataframe_by_name(
  filename = "aap_pm_database_may2014.xls",
  dataset = "10.7910/DVN/SV37LH", 
  server = "dataverse.harvard.edu",
  .f = readxl::read_xls,
  original = T)
#First, we retrieve a plain-text file like this dataset on electricity 
#consumption by Wakiyama et al. (2014). Taking the file name and dataset 
#DOI from this entry
energy <- get_dataframe_by_name(
  filename = "comprehensiveJapanEnergy.tab",
  dataset = "10.7910/DVN/ARKOTI", 
  server = "dataverse.harvard.edu")

head(energy)
str(energy)

#getdata by doi
energy2 <-
  get_dataframe_by_doi(
    filedoi     = "doi:10.7910/DVN/GDON8Q", 
    server = "dataverse.harvard.edu"
    # .f = readr::read_tsv,
    # original = T
  )

#we can read the tab delimited data as a dataframe
library(readr)

d <- get_dataframe_by_name(
  filename = "aap_pm_database_may2014.xls",
  dataset = "10.7910/DVN/SV37LH", 
  server = "dataverse.harvard.edu",
  .f = readxl::read_xls,
  original = T
  )

d <- get_dataset(
  "10.7910/DVN/SV37LH", 
  server = "dataverse.harvard.edu"
)

energy <- get_dataframe_by_name(
  filename = "comprehensiveJapanEnergy.tab",
  dataset = "10.7910/DVN/ARKOTI", 
  server = "dataverse.harvard.edu",
  .f = function(x) read.delim(x, sep = "\t"))

Sys.setenv("DATAVERSE_SERVER" = "datahub.eiar.gov.et")
Sys.setenv("DATAVERSE_KEY" = "ba503224-3a27-47a5-af80-35c19f567740")

d <- get_dataset(
  "10.20372/eiar-rdm/U4WOKJ", 
  server = "datahub.eiar.gov.et"
)

#cow dataset raises an error but we need to set an API key
cow <- get_dataframe_by_name(
  filename = "CoW Legacy Soil Profile Dataset_BENEFIT Cascape.tab",
  dataset = "10.20372/eiar-rdm/NYBPUX", 
  server = "datahub.eiar.gov.et")

#-------------------------------------------------------------------------------
# COW
#view cow metadata
dataset <- get_dataset("doi:10.20372/eiar-rdm/NYBPUX", server = "datahub.eiar.gov.et")
dataset$files[c("filename", "contentType", "creationDate", "originalFileSize")]

#or we can use this to view metadata
str(dataset_metadata("10.20372/eiar-rdm/NYBPUX", server = "datahub.eiar.gov.et"), 
    max.level = 2)

# API Key needed and rasises an error
ddi_raw <- get_file_metadata("CoW Legacy Soil Profile Dataset_BENEFIT Cascape.tab",
                             dataset = "10.20372/eiar-rdm/NYBPUX",
                             server = "datahub.eiar.gov.et")

#get dataframe by doi
cow <-
  get_dataframe_by_doi(
    filedoi     = "10.20372/eiar-rdm/NYBPUX", 
    server = "datahub.eiar.gov.et"
  )

#_______________________________________________________________________________
argentina_tab <- get_dataframe_by_name(
  filename = "alpl2013.tab",
  dataset = "10.7910/DVN/ARKOTI",
  server = "dataverse.harvard.edu")

#view metadata of the dataset
dataset <- get_dataset("doi:10.7910/DVN/ARKOTI", server = "dataverse.harvard.edu")
dataset$files[c("filename", "contentType")]

#get dataframe by doi
nlsw <-
  get_dataframe_by_doi(
    filedoi     = "10.70122/FK2/PPIAXE/MHDB0O",
    server      = "demo.dataverse.org"
  )