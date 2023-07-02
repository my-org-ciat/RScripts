library(httr);
library(jsonlite);
api_url <- "https://rest.isric.org/soilgrids/v2.0/properties/layers";
raw <- GET(api_url);
isric_json <- fromJSON(rawToChar(raw$content))

