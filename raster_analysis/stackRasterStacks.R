library(raster)
rfiles <- list.files(path = ".", pattern = ".tif$", all.files = T)
rstack <- stack(rfiles)
