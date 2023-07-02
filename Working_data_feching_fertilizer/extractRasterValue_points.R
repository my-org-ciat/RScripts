# The rasters must have the same extent, projection & resolution
# The points crs must be the same with the rasters
library(sp)
library(raster)
library(rgdal)

# reading multiple raster files
rlist <-
  list.files(
    path = ".",
    pattern = '.tif$',
    all.files = TRUE,
    full.names = FALSE
  )
rasterlist <- lapply(rlist, raster)

# stacking rasters
rstack <- stack(rasterlist)

# Reading points
points <- read.csv("D:/Final/points.csv", header = T)

# setting crs
coordinates(points) <- ~lat+long
new_crs <- crs(rstack)
crs(points) <- new_crs

# extracting values
rasval <- extract(rstack, points)
pointValues <- cbind(points, rasval)

#exporting the results
write.table(
  pointValues,
  file = "D:/poinvalues.csv",
  row.names = F,
  col.names = T,
  sep = ","
)
