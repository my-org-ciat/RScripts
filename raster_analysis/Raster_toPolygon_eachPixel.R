# changing a raster to a polygon with individual pixels values
require(rgeos)
require(raster)
require(rgdal)
require(sp)
r <- raster("optimal_yield_90.tif")
eth_woreda <- readOGR(dsn = "C:\\Users\\ATilaye\\Documents\\01My_Docs\\boundary\\Boundaryfiles_recent\\Boundaryfiles", layer = "ethio_woreda")
womberma <- eth_woreda[which(eth_woreda$ADM3_EN=="Wemberma"),]
r_womberma <- crop(r, womberma)
womberma_poly2 <-
  rasterToPolygons(
    r_womberma,
    n = 4,#nodes
    na.rm = TRUE,
    digits = 8, #lat,lon digits
    dissolve = F #not simplify polygons
  )
writeOGR(womberma_poly2, dsn = ".", layer = "wonberma3", driver = "ESRI Shapefile")
