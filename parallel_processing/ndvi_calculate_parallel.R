library(raster)
library(doParallel)  #Foreach Parallel Adaptor 
library(foreach)     #Provides foreach looping construct

#Define how many cores you want to use
UseCores <- detectCores() -1

#Register CoreCluster
cl       <- makeCluster(UseCores)
registerDoParallel(cl)

path       <- "Downloads/Stacks/"
stack_list <- list.files(path, pattern=".tif$", full.names=T)

#Use foreach loop and %dopar% command
foreach(i=1:length(stack_list)) %dopar% {
  library(raster)
  
  img  <- stack(stack_list[i])
  ndvi <- (img[[4]]-img[[3]]) / (img[[4]]+img[[3]])
  
  outname <- sub(pattern     = ".tif",
                 replacement = "_ndvi.tif", 
                 x           = stack_list[i])
  
  writeRaster(ndvi, 
              filename  = outname,
              overwrite = T)
  
}