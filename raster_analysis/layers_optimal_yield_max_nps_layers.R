y = c()
n <- c(1:nlayers(rstack))# yield stack
for(i in 1:length(max)){ # which.max value 
  z <- values(max)[i]
  for(j in 1:nlayers(rstack)){
    if(z == n[j]){
      y[i] <- names(rstack[[j]])
    }
  }
}

# capture layer names and change to data frame
nps <-
  strcapture(
    pattern = "(.*?).([[:digit:]]+).([[:digit:]]+).([[:digit:]]+)",
    y,
    proto = data.frame(
      chr = character(),
      p = integer(),
      s = integer(),
      n = integer()
    )
  )

#write the n, p, s layers
n <- p <- s <- raster(r1)
values(n) <- nps$n
values(p) <- nps$p
values(s) <- nps$s
nuitrient <- stack(n, p, s)

writeRaster(nuitrient, filename =  paste(for (i in 1:nlayers(nuitrient)) {i},"maximum", sep = "_"),bylayer = T, format = "GTiff", overwrite = T)

