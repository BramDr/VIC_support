library(raster)
library(fields)
rm(list = ls())

# Input
direction.file <- "../../../Data/Primary/Hydro1kHydroSHEDS/DRT_12th_FDR_globe.asc"
downstream.out <- "../../../Data/Transformed/Routing/downstream_5min_Indus.RDS"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

extent.out <- extent(min(out.lons) - resolution / 2, 
                     max(out.lons) + resolution / 2, 
                     min(out.lats) - resolution / 2, 
                     max(out.lats) + resolution / 2)

# Load
direction <- raster(direction.file)
direction <- crop(direction, extent.out)
direction <- as.matrix(direction)
direction <- t(direction[dim(direction)[1]:1,])
image.plot(direction)

# Setup
direction.to.index <- function(direction) {
  if(is.na(direction)){
    direction = 0
  }
  
  if(direction == 1){
    return(c(1, 0)) # east
  } else if(direction == 2){
    return(c(1, -1)) # south east
  } else if(direction == 4){
    return(c(0, -1)) # south
  } else if(direction == 8){
    return(c(-1, -1)) # south west
  } else if(direction == 16){
    return(c(-1, 0)) # west
  } else if(direction == 32){
    return(c(-1, 1)) # north west
  } else if(direction == 64){
    return(c(0, 1)) # north
  } else if(direction == 128){
    return(c(1, 1)) # north east
  } else if(direction == 0){
    return(c(0, 0)) # outflow
  } else {
    print(paste0("ERROR: direction is ", direction))
  }
}

# Calculate
downstream <- array(NA, dim = c(dim(direction), 2))
for (x in 1:dim(direction)[1]) {
  for (y in 1:dim(direction)[2]) {
    if(is.na(direction[x,y])){
      next
    }
    
    nex = c(x, y) + direction.to.index(direction[x, y])
    if(nex[1] < 1 || nex[2] < 1 || nex[1] > dim(direction)[1] || nex[2] > dim(direction)[2]){
      nex = c(x,y)
    }
    downstream[x, y, ] <- nex
  }
}
image.plot(downstream[, , 1])
image.plot(downstream[, , 2])

# Save
dir.create(dirname(downstream.out))
saveRDS(downstream, downstream.out)
