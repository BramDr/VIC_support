library(fields)
rm(list = ls())

# Input
bin.functions.file = "../../Support/binFunctions.R"
grid.file = "../../../Data/Primary/LPJmL_Indus/Total/grid.bin"
downstream.file = "../../../Data/Primary/LPJmL_Indus/Total/drainage.bin"
downstream.out <- "../../../Data/Transformed/Routing/downstream_LPJmL_5min_Indus.RDS"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
source(bin.functions.file)
grid = loadBin(file = grid.file, 
               type = "integer", 
               size = 2,
               colnames = c("lon", "lat"))
downstream = loadBin(file = downstream.file,
                     type = "integer",
                     size = 4,
                     colnames = c("downstream", "distance"))

# setup
grid$data$x = NA
grid$data$y = NA
mapping.map = array(NA, dim = c(length(out.lons), length(out.lats)))
for(i in 1:nrow(grid$data)){
  x.diff = abs(out.lons - grid$data$lon[i])
  y.diff = abs(out.lats - grid$data$lat[i])
  if(min(x.diff) >= resolution / 2 || min(y.diff) >= resolution / 2){
    next
  }
  
  x = which.min(x.diff)
  y = which.min(y.diff)
  
  grid$data$x[i] = x
  grid$data$y[i] = y
  mapping.map[x,y] = i
}
image.plot(mapping.map)

# Calculate
downstream.map <- array(NA, dim = c(length(out.lons), length(out.lats), 2))
for(x in 1:dim(downstream.map)[1]){
  for(y in 1:dim(downstream.map)[2]){
    row = mapping.map[x,y]
    if(is.na(row)){
      next
    }
    
    drow = downstream$data$downstream[row] + 1
    if(drow > 0 && !is.na(grid$data$x[drow]) && !is.na(grid$data$y[drow])) {
      dx = grid$data$x[drow]
      dy = grid$data$y[drow]
    } else {
      dx = x
      dy = y
    }
    
    downstream.map[x,y,] = c(dx, dy)
  }
}
image.plot(downstream.map[, , 1])
image.plot(downstream.map[, , 2])

# Check
distance.map <- array(NA, dim = dim(downstream.map)[1:2])
for(x in 1:dim(downstream.map)[1]){
  for(y in 1:dim(downstream.map)[2]){
    if(is.na(downstream.map[x,y,])){
      next
    }
    distance.map[x,y] = sqrt((downstream.map[x,y,1] - x)^2 + (downstream.map[x,y,2] - y)^2)
  }
}
image.plot(distance.map)

# Save
dir.create(dirname(downstream.out))
saveRDS(downstream.map, downstream.out)
