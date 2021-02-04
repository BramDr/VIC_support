library(raster)
library(fields)
rm(list = ls())

# Input
bin.support.file <- "./binFunctions.R"
grid.file <- "../../../Data/Primary/LPJmL_Indus/Total/grid.bin"
drainage.file <- "../../../Data/Primary/LPJmL_Indus/Total/drainage.bin"
downstream.out <- "../../../Data/Transformed/Routing/downstream_5min_Indus.LPJmL.RDS"
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
source(bin.support.file)

grid = loadBin(grid.file, "integer", 2)
drainage = loadBin(drainage.file, "integer", 4)
direction = mapBin(grid, drainage)[[1]][[1]]
plot(direction)

# Set extent to output extent
direction = extend(direction, extent.out)
direction = crop(direction, extent.out)
plot(direction)

# Set to matrix
direction = as.matrix(t(direction))
direction = direction[,ncol(direction):1]
image.plot(direction)

# Setup
cell = 3
cell.to.index <- function(cell) {
  if(is.na(cell) || cell == -1 || cell == -9){
    return(c(NA, NA))
  }
  cell = cell + 1
  
  grid.cell = grid$matrix[cell,]
  x.diff = abs(out.lons - grid.cell[1])
  y.diff = abs(out.lats - grid.cell[2])
  if(min(x.diff) > resolution / 2 || min(y.diff) > resolution / 2){
    return(c(NA, NA))
  }
  
  x.idx = which.min(x.diff)
  y.idx = which.min(y.diff)
  return(c(x.idx, y.idx))
}

# Calculate
downstream <- array(NA, dim = c(dim(direction), 2))
x = 50
y = 50
for (x in 1:dim(direction)[1]) {
  for (y in 1:dim(direction)[2]) {
    if(is.na(direction[x,y])){
      next
    }
    
    nex = cell.to.index(direction[x,y])
    if(is.na(nex[1])){
      nex = c(x,y)
    }
    
    downstream[x,y,] = nex
  }
}
image.plot(downstream[, , 1])
image.plot(downstream[, , 2])

# Save
dir.create(dirname(downstream.out))
saveRDS(downstream, downstream.out)
