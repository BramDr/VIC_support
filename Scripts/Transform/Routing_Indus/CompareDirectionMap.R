library(raster)
library(fields)
library(sp)
rm(list = ls())

# Input
bin.support.file <- "./binFunctions.R"
grid.file <- "../../../Data/Primary/LPJmL_Indus/Total/grid.bin"
direction1.file <- "../../../Data/Primary/Dahri2020/rout_in/fdr_uib.asc"
direction2.file <- "../../../Data/Primary/LPJmL_Indus/Total/drainage.bin"
direction3.file <- "../../../Data/Primary/Hydro1kHydroSHEDS/DRT_12th_FDR_globe.asc"
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

# Setup
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

# Dahri
direction1 <- raster(direction1.file)
direction1 <- crop(direction1, extent.out)
extent.min = extent(direction1)
direction1 <- as.matrix(direction1)
direction1 <- t(direction1[dim(direction1)[1]:1,])
image.plot(direction1)

# Wu et al HydroSHEDS Hydro1k
direction3 <- raster(direction3.file)
direction3 <- crop(direction3, extent.min)
direction3 <- as.matrix(direction3)
direction3 <- t(direction3[dim(direction3)[1]:1,])
direction3[direction3 == 1] =   3
direction3[direction3 == 2] =   4
direction3[direction3 == 4] =   5
direction3[direction3 == 8] =   6
direction3[direction3 == 16] =  7
direction3[direction3 == 32] =  8
direction3[direction3 == 64] =  1
direction3[direction3 == 128] = 2

direction3[is.na(direction1)] = NA

image.plot(direction3, zlim = c(1,8))

# LPJmL
grid = loadBin(grid.file, "integer", 2)
direction2 = loadBin(direction2.file, "integer", 4)
direction2 = mapBin(grid, direction2)[[1]][[1]]
direction2 = crop(direction2, extent.out)
direction2 = extend(direction2, extent.out)
direction2 = as.matrix(t(direction2))
direction2 = direction2[,ncol(direction2):1]
#image.plot(direction2)

direction2.adj = direction2
x = 50
y = 50
for(x in 1:dim(direction2)[1]){
  for(y in 1:dim(direction2)[2]){
    if(is.na(direction2[x,y])){
      next
    }
    
    index = cell.to.index(direction2[x,y]) - c(x,y)
    if(is.na(index)[1]){
      index = c(0,0)  
    }
    
    if(index[1] == 1){
      if(index[2] == 1){
        direction2.adj[x,y] = 2
      } else if(index[2] == 0){
        direction2.adj[x,y] = 3
      } else if(index[2] == -1){
        direction2.adj[x,y] = 4
      } 
    } else if(index[1] == 0){
      if(index[2] == 1){
        direction2.adj[x,y] = 1
      } else if(index[2] == 0){
        direction2.adj[x,y] = 0
      } else if(index[2] == -1){
        direction2.adj[x,y] = 5
      } 
    } else if(index[1] == -1){
      if(index[2] == 1){
        direction2.adj[x,y] = 8
      } else if(index[2] == 0){
        direction2.adj[x,y] = 7
      } else if(index[2] == -1){
        direction2.adj[x,y] = 6
      } 
    }
  }
}

direction2.r = direction2.adj
direction2.r = t(direction2.r[,ncol(direction2.r):1])
direction2.r = raster(direction2.r)
extent(direction2.r) = extent.out
#plot(direction2.r)
direction2.r = crop(direction2.r, extent.min)
direction2.r = extend(direction2.r, extent.min)
direction2.r = as.matrix(t(direction2.r))
direction2.r = direction2.r[,ncol(direction2.r):1]
direction2.r[is.na(direction1)] = NA
image.plot(direction2.r, zlim = c(1,8))

