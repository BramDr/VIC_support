library(fields)
library(raster)
rm(list = ls())

# Input
downstream.file <- "../../../Data/Transformed/Routing/downstream_5min_Indus.RDS"
accumulation.file <- "../../../Data/Transformed/Routing/accumulation_5min_Indus.RDS"
receiving.file = "Saves/receivingCommand.RDS"
id.file = "Saves/idMap.RDS"
receiving.out <- "Saves/receivingCommandExtended.RDS"
noffset = 3

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
id = readRDS(id.file)
downstream <- readRDS(downstream.file)
accumulation <- readRDS(accumulation.file)
receiving <- readRDS(receiving.file)

# Setup
get.upstream = function(x, y, accumulation, n.index = c(-1, 0, 1)){

  ux = x
  uy = y
  ua = 0
  for(nxi in n.index){
    for(nyi in n.index){
      nx = x + nxi
      ny = y + nyi
      if(nx == x && ny == y){
        next
      }
      if(nx <= 0 || ny <= 0 || nx > dim(accumulation)[1] || ny > dim(accumulation)[2]){
        next
      }
      
      if (accumulation[nx, ny] < accumulation[x,y] && accumulation[nx, ny] > ua){
        ux = nx
        uy = ny
        ua = accumulation[nx, ny]
      }
    }
  }
  
  return(c(ux, uy))
}

# Calculate
receiving.adj = array(NA, dim = c(dim(receiving), noffset + 1))
receiving.adj[,,1] = receiving

inlet = unique(na.omit(c(receiving)))[1]
for(inlet in unique(na.omit(c(receiving)))){
  print(inlet)
  
  region = !is.na(receiving) & receiving == inlet
  
  id.sel = !is.na(id) & id == inlet
  x = which(apply(X = id.sel, MARGIN = 1, FUN = sum, na.rm = T) == 1)
  y = which(apply(X = id.sel, MARGIN = 2, FUN = sum, na.rm = T) == 1)
  
  cur = c(x,y)
  nex = downstream[cur[1], cur[2],]
  for(o in 1:noffset){
    if(cur[1] == nex[1] && cur[2] == nex[2]){
      print("outflow")
      break
    }
    
    receiving.tmp = receiving.adj[,,o + 1]
    receiving.tmp[region] = id[nex[1], nex[2]]
    receiving.adj[,,o + 1] = receiving.tmp
    
    cur = nex
    nex = downstream[cur[1], cur[2],]
  }
}
for(o in 1:(noffset + 1)){
  image.plot(receiving.adj[,,o], main = o)
}

# Save
dir.create(dirname(receiving.out))
saveRDS(receiving.adj, receiving.out)
