library(fields)
library(raster)
rm(list = ls())

# Input
downstream.file <- "../../../Data/Transformed/Routing/downstream_5min_Indus.RDS"
command.file = "../../../Data/Primary/LPJmL_Indus/lpj_com_b4_fullextent.asc"
inlet.file = "Saves/inlets_Indus.RDS"
id.file = "Saves/idMap.RDS"
receiving.out <- "Saves/receivingCommand.RDS"

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
inlet = readRDS(inlet.file)
id = readRDS(id.file)
downstream <- readRDS(downstream.file)

command = raster(command.file)
command = crop(command, extent.out)
command = as.matrix(command)
command = t(command[nrow(command):1,])

# Setup
check.in.stream <- function(downstream, xfrom, yfrom, xto, yto) {
  cur <- c(xfrom, yfrom)
  nex <- downstream[cur[1], cur[2], ]
  
  if(xfrom == xto && yfrom == yto){
    return(T)
  }
  
  while (TRUE) {
    if (cur[1] == xto && cur[2] == yto) {
      return(T)
    }
    if (cur[1] == nex[1] && cur[2] == nex[2]) {
      return(F)
    }
    
    cur <- nex
    nex <- downstream[cur[1], cur[2], ]
  }
  return(F)
}

# Calculate
command.ext = command
for(x in 1:dim(command)[1]){
  for(y in 1:dim(command)[2]){
    if(is.na(command[x,y])){
      next
    }
    
    for(nxi in c(-1,0,1)){
      for(nyi in c(-1,0,1)){
        nx = x + nxi
        ny = y + nyi
        if(nx < 0 || nx > dim(command)[1] || ny < 0 || ny > dim(command)[2]){
          next
        }
        if(!is.na(command[nx, ny])){
          next
        }
        command.ext[nx,ny] = command[x,y]
      }
    }
  }
}
image.plot(command)
image.plot(command.ext)

command.adj = array(NA, dim = dim(command.ext))
for(i in 1:nrow(inlet)){
  print(i)
  
  x = inlet$POINT_X_ADJ[i]
  y = inlet$POINT_Y_ADJ[i]
  command.id = inlet$LPJID[i]
  
  for(cx in 1:dim(command.ext)[1]){
    for(cy in 1:dim(command.ext)[2]){
      if(is.na(command.ext[cx,cy]) || command.ext[cx,cy] != command.id){
        next
      }
      
      if(!check.in.stream(downstream, x, y, cx, cy)){
        command.adj[cx,cy] = id[x,y]
      }
    }
  }
}
image.plot(command.ext, zlim = c(0, 20))
image.plot(command.adj)

exclude = array(NA, dim = dim(command.adj))
for(i in 1:nrow(inlet)){
  print(i)
  
  x = inlet$POINT_X_ADJ[i]
  y = inlet$POINT_Y_ADJ[i]
  outlet.id = id[x,y]
  
  cur <- c(x, y)
  nex <- downstream[cur[1], cur[2], ]
  
  while(T){
    exclude[cur[1], cur[2]] = T
    
    if (cur[1] == nex[1] && cur[2] == nex[2]) {
      break # reached end
    }
    
    cur <- nex
    nex <- downstream[cur[1], cur[2], ]
  }
}
image.plot(exclude)

include = array(NA, dim = dim(command.adj))
for(i in 1:nrow(inlet)){
  print(i)
  
  x = inlet$POINT_X_ADJ[i]
  y = inlet$POINT_Y_ADJ[i]
  outlet.id = id[x,y]
  
  cur <- c(x, y)
  nex <- downstream[cur[1], cur[2], ]
  
  while(T){
    exclude[cur[1], cur[2]] = T
    
    if (cur[1] == nex[1] && cur[2] == nex[2]) {
      break # reached end
    }
    
    in.area = F
    for(nxi in c(-1,0,1)){
      for(nyi in c(-1,0,1)){
        nx = cur[1] + nxi
        ny = cur[2] + nyi
        if(nx < 1 || ny < 1 || nx > dim(command.adj)[1] || ny > dim(command.adj)[2]){
          next
        }
        if(abs(nxi) + abs(nyi) == 2){
          next # no diagonals
        }
        if(is.na(command.adj[nx,ny])){
          next
        }
        if(command.adj[nx,ny] == outlet.id){
          in.area = T
        }
        if(in.area){
          break
        }
      }
      if(in.area){
        break
      }
    }
    if(in.area){
      include[cur[1], cur[2]] = outlet.id
    }
    
    cur <- nex
    nex <- downstream[cur[1], cur[2], ]
  }
}
image.plot(include)

command.ids = unique(na.omit(c(command.adj)))
command.fill = command.adj
processed = array(NA, dim = dim(command.adj))
for(i in 1:nrow(inlet)){
  print(i)
  
  x = inlet$POINT_X_ADJ[i]
  y = inlet$POINT_Y_ADJ[i]
  outlet.id = id[x,y]
  
  cur <- c(x, y)
  nex <- downstream[cur[1], cur[2], ]
  
  while(T){
    if(is.na(include[cur[1],cur[2]]) || include[cur[1], cur[2]] != outlet.id){
      cur <- nex
      nex <- downstream[cur[1], cur[2], ]
      if (cur[1] == nex[1] && cur[2] == nex[2]) {
        break # reached end
      }
      next # end of area
    }
    
    if (cur[1] == nex[1] && cur[2] == nex[2]) {
      break # reached end
    }
    
    nis = c(-2,-1,0,1,2)
    #nis = c(-1,0,1)
    for(nxi in nis){
      for(nyi in nis){
        nx = cur[1] + nxi
        ny = cur[2] + nyi
        if(nx < 1 || ny < 1 || nx > dim(command.adj)[1] || ny > dim(command.adj)[2]){
          next
        }
        if(!is.na(command.adj[nx,ny])){
          next
        }
        processed[nx, ny] = T
        if(!is.na(exclude[nx,ny])){
          next
        }
        if(check.in.stream(downstream, nx, ny, x, y)){
          next
        }
        command.fill[nx,ny] = outlet.id
      }
    }
    
    cur <- nex
    nex <- downstream[cur[1], cur[2], ]
  }
}
command.fill[!is.na(exclude)] = NA
image.plot(command.adj)
image.plot(command.fill)

Nreceiving <- array(0, dim = dim(command.fill))
for(i in 1:nrow(inlet)){
  print(i)
  
  x = inlet$POINT_X_ADJ[i]
  y = inlet$POINT_Y_ADJ[i]
  outlet.id = id[x,y]
  
  Nreceiving[x,y] = sum(!is.na(command.fill) & command.fill == outlet.id)
}
image.plot(Nreceiving)

# Save
dir.create(dirname(receiving.out))
saveRDS(command.fill, receiving.out)
