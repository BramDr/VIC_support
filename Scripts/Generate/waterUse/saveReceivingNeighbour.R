library(ncdf4)
library(fields)
rm(list = ls())

# Input
basin.file <- "../../../Data/Transformed/Delta/deltaBasins_30min_global.RDS"
downstream.file <- "../../../Data/Transformed/Routing/downstream_30min_global.RDS"
elev.file <- "../../../Data/Primary/VIC/VIC_params_global.nc"
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
receiving.out <- "Saves/receivingNeighbour.RDS"

# Load
nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

nc <- nc_open(elev.file)
elev <- ncvar_get(nc, "elev")
nc_close(nc)

basin <- readRDS(basin.file)
downstream <- readRDS(downstream.file)

# Setup
check.in.stream = function(xfrom, yfrom, xto, yto){
  cur <- c(xfrom, yfrom)
  nex <- downstream[cur[1], cur[2], ]
  
  goes = F
  while (TRUE) {
    if (cur[1] == xto && cur[2] == yto){
      return(T)
    }
    if (cur[1] == nex[1] && cur[2] == nex[2]) {
      return(F)
    }
    
    cur <- nex
    nex <- downstream[cur[1], cur[2], ]
  }
}

# Calculate

## Get neighbour cells
neighbour.receiving = vector(mode = "list", length = 720)
for(x in 1:dim(downstream)[1]){
  neighbour.receiving[[x]] = vector(mode = "list", length = 360)
}
for(x in 1:dim(downstream)[1]){
  print(x)
  for(y in 1:dim(downstream)[2]){
    if(is.na(downstream[x,y,1])){
      next
    }
    if(is.na(elev[x,y])){
      next
    }
    if(is.na(basin[x,y])){
      next
    }
    
    neighbours = data.frame(x = numeric(), y = numeric())
    
    for(nxo in c(-1,0,1)){
      for(nyo in c(-1,0,1)){
        nx = x + nxo
        ny = y + nyo
        
        if(nx < 1){
          nx = 1
        }
        if(nx > dim(downstream)[1]){
          nx = dim(downstream)[1]
        }
        if(ny < 1){
          ny = 1
        }
        if(ny > dim(downstream)[1]){
          ny = dim(downstream)[1]
        }
        
        ## Skip itself
        if(nx == x && ny == y){
          next
        }
        
        if(is.na(elev[nx,ny])){
          next
        }
        if(is.na(downstream[nx,ny,1])){
          next
        }
        if(is.na(basin[nx,ny])){
          next
        }
        
        ## Check elevation
        if(elev[nx,ny] >= elev[x,y] - 1){
          next
        }
        
        ## Check basin
        if(basin[nx,ny] != basin[x,y]){
          next
        }
        
        ## Check if location goes to neighbour
        if(check.in.stream(x,y,nx,ny)){
          next
        }
        
        ## Check if neighbour goes to location
        if(check.in.stream(nx,ny,x,y)){
          next
        }
        
        neighbours[nrow(neighbours) + 1,] = c(nx, ny)
      }
    }
    
    neighbours = unique(neighbours)
    
    if(nrow(neighbours) == 0){
      next
    }
    
    neighbour.receiving[[x]][[y]]$df <- neighbours
  }
}

## Nreceiving
Nreceiving <- mask * 0
for (x in 1:dim(downstream)[1]) {
  for (y in 1:dim(downstream)[2]) {
    if (length(names(neighbour.receiving[[x]][[y]])) == 0) {
      next
    }
    
    df = neighbour.receiving[[x]][[y]]$df
    
    Nreceiving[x, y] <- Nreceiving[x, y] + nrow(df)
  }
}
image.plot(Nreceiving)

# Save
dir.create(dirname(receiving.out))
saveRDS(neighbour.receiving, receiving.out)
