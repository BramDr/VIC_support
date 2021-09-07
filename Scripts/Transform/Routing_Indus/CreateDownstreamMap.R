library(fields)
rm(list = ls())

# Input
downstream.dahri.file <- "../../../Data/Transformed/Routing/downstream_Dahri_5min_Indus.RDS"
downstream.hydro.file <- "../../../Data/Transformed/Routing/downstream_Hydro1k_5min_Indus.RDS"
#downstream.hydro.file <- "../../../Data/Transformed/Routing/downstream_LPJmL_5min_Indus.RDS"
accumulation.dahri.file <- "../../../Data/Transformed/Routing/accumulation_Dahri_5min_Indus.RDS"
accumulation.hydro.file <- "../../../Data/Transformed/Routing/accumulation_Hydro1k_5min_Indus.RDS"
#accumulation.hydro.file <- "../../../Data/Transformed/Routing/accumulation_LPJmL_5min_Indus.RDS"
downstream.out <- "../../../Data/Transformed/Routing/downstream_5min_Indus.RDS"

downstream.dahri = readRDS(downstream.dahri.file)
downstream.hydro = readRDS(downstream.hydro.file)
accumulation.dahri = readRDS(accumulation.dahri.file)
accumulation.hydro = readRDS(accumulation.hydro.file)

# Calculate
## Overwrite Hydro1k with Dahri downstream
downstream.overwrite = downstream.hydro
for(x in 1:dim(downstream.hydro)[1]){
  for(y in 1:dim(downstream.hydro)[2]){
    if(is.na(downstream.dahri[x,y,1])){
      next
    }
    downstream.overwrite[x,y,] = downstream.dahri[x,y,]
  } 
}

## Find Hydro1k areas that are outside of the Dahri domain but flow into the Dari domain
omit.map = array(0, dim = dim(downstream.overwrite)[1:2])
for(x in 1:dim(downstream.overwrite)[1]){
  for(y in 1:dim(downstream.overwrite)[2]){
    if(is.na(downstream.overwrite[x,y,1])){
      next
    }
    if(!is.na(downstream.dahri[x,y,1])){
      next
    }
    
    cur <- c(x, y)
    nex <- downstream.overwrite[x, y, ]
    direction = nex - cur
    
    while (TRUE) {
      if(!is.na(downstream.dahri[cur[1], cur[2], 1])){
        omit.map[x,y] = 1
        break
      }
      
      if (cur[1] == nex[1] && cur[2] == nex[2]) {
        break
      }
      
      cur <- nex
      nex <- downstream.overwrite[cur[1], cur[2], ]
    }
  } 
}
image.plot(omit.map)

## Flood-fill areas
flood.map = omit.map
fill.count = 2
for(x in 1:dim(flood.map)[1]){
  for(y in 1:dim(flood.map)[2]){
    if(flood.map[x,y]!=1){
      next
    }
    
    surround.idxs = data.frame(x = x, y = y)
    flood.map[x, y] = fill.count
    
    while(nrow(surround.idxs) > 0){
      new.surround.idxs = data.frame(x = numeric(), y = numeric())
      for(i in 1:nrow(surround.idxs)){
        sx = surround.idxs$x[i]
        sy = surround.idxs$y[i]
        
        for(nxi in c(-1,0,1)){
          for(nyi in c(-1,0,1)){
            if(nxi == 0 && nyi == 0){
              next
            }
            nx = sx + nxi
            ny = sy + nyi
            if(nx < 1 || nx > dim(flood.map)[1] || 
               ny < 1 || ny > dim(flood.map)[2]){
              next
            }
            
            if(flood.map[nx, ny] != 1){
              next
            }
            
            new.surround.idxs[nrow(new.surround.idxs) + 1,] = c(nx, ny)
            flood.map[nx, ny] = fill.count
          }
        }
      }
      surround.idxs = new.surround.idxs
    }
    
    fill.count = fill.count + 1
  } 
}
image.plot(flood.map)
table.flood = table(flood.map)

## Make all flood areas that are larger than 15 cells outflow points
downstream.avoid = downstream.overwrite
change.map = array(0, dim = dim(flood.map))
for(x in 1:dim(flood.map)[1]){
  for(y in 1:dim(flood.map)[2]){
    if(flood.map[x,y] <= 1){
      next
    }
    
    flood.count = table.flood[names(table.flood) == flood.map[x,y]]
    if(flood.count > 15){
      downstream.avoid[x,y,] = c(x,y)
      change.map[x,y] = 1
    }
  } 
}
image.plot(change.map)

## Connect Dahri outflow points to best matching (largest accumulation) neighbour of Hydro1k
connect.map = array(0, dim = dim(downstream.dahri)[1:2])
downstream.reconnect = downstream.avoid
for(x in 1:dim(downstream.avoid)[1]){
  for(y in 1:dim(downstream.avoid)[2]){
    if(is.na(downstream.dahri[x,y,1])){
      next
    }
    if(downstream.dahri[x,y,1] != x || downstream.dahri[x,y,2] != y){
      next
    }
    
    connect.map[x,y] = 1
    
    max.acc = 0
    max.x = x
    max.y = y
    
    for(nxi in c(-1,0,1)){
      for(nyi in c(-1,0,1)){
        if(nxi == 0 && nyi == 0){
          next
        }
        nx = x + nxi
        ny = y + nyi
        if(nx < 1 || nx > dim(downstream.avoid)[1] || 
           ny < 1 || ny > dim(downstream.avoid)[2]){
          next
        }
        
        if(!is.na(downstream.dahri[nx, ny, 1])){
          next
        }
        
        new.acc = accumulation.hydro[nx, ny]
        if(new.acc > max.acc){
          max.x = nx
          max.y = ny
          max.acc = new.acc
        }
      }
    }
    print(paste0(x, " : ", y, " -> ", max.x, " : ", max.y))
    
    downstream.reconnect[x,y,] = c(max.x, max.y)
  }
}
image.plot(connect.map)

dir.create(dirname(downstream.out))
saveRDS(downstream.reconnect, downstream.out)
