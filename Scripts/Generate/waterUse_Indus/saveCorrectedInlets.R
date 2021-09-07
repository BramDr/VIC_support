library(fields)
rm(list = ls())

# Input
accumulation.file <- "../../../Data/Transformed/Routing/accumulation_5min_Indus.RDS"
accumulation.lpjml.file <- "../../../Data/Transformed/Routing/accumulation_LPJmL_5min_Indus.RDS"
basin.file <- "../../../Data/Transformed/Delta/deltaBasins_5min_Indus.RDS"
downstream.file <- "../../../Data/Transformed/Routing/downstream_5min_Indus.RDS"
dam.file <- "../dams_Indus/Saves/globalDamsMerge.csv"
inlet.file = "../../../Data/Primary/LPJmL_Indus/LPJ_command_inlets_all_replacements_b.txt"
inlet.out <- "Saves/inlets_Indus.RDS"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
accumulation <- readRDS(accumulation.file)
accumulation.lpjml <- readRDS(accumulation.lpjml.file)
basin <- readRDS(basin.file)
downstream <- readRDS(downstream.file)

dams <- read.csv(dam.file, stringsAsFactors = F)
inlet = read.table(inlet.file, sep = ",", header = T)

# Setup
table.basin = table(basin)
indus.basin.id = as.numeric(names(table.basin[order(table.basin, decreasing = T)])[1])

## Make inlet map
inlet$POINT_LON = inlet$POINT_X
inlet$POINT_LAT = inlet$POINT_Y
inlet$POINT_X = NA
inlet$POINT_Y = NA
inlet$POINT_ACC = NA
inlet$POINT_ACC2 = NA

inlet.map = array(0, dim = c(length(out.lons), length(out.lats)))
for(row in 1:nrow(inlet)){
  x.diff = abs(out.lons - inlet$POINT_LON[row])
  y.diff = abs(out.lats - inlet$POINT_LAT[row])
  
  if(min(x.diff) >= resolution / 2 || min(y.diff) >= resolution / 2){
    next
  }
  
  x = which.min(x.diff)
  y = which.min(y.diff)
  
  inlet$POINT_X[row] = x
  inlet$POINT_Y[row] = y
  inlet$POINT_ACC[row] = accumulation.lpjml[x,y] * 1e-6
  inlet$POINT_ACC2[row] = accumulation[x,y] * 1e-6
  
  inlet.map[x,y] = inlet.map[x,y] + 1
}
image.plot(inlet.map)

## Adjust inlets to custom routing
inlet.adj = inlet[complete.cases(inlet),]
inlet.adj$POINT_LON_ADJ = NA
inlet.adj$POINT_LAT_ADJ = NA
inlet.adj$POINT_X_ADJ = NA
inlet.adj$POINT_Y_ADJ = NA
inlet.adj$POINT_ACC_ADJ = NA
inlet.adj$POINT_DAM = NA
inlet.adj$POINT_CHANGED = F

i = 9
for(i in 1:nrow(inlet.adj)){
  print(i)
  
  x = inlet.adj$POINT_X[i]
  y = inlet.adj$POINT_Y[i]
  
  min.diff = abs(accumulation[x,y] * 1e-6 - inlet.adj$POINT_ACC[i])
  min.x = x
  min.y = y
  
  # Check neighboring dams
  nxo = 0
  nyo = 1
  max.neighbour = 2
  for(nxo in -max.neighbour:max.neighbour){
    for(nyo in -max.neighbour:max.neighbour){
      nx = x + nxo
      ny = y + nyo
      
      if(nx > dim(inlet.map)[1] ||
         ny > dim(inlet.map)[2] ||
         nx < 1 ||
         ny < 1){
        next
      }
      
      dam.row = which(dams$MODEL_X == nx & dams$MODEL_Y == ny)
      
      if(length(dam.row) > 0 && 
         dams$USE_IRRI[dam.row] == 1){
        
        cur.diff = abs(accumulation[nx,ny] * 1e-6 - inlet.adj$POINT_ACC[i])
        
        if (cur.diff / inlet.adj$POINT_ACC[i] < 0.25 &&
            cur.diff < min.diff){
          # Move the inlet if a neighbouring cell with correct, 
          # minimal upstream accumulation,
          # and includes an irrigation dam
          min.diff = cur.diff
          min.x = nx
          min.y = ny
        }
      }
    }
  }
  
  if(min.diff / inlet.adj$POINT_ACC[i] < 0.25){
    inlet.adj$POINT_LON_ADJ[i] = out.lons[min.x]
    inlet.adj$POINT_LAT_ADJ[i] = out.lats[min.y]
    inlet.adj$POINT_X_ADJ[i] = min.x
    inlet.adj$POINT_Y_ADJ[i] = min.y
    inlet.adj$POINT_ACC_ADJ[i] = accumulation[min.x,min.y] * 1e-6
    
    dam.row = which(dams$MODEL_X == nx & dams$MODEL_Y == ny)
    if(length(dam.row) > 0){
      inlet.adj$POINT_DAM[i] = dam.row
    }
    
    if(!(min.x == x && min.y == y)){
      print(paste0("Inlet ", i, 
                   " changed, following neighbour dam, from ", 
                   inlet.adj$POINT_ACC2[i], " to ", 
                   inlet.adj$POINT_ACC_ADJ[i], "; ",
                   inlet.adj$POINT_ACC[i]))
    } else {
      if(is.na(inlet.adj$POINT_DAM[i])){
        print(paste0("Inlet ", i, " didnt change, ",
                     "and has no dam"))
      } else {
        print(paste0("Inlet ", i, " didnt change, ",
                     "but has a dam"))
      }
    }
    
    next
  }

  for(nxo in c(-1,0,1)){
    for(nyo in c(-1,0,1)){
      nx = x + nxo
      ny = y + nyo
      
      if(nx == x && ny == y){
        next
      }
      if(nx > dim(inlet.map)[1] ||
         ny > dim(inlet.map)[2] ||
         nx < 1 ||
         ny < 1){
        next
      }
      
      cur.diff = abs(accumulation[nx,ny] * 1e-6 - inlet.adj$POINT_ACC[i])
      if(cur.diff > min.diff){
        next
      }
      
      min.diff = cur.diff
      min.x = nx
      min.y = ny
    }
  }
  
  inlet.adj$POINT_LON_ADJ[i] = out.lons[min.x]
  inlet.adj$POINT_LAT_ADJ[i] = out.lats[min.y]
  inlet.adj$POINT_X_ADJ[i] = min.x
  inlet.adj$POINT_Y_ADJ[i] = min.y
  inlet.adj$POINT_ACC_ADJ[i] = accumulation[min.x,min.y] * 1e-6
  
  if(!(min.x == x && min.y == y)){
    print(paste0("Inlet ", i, 
                 " changed, following bad inlet, from ", 
                 inlet.adj$POINT_ACC2[i], " to ", 
                 inlet.adj$POINT_ACC_ADJ[i], "; ", 
                 inlet.adj$POINT_ACC[i]))
  } else {
    print(paste0("Inlet ", i, " didnt change, ",
                 "BAD INLET"))
  }
}

## Check inlet changes
inlet.map = array(0, dim = dim(inlet.map))
inlet.adj.map = array(0, dim = dim(inlet.map))
for(i in 1:nrow(inlet.adj)) {
  x = inlet.adj$POINT_X[i]
  y = inlet.adj$POINT_Y[i]
  inlet.map[x,y] = 1
  
  x = inlet.adj$POINT_X_ADJ[i]
  y = inlet.adj$POINT_Y_ADJ[i]
  inlet.adj.map[x,y] = 1
}
image.plot(inlet.adj.map + inlet.map)

# Save
dir.create(dirname(inlet.out))
saveRDS(inlet.adj, inlet.out)

