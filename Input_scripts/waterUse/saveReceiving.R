library(ncdf4)
library(fields)
rm(list = ls())

# Input
count.file = "Saves/count.RDS"
basin.file = "Saves/basin.RDS"
delta.file = "Input/delta_map_global.RDS"
mask.file = "Input/domain_global.nc"
receiving.id.out = "Saves/receiving_id.RDS"
nreceiving.out = "Saves/Nreceiving.RDS"
receiving.out = "Saves/receiving.RDS"

# Load
nc = nc_open(mask.file)
mask = ncvar_get(nc, "mask")
nc_close(nc)

count = readRDS(count.file)
basin = readRDS(basin.file)
delta = readRDS(delta.file)

# Calculate
## Get the basin associated with the delta
delta.basin = data.frame(delta = unique(na.omit(c(delta))), basin = NA, count = 0, x = NA, y = NA)
for(x in 1:dim(delta)[1]){
  for(y in 1:dim(delta)[2]){
    if(is.na(delta[x,y])){
      next
    }
    
    row = which(delta.basin$delta == delta[x,y])
    if(count[x,y] > delta.basin$count[row]){
      delta.basin$basin[row] = basin[x,y]
      delta.basin$count[row] = count[x,y]
      delta.basin$x[row] = x
      delta.basin$y[row] = y
    }
  }
}

## Get the basin outflow point associated with the delta
for(x in 1:dim(basin)[1]){
  for(y in 1:dim(basin)[2]){
    if(is.na(basin[x,y])){
      next
    }
    if(!basin[x,y] %in% delta.basin$basin){
      next
    }
    
    row = which(delta.basin$basin == basin[x,y])
    if(count[x,y] > delta.basin$count[row]){
      delta.basin$count[row] = count[x,y]
      delta.basin$x[row] = x
      delta.basin$y[row] = y
    }
  }
}

## Give cells a receiving id
receiving.id = mask * 0
id.counter = 1
for (x in 1:dim(receiving.id)[1]) {
  for (y in 1:dim(receiving.id)[2]) {
    if (is.na(mask[x, y])) {
      next
    }
    
    receiving.id[x, y] = id.counter
    id.counter = id.counter + 1
  }
}

Nreceiving = mask * 0
for (x in 1:dim(delta)[1]) {
  for (y in 1:dim(delta)[2]) {
    if (is.na(delta[x, y])) {
      next
    }
    
    row.d = which(delta.basin$delta == delta[x,y])
    x.d = delta.basin$x[row.d]
    y.d = delta.basin$y[row.d]
    count.d = delta.basin$count[row.d]
    
    ## Remove cells in the main stem
    if(count.d - 50 <= count[x,y]){
      next
    }
    
    Nreceiving[x.d, y.d] = Nreceiving[x.d, y.d] + 1
  }
}
image.plot(Nreceiving)

receiving = array(NA, dim = c(dim(mask), max(Nreceiving, na.rm = T)))
for (x in 1:dim(delta)[1]) {
  for (y in 1:dim(delta)[2]) {
    if (is.na(delta[x, y])) {
      next
    }
    
    row.d = which(delta.basin$delta == delta[x,y])
    x.d = delta.basin$x[row.d]
    y.d = delta.basin$y[row.d]
    count.d = delta.basin$count[row.d]
    
    if(count.d - 50 <= count[x,y]){
      next
    }
    
    for(z in 1:dim(receiving)[3]){
      if(is.na(receiving[x.d, y.d, z])){
        receiving[x.d, y.d, z] = receiving.id[x,y]
        break
      }
    }
  }
}
image.plot(receiving[,,32])

# Visual check
receiving.check = array(NA, dim = dim(Nreceiving))
for (x in 1:dim(receiving)[1]) {
  for (y in 1:dim(receiving)[2]) {
    if(is.na(Nreceiving[x,y]) || Nreceiving[x,y] == 0){
      next
    }
    
    for(z in 1:Nreceiving[x,y]){
      r.id = receiving[x,y,z]
      
      for (x2 in 1:dim(receiving.id)[1]) {
        for (y2 in 1:dim(receiving.id)[2]) {
          if(is.na(receiving.id[x2,y2])){
            next
          }
          if(receiving.id[x2,y2] != r.id){
            next
          }
          
          receiving.check[x2,y2] = 1;
        }
      }
    }
  }
}
image.plot(receiving.check)
image.plot(delta)
image.plot(delta[(720 * 0.7):(720 * 0.8), (360 * 0.5):(360 * 0.6)])
image.plot(receiving.check[(720 * 0.7):(720 * 0.8), (360 * 0.5):(360 * 0.6)])
image.plot(delta[(720 * 0.3):(720 * 0.4), (360 * 0.4):(360 * 0.5)])
image.plot(receiving.check[(720 * 0.3):(720 * 0.4), (360 * 0.4):(360 * 0.5)])
image.plot(delta[(720 * 0.2):(720 * 0.3), (360 * 0.5):(360 * 0.7)])
image.plot(receiving.check[(720 * 0.2):(720 * 0.3), (360 * 0.5):(360 * 0.7)])
image.plot(delta[(720 * 0.1):(720 * 0.2), (360 * 0.8):(360 * 1)])
image.plot(receiving.check[(720 * 0.1):(720 * 0.2), (360 * 0.8):(360 * 1)])

# Save
saveRDS(receiving, receiving.out)
saveRDS(receiving.id, receiving.id.out)
saveRDS(Nreceiving, nreceiving.out)