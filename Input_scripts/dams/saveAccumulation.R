library(ncdf4)
library(fields)
rm(list = ls())

# Input
area.file = "Input/domain_global.nc"
downstream.file = "Saves/downstream.RDS"
accumulation.out = "Saves/accumulation.RDS"

# Load
downstream = readRDS(downstream.file)

nc = nc_open(area.file)
area = ncvar_get(nc, "area")
nc_close(nc)
image.plot(area)

# Calculate
accumulation = array(NA, dim = dim(area))
for(x in 1:dim(area)[1]){
  for(y in 1:dim(area)[2]){
    if(is.na(area[x,y]) || is.na(downstream[x,y,1])){
      next
    }
    
    cur = c(x,y)
    nex = downstream[x,y,]
    while(TRUE){
      if(is.na(accumulation[cur[1],cur[2]])){
        accumulation[cur[1],cur[2]] = 0
      }
      accumulation[cur[1],cur[2]] = accumulation[cur[1],cur[2]] + area[x,y]
      
      if(cur[1] == nex[1] && cur[2] == nex[2]){
        break
      }
      
      cur = nex
      nex = downstream[cur[1],cur[2],]
    }
  }
}
image.plot(accumulation)

# Save
saveRDS(accumulation, accumulation.out)