library(fields)
library(ncdf4)
rm(list = ls())

# Input
ind.tmp = "Saves/industrial_demand_global_tmp_total.RDS"
mask.file = "Input/domain_global.nc"
ind.out = "Saves/industrial_demand_global_total.RDS"
years = 1979:2016

# Load
ind.agg = readRDS(ind.tmp)

nc = nc_open(mask.file)
mask = ncvar_get(nc, "mask")
nc_close(nc)

# Calculate
ind.int = ind.agg
for(x in 1:dim(ind.int)[1]){
  for(y in 1:dim(ind.int)[2]){
    if(is.na(mask[x,y]) || mask[x,y] == 0){
      ind.int[x,y,] = NA
      next
    }
    
    for(z in 1:dim(ind.int)[3]){
      if(!is.na(ind.agg[x,y,z])){
        next
      }
      
      ind.int[x,y,z] = 0
    }
  }
}

ind.dem = array(NA, dim = c(dim(ind.int)[1:2], dim(ind.int)[3]))
for(z in 1:dim(ind.int)[3]){
  year = years[z]
  print(paste0("Working on year ", year))
  
  for(m in 1:12){
    ind.dem[,,z] = ind.int[,,z]
  }
}

# Save
saveRDS(ind.dem, ind.out)
