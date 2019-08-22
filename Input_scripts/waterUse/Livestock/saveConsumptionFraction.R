library(fields)

rm(list = ls())

# Input
mask.file = "Input/domain_global.nc"
cons.out = "Saves/livestock_consumption_fraction.RDS"

# Load
nc = nc_open(mask.file)
mask = ncvar_get(nc, "mask")
nc_close(nc)

# Setup
lats = seq(-89.75, 89.75, by = 0.5)
lons = seq(-179.75, 179.75, by = 0.5)

# Calculate
cons.map = array(NA, dim = c(length(lons), length(lats)))
for(x in 1:dim(cons.map)[1]){
  for(y in 1:dim(cons.map)[2]){
    if(is.na(mask[x,y])){
      next
    }
    
    cons.map[x,y] = 1
  }
}
image.plot(cons.map)

# Save
saveRDS(cons.map, cons.out)
