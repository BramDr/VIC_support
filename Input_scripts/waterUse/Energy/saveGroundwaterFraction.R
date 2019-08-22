library(fields)

rm(list = ls())

# Input
mask.file = "Input/domain_global.nc"
gw.out = "Saves/energy_groundwater_fraction.RDS"

# Load
nc = nc_open(mask.file)
mask = ncvar_get(nc, "mask")
nc_close(nc)

# Setup
lats = seq(-89.75, 89.75, by = 0.5)
lons = seq(-179.75, 179.75, by = 0.5)

# Calculate
gw.map = array(NA, dim = c(length(lons), length(lats)))
for(x in 1:dim(gw.map)[1]){
  for(y in 1:dim(gw.map)[2]){
    if(is.na(mask[x,y])){
      next
    }
    
    gw.map[x,y] = 0
  }
}
image.plot(gw.map)

# Save
saveRDS(gw.map, gw.out)
