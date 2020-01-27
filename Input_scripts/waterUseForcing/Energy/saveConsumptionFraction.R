library(fields)

rm(list = ls())

# Input
ene.file = "Input/location_energy_global.csv"
mask.file = "Input/domain_global.nc"
cons.out = "Saves/energy_consumption_fraction.RDS"

# Load
nc = nc_open(mask.file)
mask = ncvar_get(nc, "mask")
nc_close(nc)

ene = read.csv(ene.file)

# Setup
lats = seq(-89.75, 89.75, by = 0.5)
lons = seq(-179.75, 179.75, by = 0.5)

# Calculate
with.map = array(NA, dim = c(length(lons), length(lats)))
cons.map = array(NA, dim = c(length(lons), length(lats)))
cons.frac.map = array(NA, dim = c(length(lons), length(lats)))
for(x in 1:dim(cons.map)[1]){
  for(y in 1:dim(cons.map)[2]){
    if(is.na(mask[x,y])){
      next
    }
    
    with.map[x,y] = 0
    cons.map[x,y] = 0
    cons.frac.map[x,y] = 0
  }
}

for(i in 1:nrow(ene)){
  x = which(lons == ene$Lon_cel[i])
  y = which(lats == ene$Lat_cel[i])
  
  if(is.na(mask[x,y])){
    next
  }
  
  with.map[x,y] = with.map[x,y] + ene$Withdrawal[i]
  cons.map[x,y] = cons.map[x,y] + ene$Consumption[i]
}

sel = !is.na(with.map) & with.map > 0
cons.frac.map[sel] = cons.map[sel] / with.map[sel]

# Save
saveRDS(cons.frac.map, cons.out)
