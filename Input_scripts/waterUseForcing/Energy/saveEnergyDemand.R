library(fields)
library(ncdf4)
rm(list = ls())

# Input
ene.file = "Input/location_energy_global.csv"
mask.file = "Input/domain_global.nc"
area.file = "Input/domain_global.nc"
dem.out = "Saves/energy_demand_global.RDS"
years = 1979:2016

# Load
nc = nc_open(mask.file)
mask = ncvar_get(nc, "mask")
nc_close(nc)

nc = nc_open(area.file)
area = ncvar_get(nc, "area")
nc_close(nc)

ene = read.csv(ene.file)

# Setup
lats = seq(-89.75, 89.75, by = 0.5)
lons = seq(-179.75, 179.75, by = 0.5)

# Calculate
dem.map = array(NA, dim = c(length(lons), length(lats), length(years) * 12))
for(x in 1:dim(dem.map)[1]){
  for(y in 1:dim(dem.map)[2]){
    if(is.na(mask[x,y])){
      next
    }
    
    dem.map[x,y,] = 0
  }
}

for(z in 1:length(years)){
  year = years[z]
  print(paste0("Working on year ",year))
  
  ene.sel = ene[ene$Year_operational <= year,]
  
  for(i in 1:nrow(ene.sel)){
    x = which(lons == ene.sel$Lon_cel[i])
    y = which(lats == ene.sel$Lat_cel[i])
    
    if(is.na(mask[x,y])){
      next
    }
    
    dem.map[x,y,((z - 1) * 12 + 1):(z * 12)] = dem.map[x,y, ((z - 1) * 12 + 1):(z * 12)] + (ene.sel$Withdrawal[i] / area[x,y] * 1e3)
  }
}

# Save
saveRDS(dem.map, dem.out)
