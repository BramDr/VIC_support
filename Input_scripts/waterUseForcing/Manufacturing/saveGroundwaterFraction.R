library(fields)

rm(list = ls())

# Input
gw.file = "Input/G_FRACTGW_MAN.txt"
id.file = "Input/Arc_ID_lon_lat_continentalarea.txt"
mask.file = "Input/domain_global.nc"
gw.out = "Saves/manufacturing_groundwater_fraction.RDS"

# Load
id = read.table(id.file, header = TRUE)
gw = read.table(gw.file, header = TRUE)

nc = nc_open(mask.file)
mask = ncvar_get(nc, "mask")
nc_close(nc)

# Setup
lats = seq(-89.75, 89.75, by = 0.5)
lons = seq(-179.75, 179.75, by = 0.5)

# Calculate
gw.map = array(NA, dim = c(length(lons), length(lats)))
for(i in 1:nrow(gw)){
  arc.id = gw$Arc_ID[i]
  arc.row = which(id$Arc_ID == arc.id)
  lat = id$lat[arc.row]
  lon = id$lon[arc.row]
  y = which(lats == lat)
  x = which(lons == lon)
  
  if(is.na(mask[x,y])){
    next
  }
  
  gw.map[x,y] = gw$VALUE[i]
}

for(x in 1:dim(mask)[1]){
  for(y in 1:dim(mask)[2]){
    if(is.na(mask[x,y])){
      next
    }
    
    if(is.na(gw.map[x,y])){
      gw.map[x,y] = 0
    }
  }
}
image.plot(gw.map)

# Save
saveRDS(gw.map, gw.out)
