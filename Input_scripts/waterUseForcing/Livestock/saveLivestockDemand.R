library(fields)
library(ncdf4)
rm(list = ls())

# Input
liv.dir = "Input/"
meteo.dir = "Input/"
mask.file = "Input/domain_global.nc"
area.file = "Input/domain_global.nc"
liv.out = "Saves/livestock_demand_global.RDS"
years = 1979:2016

# Load
liv.files = list.files(liv.dir, pattern = ".csv", full.names = T)
meteo.files = list.files(meteo.dir, pattern = ".nc", full.names = T)

nc = nc_open(mask.file)
mask = ncvar_get(nc, "mask")
nc_close(nc)

nc = nc_open(area.file)
area = ncvar_get(nc, "area")
nc_close(nc)

# Setup
lats = seq(-89.75, 89.75, by = 0.5)
lons = seq(-179.75, 179.75, by = 0.5)

maps = list()
for(liv.file in liv.files){
  print(paste0("File ", basename(liv.file)))
  
  liv = read.csv(liv.file)
  
  high.map = array(NA, dim = c(length(lons), length(lats)))
  low.map = array(NA, dim = c(length(lons), length(lats)))
  drink.map = array(NA, dim = c(length(lons), length(lats)))
  service.map = array(NA, dim = c(length(lons), length(lats)))
  
  for(i in 1:nrow(liv)){
    x = which(lons == liv$lon[i])
    y = which(lats == liv$lat[i])
    
    high.map[x,y] = liv$scaleHigh[i]
    low.map[x,y] = liv$scaleLow[i]
    drink.map[x,y] = liv$drink[i]
    service.map[x,y] = liv$servicing[i]
  }
  
  liv.maps = list(high = high.map, low = low.map, drink = drink.map, service = service.map)
  maps[[length(maps) + 1]] = liv.maps
}

# Calculate
liv.dem = array(0, dim = c(length(lons), length(lats),length(years) * 12))
for(z in 1:length(years)){
  year = years[z]
  print(paste0("Working on year ",year))
  
  meteo.file = grep(meteo.files, pattern = year, value = T)
  
  nc = nc_open(meteo.file)
  tair = ncvar_get(nc, "Tair")
  nc_close(nc)
  
  tair = tair - 273.15
  tair[tair < 15] = 15
  tair[tair > 35] = 35
  
  for(l in 1:length(liv.files)){
    print(paste0("File ", basename(liv.files[l])))
    
    liv.maps = maps[[l]]
    
    drink.map = drink.map = array(NA, dim = c(length(lons), length(lats)))
    for(m in 1:12){
      tair.m = tair[,,m]
      
      sel = !is.na(tair.m) & tair.m >= 25
      drink.map[sel] = liv.maps$drink[sel] + liv.maps$drink[sel] * liv.maps$high[sel] * (tair.m[sel] - 25)
      
      sel = !is.na(tair.m) & tair.m < 25
      drink.map[sel] = liv.maps$drink[sel] + liv.maps$drink[sel] * liv.maps$low[sel] * (tair.m[sel] - 25)
      
      liv.dem[,,(z - 1) * 12 + m] = liv.dem[,,(z - 1) * 12 + m] + drink.map + liv.maps$service
      
    }
  }
}

liv.dem.mm = liv.dem
for(z in 1:dim(liv.dem.mm)[3]){
  liv.dem.mm[,,z] = liv.dem.mm[,,z] / area * 1e3
}
for(x in 1:dim(mask)[1]){
  for(y in 1:dim(mask)[2]){
    if(is.na(mask[x,y]) || mask[x,y] == 0){
      liv.dem.mm[x,y,] = NA
      next
    }
    
    if(is.na(liv.dem.mm[x,y,1])){
      liv.dem.mm[x,y,] = 0
    }
  }
}

# Save
saveRDS(liv.dem.mm, liv.out)
