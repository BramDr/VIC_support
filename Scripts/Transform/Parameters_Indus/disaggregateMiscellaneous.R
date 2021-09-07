rm(list = ls())

library(ncdf4)
library(fields)

vic.orig = "../../../../Data/Primary/VIC/VIC_params_global.nc"
avg.t.out = "../../../../Data/Transformed/Parameters/avgT_5min_Indus.RDS"
annual.prec.out = "../../../../Data/Transformed/Parameters/annualPrec_5min_Indus.RDS"

# Setup
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

map.data = function(mapping, data){
  out.data = array(NA, dim = dim(mapping)[1:2])
  for(x in 1:dim(out.data)[1]){
    for(y in 1:dim(out.data)[2]){
      map.x = mapping[x,y,1]
      map.y = mapping[x,y,2]
      out.data[x,y] = data[map.x, map.y]
    }
  }
  return(out.data)
}

# Calculate mapping
nc = nc_open(vic.orig)
in.lons = nc$dim$lon$vals
in.lats = nc$dim$lat$vals
nc_close(nc)

mapping = array(NA, dim = c(length(out.lons), length(out.lats), 2))
for(x in 1:length(out.lons)){
  for(y in 1:length(out.lats)){
    map.x = which.min(abs(in.lons - out.lons[x]))
    map.y = which.min(abs(in.lats - out.lats[y]))
    mapping[x,y,] = c(map.x, map.y)
  }
}

# Disaggregate
nc = nc_open(vic.orig)
avg.t = ncvar_get(nc, "avg_T")
annual.prec = ncvar_get(nc, "annual_prec")
nc_close(nc)

avg.t.map = map.data(mapping, avg.t)
annual.prec.map = map.data(mapping, annual.prec)

dir.create(dirname(avg.t.out))
dir.create(dirname(annual.prec.out))
saveRDS(avg.t.map, avg.t.out)
saveRDS(annual.prec.map, annual.prec.out)
