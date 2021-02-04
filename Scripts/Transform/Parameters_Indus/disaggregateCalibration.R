rm(list = ls())

library(ncdf4)
library(fields)

calibration.file = "../../../../Data/Primary/VIC/VIC_params_global.nc"
c.out = "../../../../Data/Transformed/Parameters/c_5min_Indus.RDS"
ws.out = "../../../../Data/Transformed/Parameters/Ws_5min_Indus.RDS"
ds.out = "../../../../Data/Transformed/Parameters/Ds_5min_Indus.RDS"
dsmax.out = "../../../../Data/Transformed/Parameters/Dsmax_5min_Indus.RDS"
depth.out = "../../../../Data/Transformed/Parameters/depth_5min_Indus.RDS"
infilt.out = "../../../../Data/Transformed/Parameters/infilt_5min_Indus.RDS"

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
nc = nc_open(calibration.file)
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
nc = nc_open(calibration.file)
c = ncvar_get(nc, "c")
ws = ncvar_get(nc, "Ws")
ds = ncvar_get(nc, "Ds")
dsmax = ncvar_get(nc, "Dsmax")
depth = ncvar_get(nc, "depth", start = c(1,1,2), count = c(-1,-1,1))
infilt = ncvar_get(nc, "infilt")
nc_close(nc)

c.map = map.data(mapping, c)
ws.map = map.data(mapping, ws)
ds.map = map.data(mapping, ds)
dsmax.map = map.data(mapping, dsmax)
depth.map = map.data(mapping, depth)
infilt.map = map.data(mapping, infilt)

dir.create(dirname(c.out))
dir.create(dirname(ws.out))
dir.create(dirname(ds.out))
dir.create(dirname(dsmax.out))
dir.create(dirname(depth.out))
dir.create(dirname(infilt.out))
saveRDS(c.map, c.out)
saveRDS(ws.map, ws.out)
saveRDS(ds.map, ds.out)
saveRDS(dsmax.map, dsmax.out)
saveRDS(depth.map, depth.out)
saveRDS(infilt.map, infilt.out)
