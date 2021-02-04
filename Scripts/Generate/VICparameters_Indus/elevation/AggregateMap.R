rm(list = ls())
library(raster)
library(fields)

in.file = "../../../../../Data/Transformed/DEM/DEM_30m_Indus.tif"
out.elevation.file = "./Saves/elevation_5min_Indus.RDS"
out.elev.file = "./Saves/elev_5min_Indus.RDS"
out.nelev.file = "./Saves/Nelev_5min_Indus.RDS"
out.areafract.file = "./Saves/AreaFract_5min_Indus.RDS"
dem.range = 200

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

in.dem = raster(in.file)

# Setup
in.resolution = res(in.dem)
in.lons = seq(from = extent(in.dem)[1] + in.resolution[1] / 2, 
              to = extent(in.dem)[2] - in.resolution[1] / 2, 
              by = in.resolution[1])
in.lats = seq(from = extent(in.dem)[3] + in.resolution[2] / 2, 
              to = extent(in.dem)[4] - in.resolution[2] / 2, 
              by = in.resolution[2])

mapping.x = rep(NA, length(in.lons))
mapping.y = rep(NA, length(in.lats))
for(x in 1:length(in.lons)){
  map.x = which.min(abs(out.lons - in.lons[x]))
  mapping.x[x] = map.x
}
for(y in 1:length(in.lats)){
  map.y = which.min(abs(out.lats - in.lats[y]))
  mapping.y[y] = map.y
}

mapping.x.table = table(mapping.x)
mapping.y.table = table(mapping.y)
depth = max(mapping.x.table) * max(mapping.y.table)

# Calculate
in.dem = as.matrix(in.dem)
in.dem <- t(in.dem[dim(in.dem)[1]:1,])
# plot(raster(in.dem))

out.dem = array(0, dim = c(length(out.lons), length(out.lats), 50))
out.frac = array(0, dim = c(length(out.lons), length(out.lats), 50))
out.dem.mean = array(0, dim = c(length(out.lons), length(out.lats)))
out.dem.nelev = array(0, dim = c(length(out.lons), length(out.lats)))
x = 20
y = 20
for(x in 1:dim(out.dem)[1]){
  print(x)
  for(y in 1:dim(out.dem)[2]){
    sel.x = which(mapping.x == x)
    sel.y = which(mapping.y == y)
    
    sel.dem = c(in.dem[sel.x, sel.y])
    sel.dem = na.omit(sel.dem)
    if(length(sel.dem) == 0){
      next
    }
    
    sel.dem = sel.dem[order(sel.dem)]
    
    agg.breaks = seq(from = sel.dem[1], to = sel.dem[length(sel.dem)], by = dem.range)
    agg.dem = as.numeric(cut(sel.dem, c(agg.breaks, Inf), include.lowest = T))
    
    mean.dem = aggregate(x = sel.dem, by = list(agg.dem), FUN = mean)$x
    count.dem = aggregate(x = sel.dem, by = list(agg.dem), FUN = function(x){sum(!is.na(x))})$x
    frac.dem = count.dem / sum(count.dem)
    mean.dem.total = mean(sel.dem)
    
    out.dem[x,y,1:length(mean.dem)] = mean.dem
    out.frac[x,y,1:length(frac.dem)] = frac.dem
    out.dem.mean[x,y] = mean.dem.total
    out.dem.nelev[x,y] = length(mean.dem)
  }
}

image.plot(out.dem[,,1])
image.plot(out.dem[,,4])
image.plot(out.frac[,,1])
image.plot(out.frac[,,4])
image.plot(out.dem.mean)
image.plot(out.dem.nelev)

dir.create(dirname(out.elevation.file))
dir.create(dirname(out.elev.file))
dir.create(dirname(out.nelev.file))
dir.create(dirname(out.areafract.file))
saveRDS(out.dem, out.elevation.file)
saveRDS(out.dem.mean, out.elev.file)
saveRDS(out.dem.nelev, out.nelev.file)
saveRDS(out.frac, out.areafract.file)
