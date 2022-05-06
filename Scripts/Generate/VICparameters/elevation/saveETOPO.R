rm(list = ls())
library(ncdf4)
library(fields)
library(raster)

in.file = "../../../Data/Primary/ETOPO5/alwdgg.tif"
out.elevation.file = "./Saves/elevation_ETOPO5_30min_global.RDS"
out.elev.file = "./Saves/elev_ETOPO5_30min_global.RDS"
out.nelev.file = "./Saves/nelev_ETOPO5_30min_global.RDS"
out.areafract.file = "./Saves/areafract_ETOPO5_30min_global.RDS"
dem.range = 1000

# Load
dem = raster(in.file)
dem = as.matrix(dem)
dem <- t(dem[dim(dem)[1]:1,])

# Setup
out.lons = seq(from = -180 + 0.5 / 2, 
              to = 180 - 0.5 / 2, 
              by = 0.5)
out.lats = seq(from = -90 + 0.5 / 2, 
              to = 90 - 0.5 / 2, 
              by = 0.5)
in.resolution = 360 / dim(dem)[1]
in.lons = seq(from = -180 + in.resolution / 2, 
              to = 180 - in.resolution / 2, 
              by = in.resolution)
in.lats = seq(from = -90 + in.resolution / 2, 
              to = 90 - in.resolution / 2, 
              by = in.resolution)

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
    
    sel.dem = c(dem[sel.x, sel.y])
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

# Save
dir.create(dirname(out.elevation.file))
dir.create(dirname(out.elev.file))
dir.create(dirname(out.nelev.file))
dir.create(dirname(out.areafract.file))
saveRDS(out.dem, out.elevation.file)
saveRDS(out.dem.mean, out.elev.file)
saveRDS(out.dem.nelev, out.nelev.file)
saveRDS(out.frac, out.areafract.file)
