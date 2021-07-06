rm(list = ls())
library(raster)
library(rgdal)
library(gdalUtils)

snowcover.dir = "../Temporal_aggregation/Saves"
snowcover.out = "../../../../Data/Transformed/MODIS/"
out.resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + out.resolution / 2, to = 180 - out.resolution / 2, by = out.resolution)
global.lats = seq(from = -90 + out.resolution / 2, to = 90 - out.resolution / 2, by = out.resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

extent.out <- extent(min(out.lons) - out.resolution / 2, 
                     max(out.lons) + out.resolution / 2, 
                     min(out.lats) - out.resolution / 2, 
                     max(out.lats) + out.resolution / 2)

in.resolution = 1 / 20
in.lon.range = c(min = 66, max = 83)
in.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + in.resolution / 2, to = 180 - in.resolution / 2, by = in.resolution)
global.lats = seq(from = -90 + in.resolution / 2, to = 90 - in.resolution / 2, by = in.resolution)
in.lons = global.lons[global.lons <= in.lon.range["max"] & global.lons >= in.lon.range["min"]]
in.lats = global.lats[global.lats <= in.lat.range["max"] & global.lats >= in.lat.range["min"]]

extent.in <- extent(min(in.lons) - in.resolution / 2, 
                     max(in.lons) + in.resolution / 2, 
                     min(in.lats) - in.resolution / 2, 
                     max(in.lats) + in.resolution / 2)

snowcover.files = list.files(snowcover.dir, pattern = "snowcover", full.names = T, recursive = T, include.dirs = F)

snowcover.file = snowcover.files[1]
for(snowcover.file in snowcover.files){
  print(basename(snowcover.file))
  
  snowcover = readRDS(snowcover.file)
  snowcover[snowcover > 100] = NA
  
  snowcover.adj = array(NA, dim = c(length(out.lons), length(out.lats), dim(snowcover)[3]))
  z = 1
  for(z in 1:dim(snowcover)[3]) {
    print(z)
    snowcover.r = snowcover[,,z]
    snowcover.r = t(snowcover.r[,ncol(snowcover.r):1])
    snowcover.r = raster(snowcover.r)
    extent(snowcover.r) = extent.in
    #plot(snowcover.r, zlim = c(0,100))
    
    snowcover.agg = raster(nrow = length(out.lats), ncol = length(out.lons),
                           xmn = extent.out[1], xmx = extent.out[2],
                           ymn = extent.out[3], ymx = extent.out[4])
    snowcover.agg = resample(snowcover.r, snowcover.agg)
    #plot(snowcover.agg, zlim = c(0,100))
    
    snowcover.m = as.matrix(snowcover.agg)
    snowcover.m = t(snowcover.m[nrow(snowcover.m):1,])
    snowcover.adj[,,z] = snowcover.m
  }
  snowcover.mean = apply(X = snowcover.adj, MARGIN = c(1,2), FUN = mean, na.rm = T)
  plot(raster(snowcover.mean))
  
  snowcover.out.tmp = paste0(snowcover.out, basename(snowcover.file))
  snowcover.out.tmp = gsub(x = snowcover.out.tmp, pattern = "_3min_", replacement = "_5min_")
  dir.create(dirname(snowcover.out.tmp))
  saveRDS(snowcover.adj, snowcover.out.tmp)
}
