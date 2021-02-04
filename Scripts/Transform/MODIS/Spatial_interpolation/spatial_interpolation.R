rm(list = ls())
library(fields)
library(raster)

cv.file = "../../../../Data/Transformed/MODIS/Cv_5min_Indus.RDS"
in.files = c("../Temporal_interpolation/Saves/LAI_5min_Indus.RDS",
             "../Temporal_interpolation/Saves/NDVI_5min_Indus.RDS",
             "../Temporal_interpolation/Saves/albedo_5min_Indus.RDS")
in.files = c("../Temporal_interpolation/Saves/LAI_5min_Indus.RDS",
             "../Temporal_interpolation/Saves/NDVI_5min_Indus.RDS")
out.dir = "../../../../Data/Transformed/MODIS"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 37)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

cv = readRDS(cv.file)

for(in.file in in.files) {
  print(basename(in.file))
  
  data = readRDS(in.file)

  data.r = raster(data[,,1,1])
  extent(data.r) = c(min(out.lons), max(out.lons), min(out.lats), max(out.lats))
  weights = focalWeight(data.r, resolution, "Gauss")
  weights.large = focalWeight(data.r, resolution * 20, "Gauss")
  image.plot(weights)
  image.plot(weights.large)
      
  z = 13
  m = 6
  data.adj = data
  for(z in 1:dim(data)[3]){
    print(z)
    for(m in 1:dim(data)[4]){
      data.r = raster(data[,,z,m])
      extent(data.r) = c(min(out.lons), max(out.lons), min(out.lats), max(out.lats))
      sum.raster = raster(data[,,z,m] * 0 + 1)
      extent(sum.raster) = c(min(out.lons), max(out.lons), min(out.lats), max(out.lats))
      
      data.weighted = as.matrix(focal(x = data.r, w=weights, NAonly = T, pad = T, na.rm = T))
      sum.weighted = as.matrix(focal(x = sum.raster, w=weights, NAonly = T, pad = T, na.rm = T))
      
      data.avg = data.weighted / sum.weighted
      data.avg[sum.weighted == 0] = NA
      data.avg[cv[,,z] <= 0] = NA
      
      missing = cv[,,z] > 0 & is.na(data.avg)
      if(sum(missing) == 0){
        data.adj[,,z,m] = data.avg
        next
      }
      
      data.r.tmp = raster(data[,,z,m])
      extent(data.r.tmp) = c(min(out.lons), max(out.lons), min(out.lats), max(out.lats))
      sum.raster.tmp = raster(data[,,z,m] * 0 + 1)
      extent(sum.raster.tmp) = c(min(out.lons), max(out.lons), min(out.lats), max(out.lats))
      
      data.weighted.tmp = as.matrix(focal(x = data.r.tmp, w=weights.large, NAonly = T, pad = T, na.rm = T))
      sum.weighted.tmp = as.matrix(focal(x = sum.raster.tmp, w=weights.large, NAonly = T, pad = T, na.rm = T))
      
      data.avg.tmp = data.weighted.tmp / sum.weighted.tmp
      data.avg.tmp[sum.weighted.tmp == 0] = NA
      
      data.avg[missing] = data.avg.tmp[missing]
      
      missing = cv[,,z] > 0 & is.na(data.avg)
      if(sum(missing) == 0){
        data.adj[,,z,m] = data.avg
        next
      }
      
      data.avg[missing] = 0
      data.adj[,,z,m] = data.avg
    }
  }

  out.file = paste0(out.dir, "/", basename(in.file))
  dir.create(dirname(out.file))
  saveRDS(data.adj, out.file)
}
