rm(list = ls())
library(raster)
library(rgdal)
library(gdalUtils)
library(Rcpp)

tile.file = "../tileset_indus.txt"
landuse.dir = "../Temporal_aggregation/Saves"
in.dir = "../Temporal_aggregation/Saves"
mapping.dir = "./Saves"
out.dir = "./Saves"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)
nlanduse = 16
nmonths = 12
sourceCpp("./Rcpp_support.cpp")
  
tiles = read.table(tile.file, stringsAsFactors = F)
mapping.files = list.files(mapping.dir, pattern = "output_mapping", full.names = T, recursive = T, include.dirs = F)
landuse.files = list.files(landuse.dir, pattern = "landcover_major", full.names = T, recursive = T, include.dirs = F)
data.mean.files = list.files(in.dir, pattern = paste0("vegetationcover"), full.names = T, recursive = T, include.dirs = F)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

output.sum = array(0, dim = c(length(out.lons), length(out.lats), nlanduse, 1))
output.count = array(0, dim = c(length(out.lons), length(out.lats), nlanduse, 1))

tile = tiles[1,1]
for (tile in unique(tiles[,1])){
  print(tile)
  
  mapping.file = grep(mapping.files, pattern = tile, value = T)
  landuse.file = grep(landuse.files, pattern = tile, value = T)
  data.mean.file = grep(data.mean.files, pattern = tile, value = T)
  
  mapping = readRDS(mapping.file)
  landuse = readRDS(landuse.file)
  data.mean = readRDS(data.mean.file)
  
  data.mean = aggregate(x = raster(data.mean), fact = 2, fun = mean, na.rm = T)
  data.mean = array(data.mean, dim = dim(data.mean))
  
  data.count = data.mean * 0 + 1
  data.count[is.na(data.count)] = 0
  data.mean[is.na(data.mean)] = -1
  
  # data.avg = data.sum / data.count
  # data.avg[data.count == 0] = NA
  # data.avg = apply(X = data.avg, MARGIN = c(1,2), FUN = mean)
  # plot(raster(landuse))
  # plot(raster(data.avg))
  
  npix_per_deg_y = floor(dim(landuse)[2]/10)
  
  output.tmp = aggregate_output_landuse_generic(mapping = mapping, 
                                                landuse = landuse,
                                                in_sum = data.mean,
                                                in_count = data.count,
                                                output_template = output.sum)
  
  # output.tmp.avg = output.tmp$sum / output.tmp$count
  # output.tmp.avg[output.tmp$count == 0 | output.tmp$sum < 0] = NA
  # output.tmp.avg = output.tmp.avg[,,,1]
  # plot(raster(output.tmp.avg[,,13,1]))
  
  output.sum = output.sum + output.tmp$sum
  output.count = output.count + output.tmp$count
}
output.avg = output.sum / output.count
output.avg[output.count <= 5 | output.sum < 0] = NA
output.avg = output.avg[,,,1]
plot(raster(output.avg.mean[,,13]))

out.file = paste0(out.dir, "/", "vegetationcover", "_5min_Indus.RDS")
dir.create(dirname(out.file))
saveRDS(output.avg, out.file)
