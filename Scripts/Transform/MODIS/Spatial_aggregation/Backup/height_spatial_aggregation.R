rm(list = ls())
library(raster)
library(rgdal)
library(gdalUtils)
library(Rcpp)

tile.file = "../tileset_indus.txt"
landuse.dir = "../Temporal_aggregation/Saves"
height.dir = "./Saves"
mapping.dir = "./Saves"
height.out = "./Saves/height_5min_Indus.RDS"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)
nlanduse = 16
sourceCpp("./Rcpp_support.cpp")

tiles = read.table(tile.file, stringsAsFactors = F)
mapping.files = list.files(mapping.dir, pattern = "output_mapping", full.names = T, recursive = T, include.dirs = F)
landuse.files = list.files(landuse.dir, pattern = "landcover_major", full.names = T, recursive = T, include.dirs = F)
height.files = list.files(height.dir, pattern = "height_h", full.names = T, recursive = T, include.dirs = F)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

output.sum = array(0, dim = c(length(out.lons), length(out.lats), nlanduse))
output.count = array(0, dim = c(length(out.lons), length(out.lats), nlanduse))

tile = tiles[1,1]
for (tile in unique(tiles[,1])){
  print(tile)
  
  mapping.file = grep(mapping.files, pattern = tile, value = T)
  landuse.file = grep(landuse.files, pattern = tile, value = T)
  height.file = grep(height.files, pattern = tile, value = T)
  
  mapping = readRDS(mapping.file)
  landuse = readRDS(landuse.file)
  height.sum = readRDS(height.file)
  height.sum[is.na(height.sum)] = 0
  height.count = array(1, dim = dim(height.sum))
  height.count[height.sum == 0] = 0
  
  npix_per_deg_y = floor(dim(landuse)[2]/10)
  
  output.tmp = aggregate_output_landuse_generic2(mapping = mapping, 
                                                 landuse = landuse,
                                                 in_sum = height.sum,
                                                 in_count = height.count,
                                                 output_template = output.sum)
  
  # output.tmp.avg = output.tmp$sum / output.tmp$count
  # output.tmp.avg[output.tmp$count == 0] = NA
  # plot(raster(output.tmp.avg[,,7]))
  
  output.sum = output.sum + output.tmp$sum
  output.count = output.count + output.tmp$count
}
output.avg = (output.sum) / output.count
output.avg[output.count <= 5] = NA

plot(raster(output.avg[,,16]))

dir.create(dirname(height.out))
saveRDS(output.avg, height.out)
