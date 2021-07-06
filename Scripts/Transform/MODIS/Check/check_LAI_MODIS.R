rm(list = ls())
library(raster)
library(rgdal)
library(gdalUtils)
library(Rcpp)

tile.file = "../tileset_indus.txt"
landuse.dir = "../Temporal_aggregation/Saves"
in.dir = "../Temporal_aggregation/Saves"
in.vars = c("LAI", "NDVI", "albedo")
mapping.dir = "./Saves"
out.dir = "./Saves"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)
nlanduse = 16
nmonths = 12
sourceCpp("./Rcpp_support.cpp")

var = "LAI"
print(var)

tiles = read.table(tile.file, stringsAsFactors = F)
mapping.files = list.files(mapping.dir, pattern = "output_mapping", full.names = T, recursive = T, include.dirs = F)
landuse.files = list.files(landuse.dir, pattern = "landcover_major", full.names = T, recursive = T, include.dirs = F)
data.sum.files = list.files(in.dir, pattern = paste0(var, "_sum"), full.names = T, recursive = T, include.dirs = F)
data.count.files = list.files(in.dir, pattern = paste0(var, "_count"), full.names = T, recursive = T, include.dirs = F)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

output.sum = array(0, dim = c(length(out.lons), length(out.lats), nlanduse, nmonths))
output.count = array(0, dim = c(length(out.lons), length(out.lats), nlanduse, nmonths))

tile = tiles[1,1]
tiles = c("h24v05", "h24v06")
tile = tiles[2]
for (tile in tiles){
  print(tile)
  
  mapping.file = grep(mapping.files, pattern = tile, value = T)
  landuse.file = grep(landuse.files, pattern = tile, value = T)
  data.sum.file = grep(data.sum.files, pattern = tile, value = T)
  data.count.file = grep(data.count.files, pattern = tile, value = T)
  
  mapping = readRDS(mapping.file)
  landuse = readRDS(landuse.file)
  data.sum = readRDS(data.sum.file)
  data.count = readRDS(data.count.file)
  
  npix_per_deg_y = floor(dim(landuse)[2]/10)
  
  output.tmp = aggregate_output_landuse_generic(mapping = mapping, 
                                                landuse = landuse,
                                                in_sum = data.sum,
                                                in_count = data.count,
                                                output_template = output.sum)
  
  output.sum = output.sum + output.tmp$sum
  output.count = output.count + output.tmp$count
  
  v = 13
  m = 1
  for(m in 1:12){
    output.avg = output.tmp$sum / output.tmp$count
    output.avg[output.tmp$count <= 5] = NA
    output.avg.r = t(output.avg[,,v,m])
    output.avg.r = output.avg.r[nrow(output.avg.r):1,]
    output.avg.r = raster(output.avg.r)
    plot(output.avg.r, zlim = c(0,4))
  }
}
output.avg = output.sum / output.count
output.avg[output.count <= 5] = NA

v = 13
m = 2
for(m in 1:12){
  output.avg.r = t(output.avg[,,v,m])
  output.avg.r = output.avg.r[nrow(output.avg.r):1,]
  output.avg.r = raster(output.avg.r)
  plot(output.avg.r > 3, zlim = c(0,4), main = paste0(m))
}
