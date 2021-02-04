rm(list = ls())
library(raster)
library(rgdal)
library(gdalUtils)
library(Rcpp)

tile.file = "../tileset_indus.txt"
landuse.dir = "../Temporal_aggregation/Saves"
mapping.dir = "./Saves"
fraction.out = "../../../../Data/Transformed/MODIS/Cv_5min_Indus.RDS"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)
nlanduse = 16
sourceCpp("./Rcpp_support.cpp")

tiles = read.table(tile.file, stringsAsFactors = F)
mapping.files = list.files(mapping.dir, pattern = "output_mapping", full.names = T, recursive = T, include.dirs = F)
landuse.files = list.files(landuse.dir, pattern = "landcover_major", full.names = T, recursive = T, include.dirs = F)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

output = array(0, dim = c(length(out.lons), length(out.lats), nlanduse))

tile = tiles[1,1]
for (tile in unique(tiles[,1])){
  print(tile)
  
  h = gsub(tile, pattern = "h", replacement = "")
  h = gsub(h, pattern = "v.*", replacement = "")
  h = as.numeric(h)
  v = gsub(tile, pattern = ".*v", replacement = "")
  v = as.numeric(v)
  
  mapping.file = grep(mapping.files, pattern = tile, value = T)
  landuse.file = grep(landuse.files, pattern = tile, value = T)
  mapping = readRDS(mapping.file)
  landuse = readRDS(landuse.file)
  
  npix_per_deg_y = floor(dim(landuse)[2]/10)
  
  output.tmp = aggregate_output_landuse_fraction(h = h, v = v,
                                                 npix_per_deg_y = npix_per_deg_y,
                                                 resolution = resolution,
                                                 mapping = mapping, 
                                                 landuse = landuse,
                                                 output_template = output)
  # plot(raster(output.tmp[,,16]))
  
  output = output + output.tmp
}

plot(raster(output[,,16]))

dir.create(dirname(fraction.out))
saveRDS(output, fraction.out)
