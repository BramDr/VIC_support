rm(list = ls())
library(raster)
library(rgdal)
library(gdalUtils)
library(Rcpp)

tile.file = "../tileset_indus.txt"
mapping.out = "./Saves/output_mapping_500m.RDS"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)
sourceCpp("./Rcpp_support.cpp")

tiles = read.table(tile.file, stringsAsFactors = F)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

tile = tiles[1,1]
for (tile in unique(tiles[,1])){
  print(tile)
  
  h = gsub(tile, pattern = "h", replacement = "")
  h = gsub(h, pattern = "v.*", replacement = "")
  h = as.numeric(h)
  v = gsub(tile, pattern = ".*v", replacement = "")
  v = as.numeric(v)
  
  npix_x = npix_y = 2400
  npix_per_deg_y = floor(npix_y/10)
  
  mapping = modis_to_output_mapping(h = h, v = v,
                                    npix_per_deg_y = npix_per_deg_y,
                                    npix_x = npix_x,
                                    npix_y = npix_y,
                                    output_lats = out.lats,
                                    output_lons = out.lons,
                                    output_resolution = resolution)
  # plot(raster(t(mapping)))
  
  mapping.out.tmp = gsub(mapping.out, pattern = "_500m", replacement = paste0("_", tile, "_500m"))
  dir.create(dirname(mapping.out.tmp))
  saveRDS(mapping, mapping.out.tmp)
}
