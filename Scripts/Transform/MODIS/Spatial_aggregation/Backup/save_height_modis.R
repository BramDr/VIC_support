rm(list = ls())
library(raster)
library(Rcpp)

tile.file = "../tileset_indus.txt"
height.file = "../../../../Data/Primary/Simard2011/Simard_Pinto_3DGlobalVeg_JGR.tif"
mapping.dir = "./Saves"
height.out = "./Saves/height_500m.RDS"
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)
sourceCpp("./Rcpp_support.cpp")

tiles = read.table(tile.file, stringsAsFactors = F)
mapping.files = list.files(mapping.dir, pattern = "height_mapping", full.names = T, recursive = T, include.dirs = F)

height = raster(height.file)
height.resolution = 360 / dim(height)[2]
height = crop(height,
              extent(out.lon.range["min"], 
                     out.lon.range["max"], 
                     out.lat.range["min"], 
                     out.lat.range["max"]))
height.lons = seq(from = extent(height)[1] + height.resolution / 2, 
                  to = extent(height)[2] - height.resolution / 2, 
                  by = height.resolution)
height.lats = seq(from = extent(height)[3] + height.resolution / 2, 
                  to = extent(height)[4] - height.resolution / 2, 
                  by = height.resolution)

height = as.matrix(height)
height[height == 0] = NA
height = t(height)
# plot(raster(height))

tile = tiles[1,1]
for (tile in unique(tiles[,1])){
  print(tile)

  mapping.file = grep(mapping.files, pattern = tile, value = T)
  
  mapping = readRDS(mapping.file)
  
  output = array(0, dim = dim(mapping))
  
  height.tmp = aggregate_modis_landuse_height(mapping = mapping,
                                              height = height,
                                              output_template = output)
  # plot(raster(height.tmp))
  
  height.out.tmp = gsub(height.out, pattern = "_500m", replacement = paste0("_", tile, "_500m"))
  dir.create(dirname(height.out.tmp))
  saveRDS(height.tmp, height.out.tmp)
}
