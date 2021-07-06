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

var = in.vars[1]
for(var in in.vars) {
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
  for (tile in unique(tiles[,1])){
    print(tile)
    
    mapping.file = grep(mapping.files, pattern = tile, value = T)
    landuse.file = grep(landuse.files, pattern = tile, value = T)
    data.sum.file = grep(data.sum.files, pattern = tile, value = T)
    data.count.file = grep(data.count.files, pattern = tile, value = T)
    
    mapping = readRDS(mapping.file)
    landuse = readRDS(landuse.file)
    data.sum = readRDS(data.sum.file)
    data.count = readRDS(data.count.file)
    
    # Adjust NDVI scale
    if(var == "NDVI") {
      data.sum = data.sum * 1e-8
    }
    
    # data.avg = data.sum / data.count
    # data.avg[data.count == 0] = NA
    # data.avg = apply(X = data.avg, MARGIN = c(1,2), FUN = mean)
    # plot(raster(landuse))
    # plot(raster(data.avg))
    
    npix_per_deg_y = floor(dim(landuse)[2]/10)
    
    output.tmp = aggregate_output_landuse_generic(mapping = mapping, 
                                                  landuse = landuse,
                                                  in_sum = data.sum,
                                                  in_count = data.count,
                                                  output_template = output.sum)
    
    # output.tmp.avg = output.tmp$sum / output.tmp$count
    # output.tmp.avg[output.tmp$count == 0] = NA
    # output.tmp.avg = apply(X = output.tmp.avg, MARGIN = c(1,2,3), FUN = mean)
    # plot(raster(output.tmp.avg[,,3]))
    
    output.sum = output.sum + output.tmp$sum
    output.count = output.count + output.tmp$count
  }
  output.avg = output.sum / output.count
  output.avg[output.count <= 5] = NA

  output.avg.mean = apply(X = output.avg, MARGIN = c(1,2,3), FUN = mean)
  plot(raster(output.avg.mean[,,13]))
  
  # Cleanup albedo
  if(var == "albedo") {
    output.avg.adj = output.avg
    
    for(z in 1:dim(output.avg)[3]){
      data.tmp = output.avg[,,z,]
      
      data.sel = na.omit(c(data.tmp))
      quant.75 = quantile(x = data.sel, probs = 0.75)
      data.mean = mean(data.sel[data.sel < quant.75])
      data.sd = sd(data.sel[data.sel < quant.75])
      
      threshold = data.mean + 4 * data.sd
      data.tmp[data.tmp > threshold] = NA
      
      output.avg.adj[,,z,] = data.tmp
    }
    output.avg.adj.mean = apply(X = output.avg.adj, MARGIN = c(1,2,3), FUN = mean)
    plot(raster(output.avg.adj.mean[,,13]))
    
    output.avg = output.avg.adj
  }
  
  out.file = paste0(out.dir, "/", var, "_5min_Indus.RDS")
  dir.create(dirname(out.file))
  saveRDS(output.avg, out.file)
}
