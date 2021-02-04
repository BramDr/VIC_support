rm(list = ls())
library(raster)
library(rgdal)
library(gdalUtils)
library(Rcpp)
library(ncdf4)

tile.file = "../tileset_indus.txt"
in.dir = "../Temporal_aggregation/Saves"
in.vars = c("LAI", "NDVI", "albedo")
in.vars = c("LAI", "NDVI")
mapping.dir = "./Saves"
out.dir = "./Saves"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)
nmonths = 12
sourceCpp("./Rcpp_support.cpp")

for(var in in.vars) {
  tiles = read.table(tile.file, stringsAsFactors = F)
  mapping.files = list.files(mapping.dir, pattern = "output_mapping", full.names = T, recursive = T, include.dirs = F)
  data.sum.files = list.files(in.dir, pattern = paste0(var, "_sum"), full.names = T, recursive = T, include.dirs = F)
  data.count.files = list.files(in.dir, pattern = paste0(var, "_count"), full.names = T, recursive = T, include.dirs = F)

  global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
  global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
  out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
  out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

  output.sum = array(0, dim = c(length(out.lons), length(out.lats), nmonths))
  output.count = array(0, dim = c(length(out.lons), length(out.lats), nmonths))

  tile = tiles[1,1]
  for (tile in unique(tiles[,1])){
    print(tile)
    
    mapping.file = grep(mapping.files, pattern = tile, value = T)
    data.sum.file = grep(data.sum.files, pattern = tile, value = T)
    data.count.file = grep(data.count.files, pattern = tile, value = T)
    
    mapping = readRDS(mapping.file)
    data.sum = readRDS(data.sum.file)
    data.count = readRDS(data.count.file)
    
    # data.avg = data.sum / data.count
    # data.avg[data.count == 0] = NA
    # data.avg = apply(X = data.avg, MARGIN = c(1,2), FUN = mean)
    # plot(raster(landuse))
    # plot(raster(data.avg))
    
    npix_per_deg_y = floor(dim(mapping)[2]/10)
    
    output.tmp = aggregate_output_generic(mapping = mapping, 
                                                  in_sum = data.sum,
                                                  in_count = data.count,
                                                  output_template = output.sum)
    
    # output.tmp.avg = output.tmp$sum / output.tmp$count
    # output.tmp.avg[output.tmp$count == 0] = NA
    # output.tmp.avg = apply(X = output.tmp.avg, MARGIN = c(1,2), FUN = mean)
    # plot(raster(output.tmp.avg))
    
    output.sum = output.sum + output.tmp$sum
    output.count = output.count + output.tmp$count
  }
  output.avg = output.sum / output.count
  output.avg[output.count <= 5] = NA

  output.avg.mean = apply(X = output.avg, MARGIN = c(1,2), FUN = mean)
  plot(raster(output.avg.mean))
  
  # Cleanup albedo
  if(var == "albedo") {
    for(z in 1:dim(output.avg)[3]){
      data.tmp = output.avg[,,z,]
      
      data.sel = na.omit(c(data.tmp))
      quant.75 = quantile(x = data.sel, probs = 0.75)
      data.mean = mean(data.sel[data.sel < quant.75])
      data.sd = sd(data.sel[data.sel < quant.75])
      
      threshold = data.mean + 4 * data.sd
      data.tmp[data.tmp > threshold] = NA
      
      output.avg[,,z,] = data.tmp
    }
  }
  
  # Create
  dim.lon <- ncdim_def(
    name = "lon",
    units = "degrees_east",
    vals = out.lons,
    longname = "longitude of grid cell center"
  )
  dim.lat <- ncdim_def(
    name = "lat",
    units = "degrees_north",
    vals = out.lats,
    longname = "latitude of grid cell center"
  )
  dim.month <- ncdim_def(
    name = "month",
    units = "month of year",
    vals = 1:12,
    longname = "month of year"
  )

  var.data = ncvar_def(
    name = var,
    units = "-",
    dim = list(dim.lon, dim.lat, dim.month), 
    longname = var, 
    missval = -1
  )

  out.file = paste0(out.dir, "/", var, "_5min_Indus.nc")
  dir.create(dirname(out.file))
  nc = nc_create(out.file, list(var.data))
  nc_close(nc)

  # Save
  nc = nc_open(out.file, write = T)
  ncvar_put(nc, var.data, vals = output.avg)
  nc_close(nc)
}
