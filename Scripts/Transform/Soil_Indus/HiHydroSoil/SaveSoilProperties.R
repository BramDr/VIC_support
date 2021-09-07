rm(list = ls())
library(fields)
library(raster)

soil.dir = "/home/bram/Data/Primary/HiHydroSoilv2.0/"
dir.out = "./Saves/"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

extent.out <- extent(min(out.lons) - resolution / 2, 
                     max(out.lons) + resolution / 2, 
                     min(out.lats) - resolution / 2, 
                     max(out.lats) + resolution / 2)

# Load
soil.files = list.files(soil.dir, pattern = ".tif", recursive = T, full.names = T)

# Setup
transform.soil.data = function(soil.file, extent.out){
  soil.data = raster(soil.file)
  soil.data = crop(soil.data, extent.out)
  soil.data[soil.data == 0] = NA
  # plot(soil.data)
  
  soil.data.template = raster(nrow = length(out.lats), ncol = length(out.lons))
  extent(soil.data.template) = extent.out
  
  soil.data.agg = resample(soil.data, soil.data.template, na.rm = T)
  # plot(soil.data.agg)
  
  soil.data.new = as.matrix(soil.data.agg)
  soil.data.new = t(soil.data.new[nrow(soil.data.new):1,])
  soil.data.new = soil.data.new * 1e-4
  #image.plot(soil.data.new, zlim = c(0, 0.06))
  
  return(soil.data.new)
}

# Calculate & save
soil.file = soil.files[2]
for(soil.file in soil.files){
  out.file = paste0(dir.out, basename(soil.file))
  out.file = gsub(x = out.file, pattern = ".tif", replacement = ".RDS")
  if(file.exists(out.file)){
    next
  }
  
  print(basename(soil.file))
  soil.data = transform.soil.data(soil.file, extent.out)
  image.plot(soil.data)
  
  dir.create(dirname(out.file))
  saveRDS(soil.data, out.file)
}
