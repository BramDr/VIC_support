rm(list = ls())
library(raster)

in.files = c("./Saves/cut_n00e060.tif",
             "./Saves/cut_n30e060.tif")
out.file = "../../../Data/Transformed/DEM/DEM_30m_Indus.tif"
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

in.file = in.files[1]
for(in.file in in.files){
  dem <- raster(in.file)
  
  if(!exists("dem.final")){
    dem.final = extend(dem, extent.out)
    plot(dem.final)
    next
  }
  
  dem.final = merge(dem, dem.final)
  plot(dem.final)
}

dir.create(dirname(out.file))
writeRaster(dem.final, out.file, overwrite = T)
