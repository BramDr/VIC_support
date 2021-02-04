rm(list = ls())
library(raster)

in.files = c("../../../Data/Primary/SRTM/cut_n00e060.tif",
             "../../../Data/Primary/SRTM/cut_n30e060.tif")
out.dir = "./Saves/"
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
  extent.dem = extent(dem)
  
  extent.sel = extent(max(extent.out[1], extent.dem[1]),
                      min(extent.out[2], extent.dem[2]),
                      max(extent.out[3], extent.dem[3]),
                      min(extent.out[4], extent.dem[4]))
  
  dem <- crop(dem, extent.sel)
  plot(dem)
  
  out.file = paste0(out.dir, "/", basename(in.file))
  dir.create(dirname(out.file))
  writeRaster(dem, out.file, overwrite = T)
}
