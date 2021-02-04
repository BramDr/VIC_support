library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
cv.file <- "../../../../Data/Transformed/MODIS/cv_5min_Indus.RDS"
frac.out = "./Saves/frac_5min_Indus.RDS"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
cv.modis = readRDS(cv.file)

# Calculate
frac = array(0, dim = c(length(out.lons), length(out.lats)))
for(i in 1:dim(cv.modis)[3]){
  if(i %in% c(1)){
    frac[,1:168] = frac[,1:168] + cv.modis[,,i]
    next
  }
}
for(z in 169:180){
  frac[,z] = frac[,168]
}
image.plot(frac, main = "frac")

dir.create(dirname(frac.out))
saveRDS(frac, frac.out)
