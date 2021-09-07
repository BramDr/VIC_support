rm(list = ls())
library(fields)

height.5min.file = "./Saves/height_5min_Indus.RDS"
height.15min.file = "./Saves/height_15min_Indus.RDS"
height.30min.file = "./Saves/height_30min_Indus.RDS"
adjustment.15min.out = "./Saves/adjustment_15min_Indus.RDS"
adjustment.30min.out = "./Saves/adjustment_30min_Indus.RDS"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

alt.15min.resolution = 1 / 4
alt.15min.global.lats = seq(from = -90 + alt.15min.resolution / 2, to = 90 - alt.15min.resolution / 2, by = alt.15min.resolution)
alt.15min.lats = alt.15min.global.lats[alt.15min.global.lats <= out.lat.range["max"] & alt.15min.global.lats >= out.lat.range["min"]]
alt.15min.lats = rep(alt.15min.lats, each = 3)

alt.30min.resolution = 1 / 2
alt.30min.global.lats = seq(from = -90 + alt.30min.resolution / 2, to = 90 - alt.30min.resolution / 2, by = alt.30min.resolution)
alt.30min.lats = alt.30min.global.lats[alt.30min.global.lats <= out.lat.range["max"] & alt.30min.global.lats >= out.lat.range["min"]]
alt.30min.lats = rep(alt.30min.lats, each = 6)

elevation.factor.1 = c(-0.00696, -0.00700, -0.00707, -0.00748, -0.00796, -0.00854, -0.00739, -0.00699, -0.00749, -0.00778, -0.00719, -0.00693)
latitude.factor.1 = c(-0.361, -0.856, -0.887, -0.927, -0.691, 0.552, 1.080, 0.869, 0.595, 0.080, -0.606, -0.600)
elevation.factor.2 = c(-0.00508, -0.00549, -0.00548, -0.00558, -0.00591, -0.00604, -0.00583, -0.00578, -0.00533, -0.00513, -0.00519, -0.00523)
latitude.factor.2 = c(-0.614, -0.781, -1.050, -1.250, -1.060, -0.854, -0.273, -0.333, -1.080, -1.260, -0.941, -0.460)

# Load
height.5min = readRDS(height.5min.file)
height.15min = readRDS(height.15min.file)
height.30min = readRDS(height.30min.file)

# Calculate
height.15min.diff = array(NA, dim = dim(height.5min))
height.30min.diff = array(NA, dim = dim(height.5min))
latitude.15min.diff = array(NA, dim = dim(height.5min))
latitude.30min.diff = array(NA, dim = dim(height.5min))
adjustment.15min = array(0, dim = c(dim(height.5min), 12))
adjustment.30min = array(0, dim = c(dim(height.5min), 12))
for(x in 1:dim(height.5min)[1]){
  for(y in 1:dim(height.5min)[2]){
    if(is.na(height.5min[x,y])){
      next
    }
    
    height.15min.diff[x,y] = height.5min[x,y] - height.15min[x,y]
    height.30min.diff[x,y] = height.5min[x,y] - height.30min[x,y]
    latitude.15min.diff[x,y] = out.lats[y] - alt.15min.lats[y]
    latitude.30min.diff[x,y] = out.lats[y] - alt.30min.lats[y]
      
    adjustment.15min[x,y,] = 
      ((elevation.factor.1 * height.15min.diff[x,y] + latitude.factor.1 * latitude.15min.diff[x,y]) +
       (elevation.factor.1 * height.15min.diff[x,y] + latitude.factor.1 * latitude.15min.diff[x,y])) / 2
    adjustment.30min[x,y,] = 
      ((elevation.factor.1 * height.30min.diff[x,y] + latitude.factor.1 * latitude.30min.diff[x,y]) +
       (elevation.factor.1 * height.30min.diff[x,y] + latitude.factor.1 * latitude.30min.diff[x,y])) / 2
  }
}
image.plot(height.15min.diff)
image.plot(height.30min.diff)
image.plot(latitude.15min.diff)
image.plot(latitude.30min.diff)
image.plot(adjustment.15min[,,1])
image.plot(adjustment.30min[,,1])

# Save
dir.create(dirname(adjustment.15min.out))
saveRDS(adjustment.15min, adjustment.15min.out)
dir.create(dirname(adjustment.30min.out))
saveRDS(adjustment.30min, adjustment.30min.out)
