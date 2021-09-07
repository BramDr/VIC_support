library(fields)
rm(list = ls())

# Input
soil.file <- "../../../Data/Primary/Dahri2020/vic_in/soilparams.txt"
infilt.out <- "../../../Data/Transformed/Parameters/infilt_Dahri_5min_Indus.RDS"

# Load
soil <- read.table(file = soil.file)

# Setup
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Calculate
infilt.map <- array(NA, dim = c(length(out.lons), length(out.lats)))
for (i in 1:nrow(soil)) {
  x.diff = abs(out.lons - soil[i, 4])
  y.diff = abs(out.lats - soil[i, 3])
  if(min(x.diff) > resolution / 2 || min(y.diff) > resolution / 2){
    next
  }
  
  x = which.min(x.diff)
  y = which.min(y.diff)

  infilt.map[x, y] <- soil[i, 5]
}
image.plot(infilt.map)

# Save
dir.create(dirname(infilt.out))
saveRDS(infilt.map, infilt.out)
