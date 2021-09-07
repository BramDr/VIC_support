library(fields)
rm(list = ls())

# Input
soil.file <- "../../../Data/Primary/Dahri2020/vic_in/soilparams.txt"
baseflow.out <- "../../../Data/Transformed/Parameters/baseflow_Dahri_5min_Indus.RDS"

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
d1.map <- array(NA, dim = c(length(out.lons), length(out.lats)))
d2.map <- array(NA, dim = c(length(out.lons), length(out.lats)))
d3.map <- array(NA, dim = c(length(out.lons), length(out.lats)))
d4.map <- array(NA, dim = c(length(out.lons), length(out.lats)))
for (i in 1:nrow(soil)) {
  x.diff = abs(out.lons - soil[i, 4])
  y.diff = abs(out.lats - soil[i, 3])
  if(min(x.diff) > resolution / 2 || min(y.diff) > resolution / 2){
    next
  }
  
  x = which.min(x.diff)
  y = which.min(y.diff)

  d1.map[x, y] <- soil[i, 6]
  d2.map[x, y] <- soil[i, 7]
  d3.map[x, y] <- soil[i, 8]
  d4.map[x, y] <- soil[i, 9]
}
image.plot(d1.map)
image.plot(d2.map)
image.plot(d3.map)
image.plot(d4.map)

# Save
baseflow.out.tmp <- gsub(x = baseflow.out, pattern = "baseflow", replacement = "baseflowD1")
dir.create(dirname(baseflow.out.tmp))
saveRDS(d1.map, baseflow.out.tmp)
baseflow.out.tmp <- gsub(x = baseflow.out, pattern = "baseflow", replacement = "baseflowD2")
dir.create(dirname(baseflow.out.tmp))
saveRDS(d2.map, baseflow.out.tmp)
baseflow.out.tmp <- gsub(x = baseflow.out, pattern = "baseflow", replacement = "baseflowD3")
dir.create(dirname(baseflow.out.tmp))
saveRDS(d3.map, baseflow.out.tmp)
baseflow.out.tmp <- gsub(x = baseflow.out, pattern = "baseflow", replacement = "baseflowD4")
dir.create(dirname(baseflow.out.tmp))
saveRDS(d4.map, baseflow.out.tmp)
