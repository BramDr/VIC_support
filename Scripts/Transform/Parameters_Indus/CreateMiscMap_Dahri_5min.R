library(fields)
rm(list = ls())

# Input
soil.file <- "../../../Data/Primary/Dahri2020/vic_in/soilparams.txt"
t.avg.out <- "../../../Data/Transformed/Parameters/avgT_Dahri_5min_Indus.RDS"
annual.prec.out <- "../../../Data/Transformed/Parameters/annualPrec_Dahri_5min_Indus.RDS"

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
t.avg.map <- array(NA, dim = c(length(out.lons), length(out.lats)))
annual.prec.map <- array(NA, dim = c(length(out.lons), length(out.lats)))
for (i in 1:nrow(soil)) {
  x.diff = abs(out.lons - soil[i, 4])
  y.diff = abs(out.lats - soil[i, 3])
  if(min(x.diff) > resolution / 2 || min(y.diff) > resolution / 2){
    next
  }
  
  x = which.min(x.diff)
  y = which.min(y.diff)

  t.avg.map[x, y] <- soil[i, 26]
  annual.prec.map[x, y] <- soil[i, 49]
}
image.plot(t.avg.map)
image.plot(annual.prec.map)

# Save
dir.create(dirname(t.avg.out))
saveRDS(t.avg.map, t.avg.out)
dir.create(dirname(annual.prec.out))
saveRDS(annual.prec.map, annual.prec.out)
