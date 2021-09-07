library(fields)
rm(list = ls())

# Input
crop.file <- "./Saves/crop_mapping.RDS"
veg.class.out <- "./Saves/vegclass_crops_30min_global.RDS"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
crops <- readRDS(crop.file)

# Calculate
veg.class <- array(NA, dim = c(length(out.lons), length(out.lats), nrow(crops)))
for (i in 1:nrow(crops)) {
  print(crops$name[i])

  veg.class[, , i] <- as.numeric(crops$vic[i])
}
image.plot(veg.class[, , 1])

# Save
dir.create(dirname(veg.class.out))
saveRDS(veg.class, veg.class.out)
