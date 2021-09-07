library(fields)
rm(list = ls())

# Input
veg.file <- "../../../Data/Primary/Dahri2020/vic_in/vegelib.txt"
albedo.out <- "../../../Data/Transformed/Parameters/albedo_Dahri_5min_Indus.RDS"
displacement.out <- "../../../Data/Transformed/Parameters/displacement_Dahri_5min_Indus.RDS"
veg_rough.out <- "../../../Data/Transformed/Parameters/vegRough_Dahri_5min_Indus.RDS"

# Load
veg.text <- readLines(con = veg.file)

# Setup
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]
nclasses <- 16
nmonths <- 12

# Calculate
albedo.map <- array(NA, dim = c(length(out.lons), length(out.lats), nclasses, nmonths))
veg_rough.map <- array(NA, dim = c(length(out.lons), length(out.lats), nclasses, nmonths))
displacement.map <- array(NA, dim = c(length(out.lons), length(out.lats), nclasses, nmonths))
i = 4
for (i in 1:length(veg.text)) {
  veg.line <- veg.text[i]
  if (substr(veg.line, 1, 1) == "#") {
    next
  }

  veg.fields <- strsplit(x = veg.line, split = "\t")[[1]]
  veg.fields <- as.numeric(veg.fields)

  class <- veg.fields[1]
  for (z in 1:nmonths) {
    albedo.map[, , class, z] <- veg.fields[17 + (z - 1)]
    veg_rough.map[, , class, z] <- veg.fields[29 + (z - 1)]
    displacement.map[, , class, z] <- veg.fields[41 + (z - 1)]
  }
}
for (i in 1:nclasses) {
  plot(albedo.map[1,1,i,], type = "l", main = i)
  plot(veg_rough.map[1,1,i,], type = "l", main = i)
  plot(displacement.map[1,1,i,], type = "l", main = i)
}

# Save
#dir.create(dirname(albedo.out))
#saveRDS(albedo.map, albedo.out)
dir.create(dirname(veg_rough.out))
saveRDS(veg_rough.map, veg_rough.out)
dir.create(dirname(displacement.out))
saveRDS(displacement.map, displacement.out)
