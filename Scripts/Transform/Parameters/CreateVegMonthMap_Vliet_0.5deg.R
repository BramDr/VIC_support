library(fields)
rm(list = ls())

# Input
veg.file <- "../../../Data/Primary/vanVliet2016/world_veg_lib.txt"
albedo.out <- "../../../Data/Transformed/Parameters/albedo_Vliet30min_30min_global.RDS"
displacement.out <- "../../../Data/Transformed/Parameters/displacement_Vliet30min_30min_global.RDS"
veg_rough.out <- "../../../Data/Transformed/Parameters/vegRough_Vliet30min_30min_global.RDS"

# Load
veg.text <- readLines(con = veg.file)

# Setup
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
nclasses <- 12
nmonths <- 12

# Calculate
albedo.map <- array(NA, dim = c(length(lons), length(lats), nclasses, nmonths))
veg_rough.map <- array(NA, dim = c(length(lons), length(lats), nclasses, nmonths))
displacement.map <- array(NA, dim = c(length(lons), length(lats), nclasses, nmonths))

for (i in 1:length(veg.text)) {
  veg.line <- veg.text[i]
  if (substr(veg.line, 1, 1) == "#") {
    next
  }

  veg.fields <- strsplit(x = veg.line, split = "\t")[[1]]
  veg.fields <- as.numeric(veg.fields)

  class <- veg.fields[1]
  for(z in 1:nmonths){
    albedo.map[, , class, z] <- veg.fields[17 + (z - 1)]
    veg_rough.map[, , class, z] <- veg.fields[29 + (z - 1)]
    displacement.map[, , class, z] <- veg.fields[41 + (z - 1)]
  }
}
for (i in 1:(nclasses - 1)) {
  image.plot(albedo.map[, , i, 12], main = i)
  image.plot(veg_rough.map[, , i, 12], main = i)
  image.plot(displacement.map[, , i, 12], main = i)
}

# Save
dir.create(dirname(albedo.out))
saveRDS(albedo.map, albedo.out)
dir.create(dirname(veg_rough.out))
saveRDS(veg_rough.map, veg_rough.out)
dir.create(dirname(displacement.out))
saveRDS(displacement.map, displacement.out)
