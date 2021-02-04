library(fields)
rm(list = ls())

# Input
veg.file <- "../../../Data/Primary/Dahri2020/vic_in/vegelib.txt"
veg.out <- "../../../Data/Transformed/Parameters/vegetation_Dahri_5min_Indus.RDS"

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

# Calculate
ovrstry.map <- array(NA, dim = c(length(out.lons), length(out.lats), nclasses))
rarc.map <- array(NA, dim = c(length(out.lons), length(out.lats), nclasses))
rmin.map <- array(NA, dim = c(length(out.lons), length(out.lats), nclasses))
windh.map <- array(NA, dim = c(length(out.lons), length(out.lats), nclasses))
rgl.map <- array(NA, dim = c(length(out.lons), length(out.lats), nclasses))
solatn.map <- array(NA, dim = c(length(out.lons), length(out.lats), nclasses))
wndatn.map <- array(NA, dim = c(length(out.lons), length(out.lats), nclasses))
trunk.map <- array(NA, dim = c(length(out.lons), length(out.lats), nclasses))
i = 3
for (i in 1:length(veg.text)) {
  veg.line <- veg.text[i]
  if (substr(veg.line, 1, 1) == "#") {
    next
  }

  veg.fields <- strsplit(x = veg.line, split = "\t")[[1]]
  veg.fields <- as.numeric(veg.fields)

  class <- veg.fields[1]
  ovrstry.map[, , class] <- veg.fields[2]
  rarc.map[, , class] <- veg.fields[3]
  rmin.map[, , class] <- veg.fields[4]
  windh.map[, , class] <- veg.fields[53]
  rgl.map[, , class] <- veg.fields[54]
  solatn.map[, , class] <- veg.fields[55]
  wndatn.map[, , class] <- veg.fields[56]
  trunk.map[, , class] <- veg.fields[57]
}
plot(rarc.map[1,1,], type = "l")
plot(rmin.map[1,1,], type = "l")

# Save
veg.out.tmp <- gsub(x = veg.out, pattern = "vegetation", replacement = "vegetationOverstory")
dir.create(dirname(veg.out.tmp))
saveRDS(ovrstry.map, veg.out.tmp)
veg.out.tmp <- gsub(x = veg.out, pattern = "vegetation", replacement = "vegetationRarc")
dir.create(dirname(veg.out.tmp))
saveRDS(rarc.map, veg.out.tmp)
veg.out.tmp <- gsub(x = veg.out, pattern = "vegetation", replacement = "vegetationRmin")
dir.create(dirname(veg.out.tmp))
saveRDS(rmin.map, veg.out.tmp)
veg.out.tmp <- gsub(x = veg.out, pattern = "vegetation", replacement = "vegetationWindh")
dir.create(dirname(veg.out.tmp))
saveRDS(windh.map, veg.out.tmp)
veg.out.tmp <- gsub(x = veg.out, pattern = "vegetation", replacement = "vegetationRgl")
dir.create(dirname(veg.out.tmp))
saveRDS(rgl.map, veg.out.tmp)
veg.out.tmp <- gsub(x = veg.out, pattern = "vegetation", replacement = "vegetationSolarAttenuation")
dir.create(dirname(veg.out.tmp))
saveRDS(solatn.map, veg.out.tmp)
veg.out.tmp <- gsub(x = veg.out, pattern = "vegetation", replacement = "vegetationWindAttenuation")
dir.create(dirname(veg.out.tmp))
saveRDS(wndatn.map, veg.out.tmp)
veg.out.tmp <- gsub(x = veg.out, pattern = "vegetation", replacement = "vegetationTrunkRatio")
dir.create(dirname(veg.out.tmp))
saveRDS(trunk.map, veg.out.tmp)

