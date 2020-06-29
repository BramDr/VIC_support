library(fields)
rm(list = ls())

# Input
veg.file <- "../../../Data/Primary/Nijssen2001/0.5deg/world_veg_lib.txt"
veg.out <- "../../../Data/Transformed/Parameters/vegetation_Nijssen30min_30min_global.RDS"

# Load
veg.text <- readLines(con = veg.file)

# Setup
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
nclasses <- 12

# Calculate
ovrstry.map <- array(NA, dim = c(length(lons), length(lats), nclasses))
rarc.map <- array(NA, dim = c(length(lons), length(lats), nclasses))
rmin.map <- array(NA, dim = c(length(lons), length(lats), nclasses))
windh.map <- array(NA, dim = c(length(lons), length(lats), nclasses))
rgl.map <- array(NA, dim = c(length(lons), length(lats), nclasses))
solatn.map <- array(NA, dim = c(length(lons), length(lats), nclasses))
wndatn.map <- array(NA, dim = c(length(lons), length(lats), nclasses))
trunk.map <- array(NA, dim = c(length(lons), length(lats), nclasses))

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
for (i in 1:(nclasses - 1)) {
  image.plot(rarc.map[, , i], main = i)
}

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
