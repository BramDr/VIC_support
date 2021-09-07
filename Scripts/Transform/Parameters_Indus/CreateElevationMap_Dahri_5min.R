library(fields)
rm(list = ls())

# Input
elev.file <- "../../../Data/Primary/Dahri2020/vic_in/snowband.txt"
mapping.file <- "../../../Data/Primary/Dahri2020/vic_in/soilparams.txt"
areafract.out <- "../../../Data/Transformed/Parameters/areafract_Dahri_5min_Indus.RDS"
elevation.out <- "../../../Data/Transformed/Parameters/elevation_Dahri_5min_Indus.RDS"
pfactor.out <- "../../../Data/Transformed/Parameters/pfactor_Dahri_5min_Indus.RDS"
elev.out <- "../../../Data/Transformed/Parameters/elev_Dahri_5min_Indus.RDS"

# Load
elev.text <- readLines(con = elev.file)
mapping <- read.table(mapping.file)

# Setup
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]
nbands <- 25

# Calculate
areafract.map <- array(NA, dim = c(length(out.lons), length(out.lats), nbands))
elevation.map <- array(NA, dim = c(length(out.lons), length(out.lats), nbands))
pfactor.map <- array(NA, dim = c(length(out.lons), length(out.lats), nbands))
elev.map <- array(NA, dim = c(length(out.lons), length(out.lats)))
i = 1
for (i in 1:length(elev.text)) {
  elev.line <- elev.text[i]
  elev.fields <- strsplit(x = elev.line, split = "\t")[[1]]
  elev.fields <- as.numeric(elev.fields)
  
  cell <- elev.fields[1]
  row <- which(mapping[, 2] == cell)
  
  x.diff = abs(out.lons - mapping[row, 4])
  y.diff = abs(out.lats - mapping[row, 3])
  if(min(x.diff) > resolution / 2 || min(y.diff) > resolution / 2){
    next
  }
  
  x = which.min(x.diff)
  y = which.min(y.diff)
  
  areafract.map[x,y,] <- elev.fields[2:26]
  elevation.map[x,y,] <- elev.fields[27:51]
  pfactor.map[x,y,] <- elev.fields[52:76]
  elev.map[x,y] <- mapping[row, 22]
}
for (i in 1:nbands) {
  image.plot(elevation.map[, , i], main = i)
}
image.plot(elev.map)

# Save
dir.create(dirname(areafract.out))
saveRDS(areafract.map, areafract.out)
dir.create(dirname(elevation.out))
saveRDS(elevation.map, elevation.out)
dir.create(dirname(pfactor.out))
saveRDS(pfactor.map, pfactor.out)
dir.create(dirname(elev.out))
saveRDS(elev.map, elev.out)

