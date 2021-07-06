rm(list = ls())
library(ncdf4)
library(fields)

# Input
vegatation.file <- "./Saves/vegetation_mapping.csv"
cv.file <- "../../../../Data/Transformed/MODIS/cv_5min_Indus.RDS"
cv.out = "./Saves/Cv_5min_Indus.RDS"
frac.out = "./Saves/frac_5min_Indus.RDS"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
vegetation = read.csv(vegatation.file, stringsAsFactors = F)
cv.modis = readRDS(cv.file)

# Setup
cv.modis.sum = apply(X = cv.modis, MARGIN = c(1,2), FUN = sum)
image.plot(cv.modis.sum)

# Calculate
cv = array(0, dim = c(length(out.lons), length(out.lats), length(na.omit(unique(vegetation$vic)))))
frac = array(0, dim = c(length(out.lons), length(out.lats)))
for(i in 1:nrow(vegetation)){
  if(is.na(vegetation$vic[i])){
    frac = frac + cv.modis[,,i]
    next
  }
  cv[,,vegetation$vic[i]] = cv[,,vegetation$vic[i]] + cv.modis[,,i]
}
image.plot(frac, main = "frac")

# Set leftover Cv to bare soil
cv.sum = apply(X = cv, MARGIN = c(1,2), FUN = sum)
cv[,,dim(cv)[3]] = cv[,,dim(cv)[3]] + (1 - cv.sum)

cv.forest = apply(X = cv[,,1:5], MARGIN = c(1,2), FUN = sum)
cv.low = apply(X = cv[,,c(6:11)], MARGIN = c(1,2), FUN = sum)
cv.build = cv[,,13]
cv.crop = cv[,,12]
image.plot(cv.forest, main = "cv.forest")
image.plot(cv.low, main = "cv.low")
image.plot(cv.build, main = "cv.build")
image.plot(cv.crop, main = "cv.crop")

dir.create(dirname(cv.out))
dir.create(dirname(frac.out))
saveRDS(cv, cv.out)
saveRDS(frac, frac.out)
