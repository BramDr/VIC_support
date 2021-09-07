library(ncdf4)
library(fields)
rm(list = ls())

# Input
path.domain <- "../../../Data/VIC/Parameters/Indus_5min/domain_Dahri_Indus.nc"
path.mask <- "../../../Data/VIC/Parameters/Indus_5min/domain_Dahri_Indus.nc"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Setup
nc = nc_open(path.mask)
mask.orig = ncvar_get(nc, "mask")
nc_close(nc)

nc = nc_open(path.domain)
lon <- nc$dim$lon$vals
lat <- nc$dim$lat$vals
nc_close(nc)

points <- data.frame(lat = numeric(), lon = numeric(), name = character(), stringsAsFactors = F)
for(x in seq(from = 2, by = 6, to = length(out.lons))){
  for(y in seq(from = 2, by = 6, to = length(out.lats))){
    points[nrow(points) + 1, ] <- c(out.lats[y], out.lons[x], "")
  }
}
points$name[1] = "check_Indus"

points$lat <- as.numeric(points$lat)
points$lon <- as.numeric(points$lon)

# Calculate
mask.combine <- array(NA, dim = c(length(lon), length(lat)))
for (i in 1:nrow(points)) {
  x <- which.min(abs(lon - points$lon[i]))
  y <- which.min(abs(lat - points$lat[i]))
  if(is.na(mask.orig[x,y]) || mask.orig[x,y] == 0){
    next
  }
  
  mask.combine[x, y] <- 1
}
image.plot(mask.combine)

# Save
newname <- paste0(dirname(path.domain), "/", "domain_", paste0(points$name, collapse = ""), ".nc")

dir.create(dirname(newname))
file.copy(path.domain, newname, overwrite = T)

nc <- nc_open(newname, write = T)
ncvar_put(nc, nc$var$mask, mask.combine)
nc_close(nc)
