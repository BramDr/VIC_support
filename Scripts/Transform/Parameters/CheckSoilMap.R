library(fields)
library(ncdf4)
rm(list = ls())

# Input
soil.file <- "../../../Data/Primary/vanVliet2016/global_soil_file.new.arno.modified.fe.wfd"
header.file <- "../../../Data/Primary/Nijssen2001/2deg/world.soil.parameter.hdr"
orig.file <- "../../../Data/Primary/VIC/VIC_params_global.nc"

# Load
soil <- read.table(file = soil.file)
header <- read.table(file = header.file)

nc <- nc_open(orig.file)
depth <- ncvar_get(nc, nc$var$depth)
infilt <- ncvar_get(nc, nc$var$infilt)
bulk.density <- ncvar_get(nc, nc$var$bulk_density)
soil.density <- ncvar_get(nc, nc$var$soil_density)
porosity <- 1 - (bulk.density / soil.density)
max.moist <- depth * porosity * 1000
ksat <- ncvar_get(nc, nc$var$Ksat)
d1 <- ncvar_get(nc, nc$var$Ds)
d2 <- ncvar_get(nc, nc$var$Dsmax)
d3 <- ncvar_get(nc, nc$var$Ws)
d4 <- ncvar_get(nc, nc$var$c)
wcr <- ncvar_get(nc, nc$var$Wcr)
wp <- ncvar_get(nc, nc$var$Wp)
nc_close(nc)

# Setup
lons <- seq(from = -179, to = 179, by = 2)
lats <- seq(from = -89, to = 89, by = 2)
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)

# Calculate
var <- "d2"
map <- array(NA, dim = c(length(lons), length(lats)))
lat.idx <- which(header[1, ] == "lat")
lon.idx <- which(header[1, ] == "lon")
var.idx <- which(header[1, ] == var)
for (i in 1:nrow(soil)) {
  x <- which(lons == soil[i, lon.idx])
  y <- which(lats == soil[i, lat.idx])
  map[x, y] <- soil[i, var.idx]
}

image.plot(map)
image.plot(ksat[, , 1])
