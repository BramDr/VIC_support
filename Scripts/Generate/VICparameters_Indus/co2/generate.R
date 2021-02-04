library(ncdf4)
library(fields)
rm(list = ls())

# Input
map.support.file <- "../../../../Scripts/Support/mapFunctions.R"
generate.support.file <- "../../../../Scripts/Support/generateFunctions.R"
mask.file <- "../../../../Data/Transformed/Routing/distance_5min_Indus.RDS"

bco2.file <- "../../../../Data/Transformed/CO2/bco2_5min_Indus.RDS"

vic.out <- "../../../../Data/VIC/Parameters/Indus_5min/co2_params_Indus.nc"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
source(map.support.file)
source(generate.support.file)
mask <- readRDS(mask.file)

bco2 <- readRDS(bco2.file)

# Setup
nveg_class = dim(bco2)[3]

na.map <- is.na(mask) | mask == 0

# Calculate
bco2.fill <- fillMap(bco2, na.map, getNearestMean)

# Create
dim.lon <- ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = out.lons,
  longname = "longitude of grid cell center"
)
dim.lat <- ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = out.lats,
  longname = "latitude of grid cell center"
)
dim.veg <- ncdim_def(
  name = "veg_class", units = "class", vals = 1:nveg_class,
  longname = "vegetation class"
)

var.bco2 <- ncvar_def(
  name = "b_co2",
  units = "fraction",
  dim = list(dim.lon, dim.lat, dim.veg),
  missval = -1,
  longname = "CO2 transpiration parameter",
  prec = "double",
  compression = 9
)

dir.create(dirname(vic.out))
nc <- nc_create(
  vic.out,
  list(
    var.bco2
  )
)
nc_close(nc)

# Save
nc <- nc_open(vic.out, write = T)
ncatt_put(
  nc = nc,
  varid = 0,
  attname = "Description",
  attval = "CO2 parameters for VIC. Created by Bram Droppers"
)

ncvar_put(nc, nc$var$b_co2, bco2.fill)

nc_close(nc)
