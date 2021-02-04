library(ncdf4)
library(fields)
rm(list = ls())

# Input
map.support.file <- "../../../../Scripts/Support/mapFunctions.R"
generate.support.file <- "../../../../Scripts/Support/generateFunctions.R"

bco2.file <- "../../../../Data/Transformed/CO2/bco2_30min_global.RDS"

mask.file <- "../../../../Data/Primary/VIC/domain_global.nc"
vic.out <- "../../../../Data/VIC/Parameters/global/co2_params_Vliet_global.nc"

# Load
source(map.support.file)
source(generate.support.file)

bco2 <- readRDS(bco2.file)

nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

# Setup
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
nveg_class = dim(bco2)[3]

na.map <- is.na(mask) | mask == 0

# Calculate
bco2[,,12] = 0

bco2.fill <- fillMap(bco2, na.map, getNearestMean)

# Create
dim.lon <- ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = lons,
  longname = "longitude of grid cell center"
)
dim.lat <- ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = lats,
  longname = "latitude of grid cell center"
)
dim.veg <- ncdim_def(
  name = "veg_class", units = "class", vals = 1:nveg_class,
  longname = "Vegetation class"
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
