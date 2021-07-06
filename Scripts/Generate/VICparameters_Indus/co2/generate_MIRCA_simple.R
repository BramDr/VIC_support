library(ncdf4)
library(fields)
rm(list = ls())

# Input
map.support.file <- "../../../Support/mapFunctions.R"
generate.support.file <- "../../../Support/generateFunctions.R"
mask.file <- "../../../../Data/Transformed/Routing/mask_5min_indus.RDS"

bco2.file <- "../../../../Data/Transformed/CO2/bco2_5min_Indus.RDS"

vic.out <- "../../../../Data/VIC/Parameters/Indus_5min/co2_params_Mirca_simple_Indus.nc"

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
ncrops = 3
nbare = 2
nveg_class = dim(bco2)[3] + ncrops + nbare - 1

na.map <- is.na(mask) | mask == 0

# Calculate
old.veg.idxs = 1:(dim(bco2)[3] - 1)
old.crop.idx = 12
old.bare.idx = dim(bco2)[3]

new.veg.idxs = old.veg.idxs
new.crop.idxs = (dim(bco2)[3]):(nveg_class - nbare)
new.bare.idx = (nveg_class - nbare + 1):nveg_class

bco2.adj = array(NA, dim = c(dim(bco2)[1:2], nveg_class))
bco2.adj[,,new.veg.idxs] = bco2[,,old.veg.idxs]
bco2.adj[,,new.crop.idxs] = bco2[,,old.crop.idx]
bco2.adj[,,new.bare.idx] = bco2[,,old.bare.idx]

plot(bco2.adj[1,1,], type = "l")
bco2.fill <- fillMap(bco2.adj, na.map, getNearestMean)

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

