library(ncdf4)
rm(list = ls())

# Input
generate.support.file <- "../../../Support/generateFunctions.R"
bco2.file <- "../../../../Data/Transformed/CO2/bco2_30min_global.RDS"
co2.out <- "../../../../Data/VIC/Parameters/FACE/Arizona/co2_params_Arizona.nc"

point <- c(33.0628, -111.9826) # lat-lon

# Load
bco2 <- readRDS(bco2.file)

# Setup
bco2[,,12] = 0
bco2 <- bco2[1,1,11:12]

# Create
dim.lon <- ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = point[2],
  longname = "longitude of grid cell center"
)
dim.lat <- ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = point[1],
  longname = "latitude of grid cell center"
)
dim.veg <- ncdim_def(
  name = "veg_class", units = "class", vals = 1:2,
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

dir.create(dirname(co2.out))
nc <- nc_create(
  co2.out,
  list(
    var.bco2
  )
)
nc_close(nc)

# Save
nc <- nc_open(co2.out, write = T)
ncatt_put(
  nc = nc,
  varid = 0,
  attname = "Description",
  attval = "CO2 parameters for VIC. Created by Bram Droppers"
)

ncvar_put(nc, nc$var$b_co2, bco2)
nc_close(nc)
