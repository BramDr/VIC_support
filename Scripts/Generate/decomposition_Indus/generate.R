library(fields)
library(ncdf4)
rm(list = ls())

# Input
basin.file <- "../../../Data/Transformed/Delta/deltaBasins_5min_Indus.RDS"
decomp.out <- "../../../Data/VIC/Parameters/Indus_5min/decomp_params_Indus.nc"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
basins <- readRDS(basin.file)

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

var.basin <- ncvar_def(
  name = "basin",
  units = "#",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "Basin ID",
  compression = 9
)

dir.create(dirname(decomp.out))
nc <- nc_create(
  decomp.out,
  list(
    var.basin
  )
)
nc_close(nc)

# Save
nc <- nc_open(decomp.out, write = T)
ncatt_put(
  nc = nc,
  varid = 0,
  attname = "Description",
  attval = "Decomposition parameters for VIC. Created by Bram Droppers"
)

ncvar_put(nc, var.basin, basins)

nc_close(nc)
