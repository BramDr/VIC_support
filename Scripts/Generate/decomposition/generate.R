library(fields)
library(ncdf4)
rm(list = ls())

# Input
basin.file <- "../../../Data/Transformed/Delta/deltaBasins_30min_global.RDS"
mask.file <- "../../../Data/Transformed/Routing/mask_30min_global.RDS"
decomp.out <- "../../../Data/VIC/Parameters/global/decomp_params_global.nc"

# Load
mask <- readRDS(mask.file)
basins <- readRDS(basin.file)

# Setup
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
lons <- seq(from = -179.75, to = 179.75, by = 0.5)

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
