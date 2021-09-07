library(ncdf4)
library(fields)
rm(list = ls())

# Input
irr.param.out <- "../../../Data/VIC/Parameters/FACE/Shizukuishi/irr_params_Shizukuishi.nc"

point <- c(39.633333, 140.950000) # lat-lon

# Setup
irr.veg <- c(1)
irr.pond <- c(0)

# Save
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
dim.irr.class <- ncdim_def("irr_class", "class", 1:length(irr.veg), longname = "Irrigation class")

var.veg.class <- ncvar_def("veg_class",
  "class",
  list(dim.irr.class),
  -1,
  longname = "Vegetation class of irrigation class",
  prec = "integer"
)
var.paddy <- ncvar_def(
  "paddy",
  "0 = not paddy irrigation, 1 = paddy irrigation",
  list(dim.irr.class),
  -1,
  longname = "Paddy indicator",
  prec = "integer"
)
var.groundwater_fraction <- ncvar_def(
  "groundwater_fraction",
  "-",
  list(dim.lon, dim.lat),
  -1,
  longname = "Fraction of irrigation comming from groundwater",
  prec = "double"
)
var.irrigation_efficiency <- ncvar_def(
  "irrigation_efficiency",
  "mm mm-1",
  list(dim.lon, dim.lat),
  -1,
  longname = "Fraction of water withdrawn per water required",
  prec = "double"
)

dir.create(dirname(irr.param.out))
nc <- nc_create(
  irr.param.out,
  list(var.veg.class, var.paddy, var.groundwater_fraction, var.irrigation_efficiency)
)
nc_close(nc)

nc <- nc_open(irr.param.out, write = T)
ncvar_put(nc, var.veg.class, irr.veg)
ncvar_put(nc, var.paddy, irr.pond)
ncvar_put(nc, var.groundwater_fraction, 0)
ncvar_put(nc, var.irrigation_efficiency, 1)
nc_close(nc)
