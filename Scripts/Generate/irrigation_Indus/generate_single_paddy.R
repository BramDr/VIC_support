library(ncdf4)
library(fields)
rm(list = ls())

# Input
irr.param.out <- "../../../Data/VIC/Parameters/Indus_5min/single/irrigation_params_single_paddy_Indus.nc"
irr.eff = 0.6
irr.offset = 0:4

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
template = array(0, dim = c(length(out.lons), length(out.lats)))
groundwater = template
eff = template + irr.eff
offset = array(irr.offset, dim = dim(template))

# Setup
irr.veg <- c(1)
irr.pond <- c(1)

# Save
dim.lon <- ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = out.lons,
  longname = "longitude of cell centre"
)
dim.lat <- ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = out.lats,
  longname = "latitude of cell centre"
)
dim.irr.class <- ncdim_def("irr_class", "class", 
                           1:length(irr.veg), 
                           longname = "Irrigation class")

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
  longname = "Fraction of water applied to field per withdrawn from river",
  prec = "double"
)
var.irrigation_offset <- ncvar_def(
  "irrigation_offset",
  "days",
  list(dim.lon, dim.lat),
  -1,
  longname = "Offset at the start of the irrigation season",
  prec = "integer"
)

dir.create(dirname(irr.param.out))
nc <- nc_create(
  irr.param.out,
  list(var.veg.class, var.paddy, var.groundwater_fraction, var.irrigation_efficiency, var.irrigation_offset)
)
nc_close(nc)

nc <- nc_open(irr.param.out, write = T)
ncvar_put(nc, var.veg.class, irr.veg)
ncvar_put(nc, var.paddy, irr.pond)
ncvar_put(nc, var.groundwater_fraction, groundwater)
ncvar_put(nc, var.irrigation_efficiency, eff)
ncvar_put(nc, var.irrigation_offset, offset)
nc_close(nc)
