library(ncdf4)
library(fields)
rm(list = ls())

# Input
map.support.file <- "../../Support/mapFunctions.R"
generate.support.file <- "../../Support/generateFunctions.R"
mask.file <- "../../../Data/Transformed/Routing/mask_5min_indus.RDS"
irr.param.out <- "../../../Data/VIC/Parameters/Indus_5min/irrigation_params_Indus.nc"
irr.eff = 0.6
irr.offset = 0:4

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

split <- data.frame(
  name = c(
    "wheatRainfed", "wheatIrrigated",
    "riceRainfed", "riceIrrigated"
  ),
  irr = c(
    0, 1,
    0, 1
  ),
  paddy = c(
    0, 0,
    0, 1
  ),
  stringsAsFactors = F
)

# Load
source(map.support.file)
source(generate.support.file)

mask <- readRDS(mask.file)
groundwater = mask * 0
eff = mask * 0 + irr.eff
offset = array(irr.offset, dim = dim(eff))

# Setup
na.map <- is.na(mask) | mask == 0

irr.veg <- c(15, 16)
irr.pond <- c(0, 1)
for (i in 1:nrow(split)) {
  if (split$irr[i] == 1) {
    irr.veg <- c(irr.veg, 16 + i)
    irr.pond <- c(irr.pond, split$paddy[i])
  }
}

groundwater.filled <- fillMap(map = groundwater, na.map = na.map, nearest.function = getNearestMean)
eff.filled <- fillMap(map = eff, na.map = na.map, nearest.function = getNearestMean)
offset.filled <- fillMap(map = offset, na.map = na.map, nearest.function = getNearestMean)
# image.plot(groundwater.filled)
# image.plot(eff.filled)
# image.plot(offset.filled)

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
ncvar_put(nc, var.groundwater_fraction, groundwater.filled * 0)
ncvar_put(nc, var.irrigation_efficiency, eff.filled)
ncvar_put(nc, var.irrigation_offset, offset.filled)
nc_close(nc)
