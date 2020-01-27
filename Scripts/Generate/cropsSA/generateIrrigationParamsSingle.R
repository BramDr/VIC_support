library(fields)
library(ncdf4)
rm(list = ls())

# Input
function.script = "../../Support/mapFunctions.R"
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
irrigation.out <- "../../../Data/VIC/Parameters/global/SA/irr_params_single_global.nc"

# Setup
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
lons <- seq(from = -179.75, to = 179.75, by = 0.5)

irr.veg <- c(1)
irr.pond <- c(0)
groundwater <- array(data = 0, dim = c(length(lons), length(lats)))
eff <- array(data = 1, dim = c(length(lons), length(lats)))

# Load
source(function.script)

nc = nc_open(mask.file)
na.map = ncvar_get(nc, nc$var$mask)
nc_close(nc)
na.map = is.na(na.map) | na.map != 1
image.plot(na.map)

# Calculate
groundwater <- fillMap(map = groundwater, na.map = na.map, nearest.function = getNearestMean)
eff <- fillMap(map = eff, na.map = na.map, nearest.function = getNearestMean)

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
dim.irr.class <- ncdim_def(
  name = "irr_class", 
  units = "class", 
  vals = 1:length(irr.veg), 
  longname = "Irrigation class"
)
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

dir.create(dirname(irrigation.out))
nc <- nc_create(
  irrigation.out,
  list(var.veg.class, var.paddy, var.groundwater_fraction, var.irrigation_efficiency)
)

ncvar_put(nc, var.veg.class, irr.veg)
ncvar_put(nc, var.paddy, irr.pond)
ncvar_put(nc, var.groundwater_fraction, groundwater)
ncvar_put(nc, var.irrigation_efficiency, eff)
nc_close(nc)
