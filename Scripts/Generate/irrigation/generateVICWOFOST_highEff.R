library(ncdf4)
library(fields)
rm(list = ls())

# Input
support.script <- "../../../Scripts/Support/mapFunctions.R"
mask.file <- "../../../Data/Transformed/Routing/mask_30min_global.RDS"
gw.file <- "../../../Data/Transformed/Irrigation/irrigationGroundwaterFraction_30min_global.RDS"
eff.file <- "../../../Data/Transformed/Irrigation/irrigationEfficiency_30min_global.RDS"
irr.param.out <- "../../../Data/VIC/Parameters/global/irr_params_VICWOFOST_highEff_global.nc"
split <- data.frame(
  name = c(
    "wheatRainfed", "wheatIrrigated",
    "maizeRainfed", "maizeIrrigated",
    "riceRainfed", "riceIrrigated",
    "soybeanRainfed", "soybeanIrrigated"
  ),
  irr = c(
    0, 1,
    0, 1,
    0, 1,
    0, 1
  ),
  paddy = c(
    0, 0,
    0, 0,
    0, 1,
    0, 0
  ),
  stringsAsFactors = F
)

# Load
source(support.script)

groundwater <- readRDS(gw.file)
eff <- readRDS(eff.file)
mask <- readRDS(mask.file)

# Setup
na.map <- is.na(mask) | mask == 0

irr.veg <- c(12)
irr.pond <- c(0)
for (i in 1:nrow(split)) {
  if (split$irr[i] == 1) {
    irr.veg <- c(irr.veg, 12 + i)
    irr.pond <- c(irr.pond, split$paddy[i])
  }
}

groundwater.filled <- fillMap(map = groundwater, na.map = na.map, nearest.function = getNearestMean)
eff.filled <- fillMap(map = eff, na.map = na.map, nearest.function = getNearestMean)
# image.plot(groundwater.filled)
# image.plot(eff.filled)

# Save
dim.lon <- ncdim_def("lon", "degrees_east", seq(
  from = -179.75,
  to = 179.75,
  by = 0.5
))
dim.lat <- ncdim_def("lat", "degrees_north", seq(from = -89.75, to = 89.75, by = 0.5))
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
ncvar_put(nc, var.groundwater_fraction, groundwater.filled * 0)
ncvar_put(nc, var.irrigation_efficiency, eff.filled * 1.25)
nc_close(nc)
