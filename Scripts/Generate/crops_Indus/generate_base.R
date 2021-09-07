library(ncdf4)
library(fields)
rm(list = ls())

# Input
crop.out <- "../../../Data/VIC/Parameters/Indus_5min/single/crop_params_base_Indus.nc"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

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
dim.crop <- ncdim_def(
  name = "crop_class",
  units = "#",
  vals = 1,
  longname = "Crop class"
)
dim.month <- ncdim_def(
  name = "month",
  units = "month of year",
  vals = 1:12,
  longname = "month of year (1-12)"
)

var.Ncrop <- ncvar_def(
  name = "Ncrop",
  units = "#",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "Number of active crop classes",
  compression = 5
)
var.crop_veg_class <- ncvar_def(
  name = "crop_veg_class",
  units = "#",
  dim = list(dim.lon, dim.lat, dim.crop),
  missval = -1,
  longname = "Crop vegetation class",
  compression = 5
)
var.Cc <- ncvar_def(
  name = "Cc",
  units = "fraction",
  dim = list(dim.lon, dim.lat, dim.crop, dim.month),
  missval = -1,
  longname = "Crop coverage per month",
  compression = 5
)
var.Tfactor <- ncvar_def(
  name = "Tfactor",
  units = "K",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "Temperature factor due to elevation",
  compression = 5
)

## Calculate
template = array(0, dim = c(length(out.lons), length(out.lats)))
template.monthly = array(0, dim = c(length(out.lons), length(out.lats), 12))
  
Ncrop.filled <- template + 1
cc.filled <- template.monthly + 1
veg.class.filled <- template + 1
tfactor.filled <- template
  
dir.create(dirname(crop.out))
nc <- nc_create(
  crop.out,
  list(
    var.Ncrop,
    var.Cc,
    var.crop_veg_class,
    var.Tfactor
  )
)
nc_close(nc)

nc <- nc_open(crop.out, write = T)
ncvar_put(nc = nc, varid = var.Ncrop, vals = Ncrop.filled)
ncvar_put(nc = nc, varid = var.Cc, vals = cc.filled)
ncvar_put(nc = nc, varid = var.crop_veg_class, vals = veg.class.filled)
ncvar_put(nc = nc, varid = var.Tfactor, vals = tfactor.filled)

ncatt_put(
  nc = nc,
  varid = 0,
  attname = "Description",
  attval = "Crop parameters for VIC. Created by Bram Droppers"
)
nc_close(nc)
