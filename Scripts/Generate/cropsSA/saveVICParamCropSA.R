library(ncdf4)
library(fields)
rm(list = ls())

# Input
param.out <- "../../../Data/VIC/Parameters/global/SA/crop_params_WOFOST_global.nc"

# Setup
lat <- seq(-89.75, 89.75, by = 0.5)
lon <- seq(-179.75, 179.75, by = 0.5)
crop_class <- 1
months <- 1:12

Ncrop = array(1, dim = c(length(lon), length(lat)))
crop_veg_class = array(1, dim = c(length(lon), length(lat), length(crop_class)))
Cc = array(1, dim = c(length(lon), length(lat), length(crop_class), length(months)))

# Save
dim.lon <- ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = lon,
  longname = "longitude of grid cell center"
)
dim.lat <- ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = lat,
  longname = "latitude of grid cell center"
)
dim.crop <- ncdim_def(
  name = "crop_class", 
  units = "class", 
  vals = crop_class,
  longname = "Crop class: 1 - WOFOST"
)
dim.month <- ncdim_def(
  name = "month", 
  units = "months of year", 
  vals = months,
  longname = "month of year"
)

Ncrop.var <- ncvar_def(
  name = "Ncrop",
  units = "N/A",
  dim = list(dim.lon, dim.lat),
  longname = "Number of crop tiles in the grid cell"
)
crop_veg_class.var <- ncvar_def(
  name = "crop_veg_class",
  units = "N/A",
  dim = list(dim.lon, dim.lat, dim.crop),
  longname = "Crop vegetation class"
)
Cc.var <- ncvar_def(
  name = "Cc",
  units = "fraction",
  dim = list(dim.lon, dim.lat, dim.crop, dim.month),
  longname = "Fraction of vegetation tile covered by crop tile each month"
)

dir.create(dirname(param.out))
nc <- nc_create(filename = param.out, vars = list(Ncrop.var, crop_veg_class.var, Cc.var))

ncvar_put(nc, "Ncrop", Ncrop)
ncvar_put(nc, "crop_veg_class", crop_veg_class)
ncvar_put(nc, "Cc", Cc)

ncatt_put(
  nc = nc,
  varid = 0,
  attname = "Description",
  attval = "crop parameters using WOFOST Created by Bram Droppers"
)
nc_close(nc)
