library(ncdf4)
rm(list = ls())

# Input
ocar.file <- "../VIC/Soil/Saves_observed/ocar_observed.RDS"
ph.file <- "../VIC/Soil/Saves_point/ph_point.RDS"
crop.out <- "../../../../Data/VIC/Parameters/FACE/crop_params_observed.nc"

point <- c(33.0628, -111.9826) # lat-lon

# Load
ocar = readRDS(ocar.file)
ph = readRDS(ph.file)

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
var.carbon <- ncvar_def(
  name = "carbon",
  units = "g kg-1",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "Organic carbon content for top 20 cm of soil",
  compression = 5
)
var.ph <- ncvar_def(
  name = "pH",
  units = "-",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "pH for top 20 cm of soil",
  compression = 5
)

dir.create(dirname(crop.out))
nc <- nc_create(
  crop.out,
  list(
    var.Ncrop,
    var.Cc,
    var.crop_veg_class,
    var.Tfactor,
    var.carbon,
    var.ph
  )
)
nc_close(nc)

nc <- nc_open(crop.out, write = T)
ncvar_put(nc, var.Ncrop, 1)
ncvar_put(nc, var.Cc, rep(1, 12))
ncvar_put(nc, var.crop_veg_class, 1)
ncvar_put(nc, var.Tfactor, 0)
ncvar_put(nc, var.carbon, mean(ocar[1:3]))
ncvar_put(nc, var.ph, ph[1])

ncatt_put(
  nc = nc,
  varid = 0,
  attname = "Description",
  attval = "Crop parameters for VIC. Created by Bram Droppers"
)
nc_close(nc)
