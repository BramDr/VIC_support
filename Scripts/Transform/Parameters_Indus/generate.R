library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
map.support.file <- "../../../../Scripts/Support/mapFunctions.R"
generate.support.file <- "../../../../Scripts/Support/generateFunctions.R"
cv.file <- "../Spatial_aggregation/Saves/Cv_5min_Indus.RDS"
lai.file <- "../Spatial_interpolation/Saves/LAI_5min_Indus.RDS"
albedo.file <- "../Spatial_interpolation/Saves/albedo_5min_Indus.RDS"
fcanopy.file <- "../Spatial_interpolation/Saves/fcanopy_5min_Indus.RDS"
vic.out <- "./vegetation_params_Indus.nc"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 37)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
source(map.support.file)
source(generate.support.file)

cv <- readRDS(cv.file)
lai <- readRDS(lai.file)
albedo <- readRDS(albedo.file)
fcanopy <- readRDS(fcanopy.file)

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
dim.veg_class <- ncdim_def(
  name = "veg_class",
  units = "class",
  vals = 1:dim(cv)[3],
  longname = "Vegetation class"
)
dim.month <- ncdim_def(
  name = "month",
  units = "month of year",
  vals = 1:dim(albedo)[4],
  longname = "Month of year"
)

var.Cv <- ncvar_def(
  name = "Cv",
  units = "fraction",
  dim = list(dim.lon, dim.lat, dim.veg_class)
)
var.albedo <- ncvar_def(
  name = "albedo",
  units = "fraction",
  dim = list(dim.lon, dim.lat, dim.veg_class, dim.month)
)
var.fcanopy <- ncvar_def(
  name = "fcanopy",
  units = "fraction",
  dim = list(dim.lon, dim.lat, dim.veg_class, dim.month)
)
var.LAI <- ncvar_def(
  name = "LAI",
  units = "m2 m-2",
  dim = list(dim.lon, dim.lat, dim.veg_class, dim.month)
)

nc <- nc_create(
  vic.out,
  list(var.Cv, var.albedo, var.fcanopy, var.LAI)
)
ncvar_put(nc, "Cv", cv)
ncvar_put(nc, "albedo", albedo)
ncvar_put(nc, "fcanopy", fcanopy)
ncvar_put(nc, "LAI", lai)
nc_close(nc)
