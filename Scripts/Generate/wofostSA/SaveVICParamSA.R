library(ncdf4)
library(fields)
rm(list = ls())

# Input
function.script <- "generateFunctions.R"
param.file <- "../../../Data/VIC/Parameters/global/VIC_params_global.nc"
param.out <- "../../../Data/VIC/Parameters/global/SA/VIC_params_WOFOST_global.nc"

# Setup
remove.vars <- c(
  "veg_descr", "veg_class", "Nveg", "Cv", "wind_atten",
  "wind_h", "rmin", "rarc", "rad_atten", "RGL",
  "trunk_ratio", "overstory", "root_fract", "root_depth",
  "LAI", "displacement", "veg_rough", "albedo"
)
system(command = paste0("ncks -x -v ", paste0(remove.vars, collapse = ","), " ", param.file, " -O ", param.out))

# Calculate
nc <- nc_open(filename = param.out)
nc_close(nc = nc)

lon.dim <- nc$dim$lon
lat.dim <- nc$dim$lat
root.dim <- nc$dim$root_zone
month.dim <- nc$dim$month
veg.dim <- ncdim_def(
  name = "veg_class", units = "class", vals = 1,
  longname = "Vegetation class: 1 - WOFOST crop"
)

# Save
source(function.script)
addVegVars(param.out, lon.dim, lat.dim, veg.dim, root.dim, month.dim)
addVegData(param.out)

nc <- nc_open(filename = param.out, write = T)
ncatt_put(
  nc = nc,
  varid = 0,
  attname = "Description",
  attval = "VIC parameters using WOFOST Created by Bram Droppers"
)
nc_close(nc)
