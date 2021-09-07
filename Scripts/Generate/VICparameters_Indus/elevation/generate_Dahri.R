library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
map.support.file <- "../../../../Scripts/Support/mapFunctions.R"
generate.support.file <- "../../../../Scripts/Support/generateFunctions.R"
mask.file <- "../../../../Data/Transformed/Parameters/mask_Dahri_5min_indus.RDS"

elevation.file = "../../../../Data/Transformed/Parameters/elevation_Dahri_5min_indus.RDS"
elev.file = "../../../../Data/Transformed/Parameters/elev_Dahri_5min_indus.RDS"
areafract.file = "../../../../Data/Transformed/Parameters/areafract_Dahri_5min_indus.RDS"
pfactor.file = "../../../../Data/Transformed/Parameters/pfactor_Dahri_5min_indus.RDS"

vic.orig <- "../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out <- "../../../../Data/VIC/Parameters/Indus_5min/elevation_params_Dahri_Indus.nc"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
source(map.support.file)
source(generate.support.file)
mask = readRDS(mask.file)

elev = readRDS(elev.file)
elevation = readRDS(elevation.file)
areafract = readRDS(areafract.file)
pfactor = readRDS(pfactor.file)

# Setup
na.map <- is.na(mask) | mask == 0

nelev = apply(X = areafract, MARGIN = c(1,2), FUN = function(x){sum(x > 0, na.rm = T)})

# Calculate
nelev.fill <- fillMap(nelev, na.map, getNearestZero)
elevation.fill <- fillMap(elevation, na.map, getNearestMean)
elev.fill <- fillMap(elev, na.map, getNearestMean)
areafract.fill <- fillMap(areafract, na.map, getNearestMean)
pfactor.fill <- fillMap(pfactor, na.map, getNearestMean)

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
dim.snow_band <- ncdim_def(
  name = "snow_band",
  units = "band",
  vals = 1:max(nelev, na.rm = T),
  longname = "snow band"
)

var.list = list()
for(var in elev.vars){
  print(var)
  if(var %in% c("elev", "Nelev")){
    var.var = cropyCreateVariable(base.file = vic.orig, base.name = var,
                                   dim = list(dim.lon, dim.lat), 
                                   chunksizes = c(length(out.lons), length(out.lats)))
  } else {
    var.var = cropyCreateVariable(base.file = vic.orig, base.name = var,
                                  dim = list(dim.lon, dim.lat, dim.snow_band), 
                                  chunksizes = c(length(out.lons), length(out.lats), 1))
  }
  
  var.list[[var]] = var.var
}
var.list[["Nelev"]] = 
  Nelev.var <- ncvar_def(name = "Nelev", 
                          units = "#", 
                          dim = list(dim.lon, dim.lat), 
                          longname = "Number of active snow bands", 
                          prec = "integer", missval = -1)

nc = nc_create(vic.out, vars = var.list)
nc_close(nc)

# Save
nc <- nc_open(vic.out, write = T)
ncvar_put(nc, "Nelev", nelev.fill)
ncvar_put(nc, "elev", elev.fill)
ncvar_put(nc, "elevation", elevation.fill[,,1:max(nelev, na.rm = T)])
ncvar_put(nc, "Pfactor", areafract.fill[,,1:max(nelev, na.rm = T)])
ncvar_put(nc, "AreaFract", areafract.fill[,,1:max(nelev, na.rm = T)])
nc_close(nc)
