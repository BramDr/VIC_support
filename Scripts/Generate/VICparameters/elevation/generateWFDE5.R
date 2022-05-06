library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
map.support.file <- "../../../../Scripts/Support/mapFunctions.R"
generate.support.file <- "../../../../Scripts/Support/generateFunctions.R"
mask.file <- "../../../../Data/Primary/VIC/domain_global.nc"

elev.file <- "../../../../Data/Transformed/DEM/DEM_WFDE5_30m_global.RDS"

vic.out <- "../../../../Data/VIC/Parameters/global/elevation_params_WFDE5_global.nc"

# Load
source(map.support.file)
source(generate.support.file)

nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
lon.dim = nc$dim$lon
lat.dim = nc$dim$lat
nc_close(nc)

elev = readRDS(elev.file)

# Setup
na.map <- is.na(mask) | mask == 0
elev.fill <- fillMap(elev, na.map, getNearestMean)

# Save
elevation.vars = loadElevationVars(lon.dim, lat.dim)

nc = nc_create(vic.out, vars = list(elevation.vars$elev))
ncvar_put(nc, "elev", elev.fill)
nc_close(nc)
