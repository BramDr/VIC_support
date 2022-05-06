library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
map.support.file <- "../../../../Scripts/Support/mapFunctions.R"
generate.support.file <- "../../../../Scripts/Support/generateFunctions.R"
mask.file <- "../../../../Data/Primary/VIC/domain_global.nc"

elevation.file = "./Saves/elevation_ETOPO5_30min_global.RDS"
elev.file = "./Saves/elev_ETOPO5_30min_global.RDS"
nelev.file = "./Saves/Nelev_ETOPO5_30min_global.RDS"
areafract.file = "./Saves/AreaFract_ETOPO5_30min_global.RDS"

vic.orig <- "../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out <- "../../../../Data/VIC/Parameters/global/elevation_params_ETOPO_global.nc"

# Load
source(map.support.file)
source(generate.support.file)

nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
lon.dim = nc$dim$lon
lat.dim = nc$dim$lat
nc_close(nc)

elevation = readRDS(elevation.file)
elev = readRDS(elev.file)
nelev = readRDS(nelev.file)
areafract = readRDS(areafract.file)

# Setup
na.map <- is.na(mask) | mask == 0

# Calculate
nelev.fill <- fillMap(nelev, na.map, getNearestZero)
elevation.fill <- fillMap(elevation, na.map, getNearestMean)
elev.fill <- fillMap(elev, na.map, getNearestMean)
areafract.fill <- fillMap(areafract, na.map, getNearestMean)

# Save
dim.band = ncdim_def(name = "snow_band",
                      units = "#",
                      vals = 1:max(nelev, na.rm = T),
                      longname = "Elevation band")
elevation.vars = loadElevationVars(lon.dim, lat.dim, dim.band)

nc = nc_create(vic.out, vars = elevation.vars)
ncvar_put(nc, "Nelev", nelev.fill)
ncvar_put(nc, "elev", elev.fill)
ncvar_put(nc, "elevation", elevation.fill[,,1:max(nelev, na.rm = T)])
ncvar_put(nc, "Pfactor", areafract.fill[,,1:max(nelev, na.rm = T)])
ncvar_put(nc, "AreaFract", areafract.fill[,,1:max(nelev, na.rm = T)])
nc_close(nc)
