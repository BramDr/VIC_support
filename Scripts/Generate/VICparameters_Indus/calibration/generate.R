library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
map.support.file <- "../../../../Scripts/Support/mapFunctions.R"
generate.support.file <- "../../../../Scripts/Support/generateFunctions.R"
mask.file <- "../../../../Data/Transformed/Routing/distance_5min_indus.RDS"

Ds.file <- "../../../../Data/Transformed/Parameters/Ds_5min_Indus.RDS"
Dsmax.file <- "../../../../Data/Transformed/Parameters/Dsmax_5min_Indus.RDS"
Ws.file <- "../../../../Data/Transformed/Parameters/Ws_5min_Indus.RDS"
c.file <- "../../../../Data/Transformed/Parameters/c_5min_Indus.RDS"
infilt.file <- "../../../../Data/Transformed/Parameters/infilt_5min_Indus.RDS"

vic.orig = "../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out <- "../../../../Data/VIC/Parameters/Indus_5min/calibration_params_Indus.nc"

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
mask <- readRDS(mask.file)

Ds <- readRDS(Ds.file)
Dsmax <- readRDS(Dsmax.file)
Ws <- readRDS(Ws.file)
c <- readRDS(c.file)
infilt <- readRDS(infilt.file)

# Setup
na.map <- is.na(mask) | mask == 0

# Calculate
Ds.fill <- fillMap(Ds, na.map, getNearestMean)
Dsmax.fill <- fillMap(Dsmax, na.map, getNearestMean)
Ws.fill <- fillMap(Ws, na.map, getNearestMean)
c.fill <- fillMap(c, na.map, getNearestMean)
infilt.fill <- fillMap(infilt, na.map, getNearestMean)

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

var.c = cropyCreateVariable(base.file = vic.orig, base.name = "c",
                            dim = list(dim.lon, dim.lat), 
                            chunksizes = c(length(out.lons), length(out.lats)))
var.Ds = cropyCreateVariable(base.file = vic.orig, base.name = "Ds",
                            dim = list(dim.lon, dim.lat), 
                            chunksizes = c(length(out.lons), length(out.lats)))
var.Dsmax = cropyCreateVariable(base.file = vic.orig, base.name = "Dsmax",
                            dim = list(dim.lon, dim.lat), 
                            chunksizes = c(length(out.lons), length(out.lats)))
var.Ws = cropyCreateVariable(base.file = vic.orig, base.name = "Ws",
                            dim = list(dim.lon, dim.lat), 
                            chunksizes = c(length(out.lons), length(out.lats)))
var.infilt = cropyCreateVariable(base.file = vic.orig, base.name = "infilt",
                             dim = list(dim.lon, dim.lat), 
                             chunksizes = c(length(out.lons), length(out.lats)))

dir.create(dirname(vic.out))
nc <- nc_create(
  vic.out,
  list(
    var.c,
    var.Ds,
    var.Dsmax,
    var.Ws,
    var.infilt
  )
)
nc_close(nc)

# Save
nc <- nc_open(vic.out, write = T)
ncvar_put(nc, "Ds", Ds.fill)
ncvar_put(nc, "Dsmax", Dsmax.fill)
ncvar_put(nc, "Ws", Ws.fill)
ncvar_put(nc, "c", c.fill)
ncvar_put(nc, "infilt", infilt.fill)
nc_close(nc)
