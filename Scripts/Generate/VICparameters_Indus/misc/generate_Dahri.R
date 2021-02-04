library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
map.support.file <- "../../../../Scripts/Support/mapFunctions.R"
generate.support.file <- "../../../../Scripts/Support/generateFunctions.R"
mask.file <- "../../../../Data/Transformed/Parameters/mask_Dahri_5min_Indus.RDS"

avg.t.file = "../../../../Data/Transformed/Parameters/avgT_Dahri_5min_Indus.RDS"
annual.prec.file = "../../../../Data/Transformed/Parameters/annualPrec_Dahri_5min_Indus.RDS"

vic.orig <- "../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out <- "../../../../Data/VIC/Parameters/Indus_5min/miscellaneous_params_Dahri_Indus.nc"

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

avg.t = readRDS(avg.t.file)
annual.prec = readRDS(annual.prec.file)
run.cell = array(1, dim = dim(avg.t))

# Setup
na.map <- is.na(mask) | mask == 0

# Calculate
avg.t.fill <- fillMap(avg.t, na.map, getNearestMean)
annual.prec.fill <- fillMap(annual.prec, na.map, getNearestMean)
run.cell.fill <- fillMap(run.cell, na.map, getNearestMean)

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

var.list = list()
for(var in misc.vars){
  print(var)
  var.var = cropyCreateVariable(base.file = vic.orig, base.name = var,
                                dim = list(dim.lon, dim.lat), 
                                chunksizes = c(length(out.lons), length(out.lats)))
  var.list[[var]] = var.var
}

nc = nc_create(vic.out, vars = var.list)
nc_close(nc)

# Save
nc <- nc_open(vic.out, write = T)
ncvar_put(nc, "avg_T", avg.t.fill)
ncvar_put(nc, "annual_prec", annual.prec.fill)
ncvar_put(nc, "run_cell", run.cell.fill)
nc_close(nc)
