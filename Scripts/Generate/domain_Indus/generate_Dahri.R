library(ncdf4)
library(fields)
rm(list = ls())

# Input
map.support.file <- "../../../Scripts/Support/mapFunctions.R"
generate.support.file <- "../../../Scripts/Support/generateFunctions.R"
path.area <- "../../../Data/Transformed/Routing/area_5min_Indus.RDS"
path.mask <- "../../../Data/Transformed/Parameters/mask_Dahri_5min_Indus.RDS"
vic.orig = "../../../../Data/Primary/VIC/domain_global.nc"
domain.out <- "../../../Data/VIC/Parameters/Indus_5min/domain_Dahri_Indus.nc"

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

area <- readRDS(path.area)
mask <- readRDS(path.mask)

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

var.area = cropyCreateVariable(base.file = vic.orig, base.name = "area",
                                dim = list(dim.lon, dim.lat), 
                                chunksizes = c(length(out.lons), length(out.lats)))
var.mask = cropyCreateVariable(base.file = vic.orig, base.name = "mask",
                                dim = list(dim.lon, dim.lat), 
                                chunksizes = c(length(out.lons), length(out.lats)))
var.frac = cropyCreateVariable(base.file = vic.orig, base.name = "frac",
                                dim = list(dim.lon, dim.lat), 
                                chunksizes = c(length(out.lons), length(out.lats)))

# Save
dir.create(dirname(domain.out))
nc <- nc_create(
  domain.out,
  list(
    var.area,
    var.mask,
    var.frac
  )
)
ncvar_put(nc, var.area, area)
ncvar_put(nc, var.mask, mask)
ncvar_put(nc, var.frac, mask)
nc_close(nc)
