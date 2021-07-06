library(fields)
library(ncdf4)
library(abind)
rm(list = ls())

# Input
id.file = "./Saves/idMap.RDS"
gdam.file <- "Saves/globalDamsMerge.csv"
ldam.file <- "Saves/localDamsMerge.csv"
gdam.service.file <- "Saves/globalDamsService.RDS"
ldam.service.file <- "Saves/localDamsService.RDS"
gdam.service.frac.file <- "Saves/globalDamsServiceFraction.RDS"
ldam.service.frac.file <- "Saves/localDamsServiceFraction.RDS"
mask.file <- "../../../Data/Transformed/Routing/mask_5min_indus.RDS"
dams.out <- "../../../Data/VIC/Parameters/Indus_5min/dam_params_Grand_period_Indus.nc"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
mask <- readRDS(mask.file)

id.map = readRDS(id.file)
ldams <- read.csv(ldam.file, stringsAsFactors = F)
gdams <- read.csv(gdam.file, stringsAsFactors = F)
ldams.service <- readRDS(ldam.service.file)
gdams.service <- readRDS(gdam.service.file)
ldams.service.frac <- readRDS(ldam.service.frac.file)
gdams.service.frac <- readRDS(gdam.service.frac.file)

# Setup
ndamtypes <- nrow(gdams) + nrow(ldams)
for(i in 1:nrow(gdams)){
  gdams$MODEL_AREA_FRAC[i] = min(gdams$MODEL_AREA_FRAC[i], 1)
}
for(i in 1:nrow(ldams)){
  ldams$MODEL_AREA_FRAC[i] = min(ldams$MODEL_AREA_FRAC[i], 1)
}

## Dam info
type = c(rep(1, nrow(gdams)), rep(0, nrow(ldams)))
year = rep(1800, nrow(gdams) + nrow(ldams))
cap = c(gdams$CAP_MCM, ldams$CAP_MCM)
inflow.frac = c(gdams$MODEL_AREA_FRAC, ldams$MODEL_AREA_FRAC)
nservice = c(apply(X = gdams.service, MARGIN = 3, FUN = function(x){sum(!is.na(x))}), apply(X = ldams.service, MARGIN = 3, FUN = function(x){sum(!is.na(x))}))
id = c()
for(i in 1:nrow(gdams)){
  id = c(id, id.map[gdams$MODEL_X[i], gdams$MODEL_Y[i]])
}
for(i in 1:nrow(ldams)){
  id = c(id, id.map[ldams$MODEL_X[i], ldams$MODEL_Y[i]])
}

service = abind(gdams.service, ldams.service, along = 3)
service.frac = abind(gdams.service.frac, ldams.service.frac, along = 3)

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
dim.dam <- ncdim_def(
  name = "dam_class",
  units = "#",
  vals = 1:ndamtypes,
  longname = "Dam class"
)

var.id.map <- ncvar_def(
  name = "id_map",
  units = "ID",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "ID used to identify dam cell",
  prec = "integer",
  compression = 9
)
var.id <- ncvar_def(
  name = "id",
  units = "ID",
  dim = list(dim.dam),
  missval = -1,
  longname = "ID used to identify dam cell",
  prec = "integer",
  compression = 9
)
var.type <- ncvar_def(
  name = "type",
  units = "global/local",
  dim = list(dim.dam),
  missval = -1,
  longname = "1 = global, 0 = local", 
  prec = "integer",
  compression = 9
)
var.year <- ncvar_def(
  name = "year",
  units = "years AD",
  dim = list(dim.dam),
  missval = -1,
  longname = "Building year of dam",
  prec = "integer",
  compression = 9
)
var.capacity <- ncvar_def(
  name = "capacity",
  units = "hm3",
  dim = list(dim.dam),
  missval = -1,
  longname = "Capacity of dam",
  compression = 9
)
var.inflow.frac <- ncvar_def(
  name = "inflow_fraction",
  units = "-",
  dim = list(dim.dam),
  missval = -1,
  longname = "Fraction of inflow going to the dam reservoir",
  compression = 9
)
var.nservice <- ncvar_def(
  name = "Nservice",
  units = "#",
  dim = list(dim.dam),
  missval = -1,
  longname = "Numer of service cells for dam",
  prec = "integer",
  compression = 9
)

var.service <- ncvar_def(
  name = "service",
  units = "ID",
  dim = list(dim.lon, dim.lat, dim.dam),
  missval = -1,
  longname = "Service cell ID for dam",
  prec = "integer",
  compression = 9
)
var.serve.fac <- ncvar_def(
  name = "service_fraction",
  units = "ID",
  dim = list(dim.lon, dim.lat, dim.dam),
  missval = -1,
  longname = "Fraction of demand for service cell of dam",
  compression = 9
)

dir.create(dirname(dams.out))
nc <- nc_create(
  dams.out,
  list(
    var.id.map,
    var.id,
    var.type,
    var.year,
    var.capacity,
    var.inflow.frac,
    var.nservice,
    var.service,
    var.serve.fac
  )
)
nc_close(nc)

# Save
nc <- nc_open(dams.out, write = T)
ncatt_put(
  nc = nc,
  varid = 0,
  attname = "Description",
  attval = "Dam parameters for VIC. Created by Bram Droppers"
)

ncvar_put(nc, var.id.map, id.map)
ncvar_put(nc, var.id, id)
ncvar_put(nc, var.type, type)
ncvar_put(nc, var.year, year)
ncvar_put(nc, var.capacity, cap)
ncvar_put(nc, var.inflow.frac, inflow.frac)
ncvar_put(nc, var.nservice, nservice)

ncvar_put(nc, var.service, service)
ncvar_put(nc, var.serve.fac, service.frac)

nc_close(nc)

