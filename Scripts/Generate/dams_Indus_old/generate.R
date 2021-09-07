library(fields)
library(ncdf4)
rm(list = ls())

# Input
gdam.file <- "Saves/globalDamsMerge.csv"
ldam.file <- "Saves/localDamsMerge.csv"
gdam.service.file <- "Saves/globalDamsService.RDS"
ldam.service.file <- "Saves/localDamsService.RDS"
mask.file <- "../../../Data/Transformed/Routing/mask_5min_indus.RDS"
dams.out <- "../../../Data/VIC/Parameters/Indus_5min/dam_params_Grand_Indus.nc"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
mask <- readRDS(mask.file)

ldams <- read.csv(ldam.file, stringsAsFactors = F)
gdams <- read.csv(gdam.file, stringsAsFactors = F)
ldams.service <- readRDS(ldam.service.file)
gdams.service <- readRDS(gdam.service.file)

## Ndams
nldams <- array(0, dim = dim(mask))
ngdams <- array(0, dim = dim(mask))
for (i in 1:nrow(ldams)) {
  x <- ldams$MODEL_X[i]
  y <- ldams$MODEL_Y[i]
  
  nldams[x, y] <- nldams[x, y] + 1
}
for (i in 1:nrow(gdams)) {
  x <- gdams$MODEL_X[i]
  y <- gdams$MODEL_Y[i]
  
  ngdams[x, y] <- ngdams[x, y] + 1
}
image.plot(nldams)
image.plot(ngdams)

## Dam info
ndamtypes <- max(c(nldams), c(ngdams), 1, na.rm = T)
lyear <- array(0, dim = c(dim(mask)[1:2], ndamtypes))
lcap <- array(0, dim = c(dim(mask)[1:2], ndamtypes))
linflow.frac <- array(0, dim = c(dim(mask)[1:2], ndamtypes))
gyear <- array(0, dim = c(dim(mask)[1:2], ndamtypes))
gcap <- array(0, dim = c(dim(mask)[1:2], ndamtypes))
ginflow.frac <- array(0, dim = c(dim(mask)[1:2], ndamtypes))
for (i in 1:nrow(ldams)) {
  x <- ldams$MODEL_X[i]
  y <- ldams$MODEL_Y[i]
  z <- 1
  
  lyear[x, y, z] <- ldams$YEAR[i]
  lcap[x, y, z] <- ldams$CAP_MCM[i]
  linflow.frac[x, y, z] <- ldams$MODEL_AREA_FRAC[i]
}
for (i in 1:nrow(gdams)) {
  x <- gdams$MODEL_X[i]
  y <- gdams$MODEL_Y[i]
  z <- 1

  gyear[x, y, z] <- gdams$YEAR[i]
  gcap[x, y, z] <- gdams$CAP_MCM[i]
  ginflow.frac[x, y, z] <- gdams$MODEL_AREA_FRAC[i]
}
image.plot(lyear[, , 1])
image.plot(lcap[, , 1])
image.plot(linflow.frac[, , 1])
image.plot(gyear[, , 1])
image.plot(gcap[, , 1])
image.plot(ginflow.frac[, , 1])

# Ndamservice
nlservice <- array(0, dim = c(dim(mask)[1:2], ndamtypes))
ngservice <- array(0, dim = c(dim(mask)[1:2], ndamtypes))
for (i in 1:length(ldams.service)) {
  x <- ldams$MODEL_X[i]
  y <- ldams$MODEL_Y[i]
  z <- 1

  nlservice[x, y, z] <- nlservice[x, y, z] + nrow(ldams.service[[i]])
}
for (i in 1:length(gdams.service)) {
  x <- gdams$MODEL_X[i]
  y <- gdams$MODEL_Y[i]
  z <- 1

  ngservice[x, y, z] <- ngservice[x, y, z] + nrow(gdams.service[[i]])
}
image.plot(nlservice[, , 1])
image.plot(ngservice[, , 1])

# Service id
service.id <- array(NA, dim = dim(mask))
id.counter <- 1
for (x in 1:dim(service.id)[1]) {
  for (y in 1:dim(service.id)[2]) {
    if (is.na(mask[x, y])) {
      next
    }

    service.id[x, y] <- id.counter
    id.counter <- id.counter + 1
  }
}
image.plot(service.id)

# Dam service info
ndamservice <- max(c(nlservice), c(ngservice), 1, na.rm = T)
lservice <- array(0, dim = c(dim(mask)[1:2], ndamservice, ndamtypes))
gservice <- array(0, dim = c(dim(mask)[1:2], ndamservice, ndamtypes))
lservice.frac <- array(0, dim = c(dim(mask)[1:2], ndamservice, ndamtypes))
gservice.frac <- array(0, dim = c(dim(mask)[1:2], ndamservice, ndamtypes))
for (i in 1:length(ldams.service)) {
  x <- ldams$MODEL_X[i]
  y <- ldams$MODEL_Y[i]
  z <- 1

  if (is.na(mask[x, y])) {
    next
  }

  if (nrow(ldams.service[[i]]) <= 0) {
    next
  }

  for (j in 1:nrow(ldams.service[[i]])) {
    s.x <- ldams.service[[i]]$X[j]
    s.y <- ldams.service[[i]]$Y[j]
    s.frac <- ldams.service[[i]]$FRAC[j]
    s.id <- service.id[s.x, s.y]
    lservice[x, y, j, z] <- s.id
    lservice.frac[x, y, j, z] <- s.frac
  }
}
for (i in 1:length(gdams.service)) {
  x <- gdams$MODEL_X[i]
  y <- gdams$MODEL_Y[i]
  z <- 1

  if (is.na(mask[x, y])) {
    next
  }

  if (nrow(gdams.service[[i]]) <= 0) {
    next
  }

  for (j in 1:nrow(gdams.service[[i]])) {
    s.x <- gdams.service[[i]]$X[j]
    s.y <- gdams.service[[i]]$Y[j]
    s.frac <- gdams.service[[i]]$FRAC[j]
    s.id <- service.id[s.x, s.y]
    gservice[x, y, j, z] <- s.id
    gservice.frac[x, y, j, z] <- s.frac
  }
}
image.plot(lservice[, , 1, 1])
image.plot(lservice.frac[, , 1, 1])
image.plot(gservice[, , 1, 1])
image.plot(gservice.frac[, , 1, 1])

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
dim.service <- ncdim_def(
  name = "dam_service",
  units = "#",
  vals = 1:ndamservice,
  longname = "Dam service class"
)

var.nldam <- ncvar_def(
  name = "Ndam_local",
  units = "#",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "Number of local dams in the grid cell",
  compression = 9
)
var.ngdam <- ncvar_def(
  name = "Ndam_global",
  units = "#",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "Number of global dams in the grid cell",
  compression = 9
)
var.lyear <- ncvar_def(
  name = "year_local",
  units = "years AD",
  dim = list(dim.lon, dim.lat, dim.dam),
  missval = -1,
  longname = "Building year of local dam",
  compression = 9
)
var.gyear <- ncvar_def(
  name = "year_global",
  units = "years AD",
  dim = list(dim.lon, dim.lat, dim.dam),
  missval = -1,
  longname = "Building year of global dam",
  compression = 9
)
var.lcapacity <- ncvar_def(
  name = "capacity_local",
  units = "hm3",
  dim = list(dim.lon, dim.lat, dim.dam),
  missval = -1,
  longname = "Capacity of local dam",
  compression = 9
)
var.gcapacity <- ncvar_def(
  name = "capacity_global",
  units = "hm3",
  dim = list(dim.lon, dim.lat, dim.dam),
  missval = -1,
  longname = "Capacity of global dam",
  compression = 9
)
var.linflow.frac <- ncvar_def(
  name = "inflow_fraction_local",
  units = "-",
  dim = list(dim.lon, dim.lat, dim.dam),
  missval = -1,
  longname = "Fraction of inflow going to the local dam reservoir",
  compression = 9
)
var.ginflow.frac <- ncvar_def(
  name = "inflow_fraction_global",
  units = "-",
  dim = list(dim.lon, dim.lat, dim.dam),
  missval = -1,
  longname = "Fraction of inflow going to the global dam reservoir",
  compression = 9
)

var.service.id <- ncvar_def(
  name = "service_id",
  units = "ID",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "ID used to identify servicing cell",
  compression = 9
)
var.nlservice <- ncvar_def(
  name = "Nservice_local",
  units = "#",
  dim = list(dim.lon, dim.lat, dim.dam),
  missval = -1,
  longname = "Numer of service cells for local dam",
  compression = 9
)
var.ngservice <- ncvar_def(
  name = "Nservice_global",
  units = "#",
  dim = list(dim.lon, dim.lat, dim.dam),
  missval = -1,
  longname = "Numer of service cells for global dam",
  compression = 9
)
var.lservice <- ncvar_def(
  name = "service_local",
  units = "ID",
  dim = list(dim.lon, dim.lat, dim.service, dim.dam),
  missval = -1,
  longname = "Service cell ID for local dam",
  compression = 9
)
var.gservice <- ncvar_def(
  name = "service_global",
  units = "ID",
  dim = list(dim.lon, dim.lat, dim.service, dim.dam),
  missval = -1,
  longname = "Service cell ID for global dam",
  compression = 9
)
var.lserve.fac <- ncvar_def(
  name = "service_fraction_local",
  units = "ID",
  dim = list(dim.lon, dim.lat, dim.service, dim.dam),
  missval = -1,
  longname = "Fraction of demand for service cell of local dam",
  compression = 9
)
var.gserve.fac <- ncvar_def(
  name = "service_fraction_global",
  units = "ID",
  dim = list(dim.lon, dim.lat, dim.service, dim.dam),
  missval = -1,
  longname = "Fraction of demand for service cell of globak dam",
  compression = 9
)

dir.create(dirname(dams.out))
nc <- nc_create(
  dams.out,
  list(
    var.service.id,
    var.nldam,
    var.lyear,
    var.lcapacity,
    var.linflow.frac,
    var.nlservice,
    var.lservice,
    var.lserve.fac,
    var.ngdam,
    var.gyear,
    var.gcapacity,
    var.ginflow.frac,
    var.ngservice,
    var.gservice,
    var.gserve.fac
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

ncvar_put(nc, var.service.id, service.id)

ncvar_put(nc, var.nldam, nldams)
ncvar_put(nc, var.lyear, lyear)
ncvar_put(nc, var.lcapacity, lcap)
ncvar_put(nc, var.linflow.frac, linflow.frac)
ncvar_put(nc, var.nlservice, nlservice)
ncvar_put(nc, var.lservice, lservice)
ncvar_put(nc, var.lserve.fac, lservice.frac)

ncvar_put(nc, var.ngdam, ngdams)
ncvar_put(nc, var.gyear, gyear)
ncvar_put(nc, var.ginflow.frac, ginflow.frac)
ncvar_put(nc, var.gcapacity, gcap)
ncvar_put(nc, var.ngservice, ngservice)
ncvar_put(nc, var.gservice, gservice)
ncvar_put(nc, var.gserve.fac, gservice.frac)

nc_close(nc)

