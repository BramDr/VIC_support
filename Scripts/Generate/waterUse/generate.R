library(ncdf4)
library(fields)
rm(list = ls())

# Input
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
pumping.file <- "../../../Data/Transformed/Pumping/pumpingCapacity_30min_global.RDS"
receiving.id.file <- "Saves/receiving_id.RDS"
nreceiving.file <- "Saves/Nreceiving.RDS"
receiving.file <- "Saves/receiving.RDS"
wu.out <- "../../../Data/VIC/Parameters/global/wu_params_global.nc"

# Load
nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

pumping <- readRDS(pumping.file)
receiving <- readRDS(receiving.file)
receiving.id <- readRDS(receiving.id.file)
Nreceiving <- readRDS(nreceiving.file)

lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
nwureceiving <- 1:dim(receiving)[3]

# Setup
years <- 1960:2015
pumping <- pumping[, , which(years == 2000)]
pumping[is.na(mask)] <- NA

# Create
dim.lon <- ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = lons,
  longname = "longitude of grid cell center"
)
dim.lat <- ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = lats,
  longname = "latitude of grid cell center"
)
dim.receiving <- ncdim_def(
  name = "wu_receiving",
  units = "#",
  vals = nwureceiving,
  longname = "Dam class"
)

var.pumping_capacity <- ncvar_def(
  name = "pumping_capacity",
  units = "mm day-1",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "Pumping capacity",
  compression = 9
)
var.Nreceiving <- ncvar_def(
  name = "Nreceiving",
  units = "#",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "Number of receiving cells",
  compression = 9
)
var.receiving <- ncvar_def(
  name = "receiving",
  units = "#",
  dim = list(dim.lon, dim.lat, dim.receiving),
  missval = -1,
  longname = "Receiving cell ID",
  compression = 9
)
var.receiving_id <- ncvar_def(
  name = "receiving_id",
  units = "years AD",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "ID used to identify receiving cell",
  compression = 9
)

nc <- nc_create(
  wu.out,
  list(
    var.pumping_capacity,
    var.Nreceiving,
    var.receiving,
    var.receiving_id
  )
)
nc_close(nc)

# Write
dir.create(dirname(wu.out))
nc <- nc_open(wu.out, write = T)
ncatt_put(
  nc = nc,
  varid = 0,
  attname = "Description",
  attval = "Water-use parameters for VIC. Created by Bram Droppers"
)

ncvar_put(nc, var.pumping_capacity, pumping)
ncvar_put(nc, var.Nreceiving, Nreceiving)
ncvar_put(nc, var.receiving, receiving)
ncvar_put(nc, var.receiving_id, receiving.id)

nc_close(nc)
