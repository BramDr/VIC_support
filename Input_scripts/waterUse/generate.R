library(ncdf4)
library(fields)
rm(list = ls())

# Input
receiving.id.file = "Saves/receiving_id.RDS"
nreceiving.file = "Saves/Nreceiving.RDS"
receiving.file = "Saves/receiving.RDS"
wu.out = "Output/wu_params_global.nc"

# Load
receiving = readRDS(receiving.file)
receiving.id = readRDS(receiving.id.file)
Nreceiving = readRDS(nreceiving.file)

lons = seq(from = -179.75, to = 179.75, by = 0.5)
lats = seq(from = -89.75, to = 89.75, by = 0.5)
nwureceiving = 1:dim(receiving)[3]

# Create
dim.lon = ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = lons,
  longname = "longitude of grid cell center"
)
dim.lat = ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = lats,
  longname = "latitude of grid cell center"
)
dim.receiving = ncdim_def(
  name = "wu_receiving",
  units = "#",
  vals = nwureceiving,
  longname = "Dam class"
)

var.Nreceiving = ncvar_def(
  name = "Nreceiving",
  units = "#",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "Number of receiving cells",
  compression = 9
)
var.receiving = ncvar_def(
  name = "receiving",
  units = "#",
  dim = list(dim.lon, dim.lat, dim.receiving),
  missval = -1,
  longname = "Receiving cell ID",
  compression = 9
)
var.receiving_id = ncvar_def(
  name = "receiving_id",
  units = "years AD",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "ID used to identify receiving cell",
  compression = 9
)

nc = nc_create(
  wu.out,
  list(
    var.Nreceiving,
    var.receiving,
    var.receiving_id
  )
)
nc_close(nc)

# Write
nc = nc_open(wu.out, write = T)
ncatt_put(
  nc = nc,
  varid = 0,
  attname = "Description",
  attval = "Water-use parameters for VIC. Created by Bram Droppers"
)

ncvar_put(nc, var.Nreceiving, Nreceiving)
ncvar_put(nc, var.receiving, receiving)
ncvar_put(nc, var.receiving_id, receiving.id)

nc_close(nc)
