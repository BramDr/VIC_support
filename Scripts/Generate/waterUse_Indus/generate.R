library(ncdf4)
library(fields)
rm(list = ls())

# Input
mask.file <- "../../../Data/Transformed/Routing/mask_5min_Indus.RDS"
receiving.id.file <- "Saves/idMap.RDS"
receiving.1.file <- "Saves/receivingCommandExtended.RDS"
receiving.2.file <- "Saves/receivingDelta.RDS"
wu.out <- "../../../Data/VIC/Parameters/Indus_5min/wu_params_command_Indus.nc"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
mask = readRDS(mask.file)
receiving.1 <- readRDS(receiving.1.file)
receiving.2 <- readRDS(receiving.2.file)
receiving.id <- readRDS(receiving.id.file)

# Setup
receiving = array(NA, dim = c(dim(receiving.1)[1:2], dim(receiving.1)[3] + 1))
receiving[,,1:dim(receiving.1)[3]] = receiving.1
receiving[,,dim(receiving)[3]] = receiving.2
nwureceiving <- 1:dim(receiving)[3]

Nreceiving = array(0, dim = dim(receiving.id))
for(x in 1:dim(Nreceiving)[1]){
  for(y in 1:dim(Nreceiving)[2]){
    Nreceiving[x,y] = sum(receiving.1 == receiving.id[x,y], na.rm = T) + sum(receiving.2 == receiving.id[x,y], na.rm = T)
  }
}
image.plot(Nreceiving)

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
dim.receiving <- ncdim_def(
  name = "wu_receiving",
  units = "#",
  vals = nwureceiving,
  longname = "Dam class"
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
  longname = "Source cell ID",
  compression = 9
)
var.receiving_id <- ncvar_def(
  name = "receiving_id",
  units = "#",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "ID used to identify source cell",
  compression = 9
)

nc <- nc_create(
  wu.out,
  list(
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

ncvar_put(nc, var.Nreceiving, Nreceiving)
ncvar_put(nc, var.receiving, receiving)
ncvar_put(nc, var.receiving_id, receiving.id)

nc_close(nc)
