library(fields)
library(ncdf4)

rm(list = ls())

# Input
downstream.file <- "../../../Data/Transformed/Routing/downstream_30min_global.RDS"
uh.river.file <- "Saves/UH_river.RDS"
uh.grid.file <- "Saves/UH_grid.RDS"
rout.param.out <- "../../../Data/VIC/Parameters/global/rout_params_global.nc"

# Load
uh.river <- readRDS(uh.river.file)
uh.grid <- readRDS(uh.grid.file)
downstream <- readRDS(downstream.file)

# Setup
time <- (1:dim(uh.grid)[3] - 1) * 3600
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
lons <- seq(from = -179.75, to = 179.75, by = 0.5)

downstream.id <- array(NA, dim = c(dim(downstream)[1], dim(downstream)[2]))
id.counter <- 1
for (x in 1:dim(downstream)[1]) {
  for (y in 1:dim(downstream)[2]) {
    if (is.na(downstream[x, y, 1])) {
      next
    }
    downstream.id[x, y] <- id.counter
    id.counter <- id.counter + 1
  }
}
image.plot(downstream.id)

downstream.nc <- array(NA, dim = dim(downstream.id))
for (x in 1:dim(downstream)[1]) {
  for (y in 1:dim(downstream)[2]) {
    if (is.na(downstream[x, y, 1])) {
      next
    }
    downstream.cell <- downstream[x, y, ]
    downstream.nc[x, y] <- downstream.id[downstream.cell[1], downstream.cell[2]]
  }
}
image.plot(downstream.nc)

# Save
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
dim.time <- ncdim_def(
  name = "time",
  units = "seconds",
  vals = time,
  longname = "Time in the unit hydrograph"
)

var.downstream.id <- ncvar_def(
  name = "downstream_id",
  units = "ID",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "ID used to identify downstream cell",
  compression = 9
)
var.downstream <- ncvar_def(
  name = "downstream",
  units = "ID",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "ID of the downstream cell",
  compression = 9
)
var.uh.river <- ncvar_def(
  name = "uh_inflow",
  units = "[-]",
  dim = list(dim.lon, dim.lat, dim.time),
  missval = -1,
  longname = "Unit hydrograph of the river (river)",
  compression = 9
)
var.uh.grid <- ncvar_def(
  name = "uh_runoff",
  units = "[-]",
  dim = list(dim.lon, dim.lat, dim.time),
  missval = -1,
  longname = "Unit hydrograph of the grid (cell)",
  compression = 9
)

dir.create(dirname(rout.param.out))
nc <- nc_create(
  rout.param.out,
  list(
    var.downstream.id,
    var.downstream,
    var.uh.river,
    var.uh.grid
  )
)
nc_close(nc)

nc <- nc_open(rout.param.out, write = T)
ncatt_put(
  nc = nc,
  varid = 0,
  attname = "Description",
  attval = "Routing parameters for VIC. Created by Bram Droppers"
)
ncvar_put(nc, nc$var$downstream_id, downstream.id)
ncvar_put(nc, nc$var$downstream, downstream.nc)
ncatt_put(
  nc = nc,
  varid = nc$var$downstream,
  attname = "Description",
  attval = "Dowstream cells generated based on DDM30"
)
ncvar_put(nc, nc$var$uh_inflow, uh.river)
ncatt_put(
  nc = nc,
  varid = nc$var$downstream,
  attname = "Description",
  attval = paste0(
    "Unit hydrograph generated based on Lohmann et al (1996) with flow velocity ",
    1,
    " [m s-1] and diffusion ",
    2000,
    " [m s-2]"
  )
)
ncvar_put(nc, nc$var$uh_runoff, uh.grid)
ncatt_put(
  nc = nc,
  varid = nc$var$downstream,
  attname = "Description",
  attval = paste0(
    "Unit hydrograph generated based on SCS dimensionless unith hydrograph"
  )
)
nc_close(nc)
