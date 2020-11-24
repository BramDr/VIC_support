library(ncdf4)
library(fields)
rm(list = ls())

# Input
function.script <- "../../../../Scripts/Support/mapFunctions.R"
mask.file <- "../../../../Data/Primary/VIC/domain_global.nc"
fert.dir <- "./Saves"
crop.file <- "./Saves/crop_mapping.csv"
out.dir <- "../../../../Data/VIC/Forcing/global/fertilizer_yearly_lowFert/fertilizer_yearly_lowFert_"
years <- 1979:2016

# Load
source(function.script)
crops <- read.csv(crop.file, stringsAsFactors = F)

fert.dvs.file <- list.files(path = fert.dir, pattern = "fertilizerDVS", full.names = T)
fert.n.file <- list.files(path = fert.dir, pattern = "fertilizerN", full.names = T)
fert.p.file <- list.files(path = fert.dir, pattern = "fertilizerP", full.names = T)
fert.k.file <- list.files(path = fert.dir, pattern = "fertilizerK", full.names = T)
fert.dvs <- readRDS(fert.dvs.file)
fert.n <- readRDS(fert.n.file)
fert.p <- readRDS(fert.p.file)
fert.k <- readRDS(fert.k.file)

nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

# Setup
na.map <- is.na(mask) | mask == 0

res <- 0.5
lons <- seq(
  from = -180 + res / 2,
  to = 180 - res / 2,
  by = res
)
lats <- seq(
  from = -90 + res / 2,
  to = 90 - res / 2,
  by = res
)

dim.lon <- ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = lons,
  longname = "longitude of cell centre"
)
dim.lat <- ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = lats,
  longname = "latitude of cell centre"
)
dim.crop <- ncdim_def(
  name = "crop_class",
  units = "#",
  vals = 1:nrow(crops),
  longname = "Crop class"
)
dim.fert <- ncdim_def(
  name = "fertilizer_times",
  units = "#",
  vals = 1,
  longname = "Fertilizer time"
)

# Calculate and save
fert.dvs.fill <- fillMap(fert.dvs, na.map, getNearestZero)
fert.n.fill <- fillMap(fert.n, na.map, getNearestZero)
fert.p.fill <- fillMap(fert.p, na.map, getNearestZero)
fert.k.fill <- fillMap(fert.k, na.map, getNearestZero)

for (z in 1:length(years)) {
  year <- years[z]
  print(year)

  out.file <- paste0(out.dir, year, ".nc")

  times <- as.Date(paste0(year, "-01-01"))

  time.dim <- ncdim_def(
    name = "time",
    units = "days since 1970-01-01",
    vals = as.numeric(times),
    unlim = T,
    calendar = "standard"
  )

  var.DVS_point <- ncvar_def(
    name = "DVS_point",
    units = "-",
    dim = list(dim.lon, dim.lat, dim.crop, dim.fert, time.dim),
    missval = -1,
    longname = "DVS fraction after which fertilizer is applied",
    compression = 9
  )
  var.N_amount <- ncvar_def(
    name = "N_amount",
    units = "kg ha-1",
    dim = list(dim.lon, dim.lat, dim.crop, dim.fert, time.dim),
    missval = -1,
    longname = "N fertilizer amount",
    compression = 9
  )
  var.P_amount <- ncvar_def(
    name = "P_amount",
    units = "kg ha-1",
    dim = list(dim.lon, dim.lat, dim.crop, dim.fert, time.dim),
    missval = -1,
    longname = "P fertilizer amount",
    compression = 9
  )
  var.K_amount <- ncvar_def(
    name = "K_amount",
    units = "kg ha-1",
    dim = list(dim.lon, dim.lat, dim.crop, dim.fert, time.dim),
    missval = -1,
    longname = "K fertilizer amount",
    compression = 9
  )

  dir.create(dirname(out.file))
  nc <- nc_create(out.file, list(
    var.DVS_point,
    var.N_amount,
    var.P_amount,
    var.K_amount
  ))
  for (i in 1:nrow(crops)) {
    if (is.na(crops$fertilizer[i])) {
      next
    }
    print(crops$name[i])

    ncvar_put(nc, var.DVS_point, fert.dvs.fill[, , i, z], start = c(1, 1, i, 1, 1), count = c(-1, -1, 1, -1, -1))
    ncvar_put(nc, var.N_amount, fert.n.fill[, , i, z] * 0.75, start = c(1, 1, i, 1, 1), count = c(-1, -1, 1, -1, -1))
    ncvar_put(nc, var.P_amount, fert.p.fill[, , i, z], start = c(1, 1, i, 1, 1), count = c(-1, -1, 1, -1, -1))
    ncvar_put(nc, var.K_amount, fert.k.fill[, , i, z], start = c(1, 1, i, 1, 1), count = c(-1, -1, 1, -1, -1))
  }
  nc_close(nc)
}
