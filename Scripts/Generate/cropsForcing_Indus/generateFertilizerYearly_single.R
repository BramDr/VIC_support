library(ncdf4)
library(fields)
rm(list = ls())

# Input
function.script <- "../../../Scripts/Support/mapFunctions.R"
mask.file <- "../../../Data/Transformed/Routing/mask_5min_indus.RDS"
cc.file <- "./Saves/season_fraction_crops_monthly_5min_Indus.RDS"
fert.dir <- "./Saves"
crop.file <- "./Saves/crop_mapping.RDS"
out.dir <- "../../../../Data/VIC/Forcing/Indus_5min/single/fertilizer_yearly/fertilizer_yearly_"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
source(function.script)
crops <- readRDS(crop.file)
cc <- readRDS(cc.file)
mask <- readRDS(mask.file)

fert.dvs.file <- list.files(path = fert.dir, pattern = "fertilizerDVS_5min", full.names = T)
fert.n.file <- list.files(path = fert.dir, pattern = "fertilizerN_5min", full.names = T)
fert.p.file <- list.files(path = fert.dir, pattern = "fertilizerP_5min", full.names = T)
fert.k.file <- list.files(path = fert.dir, pattern = "fertilizerK_5min", full.names = T)
fert.dvs <- readRDS(fert.dvs.file)
fert.n <- readRDS(fert.n.file)
fert.p <- readRDS(fert.p.file)
fert.k <- readRDS(fert.k.file)

years = dimnames(fert.dvs)[[3]]

# Setup
dim.lon <- ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = out.lons,
  longname = "longitude of cell centre"
)
dim.lat <- ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = out.lats,
  longname = "latitude of cell centre"
)
dim.crop <- ncdim_def(
  name = "crop_class",
  units = "#",
  vals = 1,
  longname = "Crop class"
)
dim.fert <- ncdim_def(
  name = "fertilizer_times",
  units = "#",
  vals = 1:4,
  longname = "Fertilizer time"
)

# Calculate and save
i <- 1
for (i in 1:nrow(crops)) {
  if (is.na(crops$fertilizer[i])) {
    next
  }
  
  print(crops$name[i])
  
  cc.max <- apply(X = cc[, , i, ], MARGIN = c(1, 2), FUN = max)
  na.map <- is.na(mask) | mask == 0 | is.na(cc.max) | cc.max == 0
  # image.plot(na.map)
  
  z = 1
  for (z in 1:length(years)) {
    year <- years[z]
    print(year)

    out.file <- paste0(out.dir, year, ".nc")
    out.file <- gsub(x = out.file, pattern = "fertilizer_", replacement = paste0("fertilizer_", crops$name[i], "_", crops$water[i], "_", crops$season[i], "_"))

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
    
    fert.dvs.fill <- fillMap(fert.dvs[,,z,i,], na.map, getNearestZero)
    fert.n.fill <- fillMap(fert.n[,,z,i,], na.map, getNearestZero)
    fert.p.fill <- fillMap(fert.p[,,z,i,], na.map, getNearestZero)
    fert.k.fill <- fillMap(fert.k[,,z,i,], na.map, getNearestZero)
    
    ncvar_put(nc, var.DVS_point, fert.dvs.fill)
    ncvar_put(nc, var.N_amount, fert.n.fill)
    ncvar_put(nc, var.P_amount, fert.p.fill)
    ncvar_put(nc, var.K_amount, fert.k.fill)
    
    nc_close(nc)
  }
}
