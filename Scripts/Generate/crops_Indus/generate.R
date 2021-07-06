library(ncdf4)
library(fields)
rm(list = ls())

# Input
map.support.file <- "../../Support/mapFunctions.R"
generate.support.file <- "../../Support/generateFunctions.R"
mask.file <- "../../../Data/Transformed/Routing/mask_5min_indus.RDS"

crop.file <- "./Saves/crop_mapping.RDS"
cc.file <- "./Saves/season_fraction_crops_monthly_5min_Indus.RDS"
Ncrop.file <- "./Saves/Ncrop_crops_5min_Indus.RDS"
veg.class.file <- "./Saves/vegclass_crops_30min_global.RDS"
plant.file <- "./Saves/plantday_crops_5min_Indus.RDS"
harvest.file <- "./Saves/harvestday_crops_5min_Indus.RDS"
tsum1.file <- "./Saves/tsum1_crops_5min_Indus.RDS"
tsum2.file <- "./Saves/tsum2_crops_5min_Indus.RDS"
fert.dvs.file <- "./Saves/fertilizerDVS_5min_Indus.RDS"
fert.n.file <- "./Saves/fertilizerN_5min_Indus.RDS"
fert.p.file <- "./Saves/fertilizerP_5min_Indus.RDS"
fert.k.file <- "./Saves/fertilizerK_5min_Indus.RDS"
carbon.file <- "./Saves/carbon_crops_5min_Indus.RDS"
ph.file <- "./Saves/ph_crops_5min_Indus.RDS"

crop.out <- "../../../Data/VIC/Parameters/Indus_5min/crop_params_Mirca_Indus.nc"

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

crops <- readRDS(crop.file)
cc <- readRDS(cc.file)
Ncrop <- readRDS(Ncrop.file)
veg.class <- readRDS(veg.class.file)
plant <- readRDS(plant.file)
harvest <- readRDS(harvest.file)
tsum1 <- readRDS(tsum1.file)
tsum2 <- readRDS(tsum2.file)
fert.dvs <- readRDS(fert.dvs.file)
fert.n <- readRDS(fert.n.file)
fert.p <- readRDS(fert.p.file)
fert.k <- readRDS(fert.k.file)
carbon <- readRDS(carbon.file)
ph <- readRDS(ph.file)

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
dim.crop <- ncdim_def(
  name = "crop_class",
  units = "#",
  vals = 1:nrow(crops),
  longname = "Crop class"
)
dim.month <- ncdim_def(
  name = "month",
  units = "month of year",
  vals = 1:12,
  longname = "month of year (1-12)"
)
dim.fert <- ncdim_def(
  name = "fertilizer_times",
  units = "#",
  vals = 1:4,
  longname = "Fertilizer time"
)

var.Ncrop <- ncvar_def(
  name = "Ncrop",
  units = "#",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "Number of active crop classes",
  compression = 5
)
var.crop_veg_class <- ncvar_def(
  name = "crop_veg_class",
  units = "#",
  dim = list(dim.lon, dim.lat, dim.crop),
  missval = -1,
  longname = "Crop vegetation class",
  compression = 5
)
var.Cc <- ncvar_def(
  name = "Cc",
  units = "fraction",
  dim = list(dim.lon, dim.lat, dim.crop, dim.month),
  missval = -1,
  longname = "Crop coverage per month",
  compression = 5
)
var.plant_day <- ncvar_def(
  name = "plant_day",
  units = "day of year",
  dim = list(dim.lon, dim.lat, dim.crop),
  missval = -1,
  longname = "Day of year the crop is planted",
  compression = 5
)
var.harvest_day <- ncvar_def(
  name = "harvest_day",
  units = "day of year",
  dim = list(dim.lon, dim.lat, dim.crop),
  missval = -1,
  longname = "Day of year the crop is harvested",
  compression = 5
)
var.TSUM1 <- ncvar_def(
  name = "TSUM1",
  units = "K",
  dim = list(dim.lon, dim.lat, dim.crop),
  missval = -1,
  longname = "Daily temperature sum from emergence to anthesis",
  compression = 5
)
var.TSUM2 <- ncvar_def(
  name = "TSUM2",
  units = "K",
  dim = list(dim.lon, dim.lat, dim.crop),
  missval = -1,
  longname = "Daily temperature sum from anthesis to maturity",
  compression = 5
)
var.Tfactor <- ncvar_def(
  name = "Tfactor",
  units = "K",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "Temperature factor due to elevation",
  compression = 5
)
var.DVS_point <- ncvar_def(
  name = "DVS_point",
  units = "-",
  dim = list(dim.lon, dim.lat, dim.crop, dim.fert),
  missval = -1,
  longname = "DVS fraction after which fertilizer is applied",
  compression = 5
)
var.N_amount <- ncvar_def(
  name = "N_amount",
  units = "kg ha-1",
  dim = list(dim.lon, dim.lat, dim.crop, dim.fert),
  missval = -1,
  longname = "N fertilizer amount",
  compression = 5
)
var.P_amount <- ncvar_def(
  name = "P_amount",
  units = "kg ha-1",
  dim = list(dim.lon, dim.lat, dim.crop, dim.fert),
  missval = -1,
  longname = "P fertilizer amount",
  compression = 5
)
var.K_amount <- ncvar_def(
  name = "K_amount",
  units = "kg ha-1",
  dim = list(dim.lon, dim.lat, dim.crop, dim.fert),
  missval = -1,
  longname = "K fertilizer amount",
  compression = 5
)
var.carbon <- ncvar_def(
  name = "carbon",
  units = "g kg-1",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "Organic carbon content for top 20 cm of soil",
  compression = 5
)
var.ph <- ncvar_def(
  name = "pH",
  units = "-",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "pH for top 20 cm of soil",
  compression = 5
)
var.mineralization_period <- ncvar_def(
  name = "mineralization_period",
  units = "days",
  dim = list(dim.lon, dim.lat, dim.crop),
  missval = -1,
  longname = "mineralization reference period",
  compression = 5
)

dir.create(dirname(crop.out))
nc <- nc_create(
  crop.out,
  list(
    var.Ncrop,
    var.Cc,
    var.crop_veg_class,
    var.plant_day,
    var.harvest_day,
    var.TSUM1,
    var.TSUM2,
    var.Tfactor,
    var.DVS_point,
    var.N_amount,
    var.P_amount,
    var.K_amount,
    var.carbon,
    var.ph,
    var.mineralization_period
  )
)
nc_close(nc)

nc <- nc_open(crop.out, write = T)

na.map <- is.na(mask) | mask == 0
# image.plot(na.map)

## Calculate
Ncrop.filled <- fillMap(map = Ncrop, na.map = na.map, nearest.function = getNearestZero)
#tfactor.filled <- fillMap(map = tfactor, na.map = na.map, nearest.function = getNearestZero)
carbon.filled <- fillMap(map = carbon, na.map = na.map, nearest.function = getNearestMean)
ph.filled <- fillMap(map = ph, na.map = na.map, nearest.function = getNearestMean)

ncvar_put(nc = nc, varid = var.Ncrop, vals = Ncrop.filled)
#ncvar_put(nc = nc, varid = var.Tfactor, vals = tfactor.filled)
ncvar_put(nc = nc, varid = var.carbon, vals = carbon.filled)
ncvar_put(nc = nc, varid = var.ph, vals = ph.filled)

i <- 1
for (i in 1:nrow(crops)) {
  print(crops$name[i])

  cc.max <- apply(X = cc[, , i, ], MARGIN = c(1, 2), FUN = max)
  na.map <- is.na(mask) | mask == 0 | is.na(cc.max) | cc.max == 0
  # image.plot(na.map)

  ## Calculate
  cc.filled <- fillMap(map = cc[, , i, ], na.map = na.map, nearest.function = getNearestZero)
  veg.class.filled <- fillMap(map = veg.class[, , i], na.map = na.map, nearest.function = getNearestCount)
  plant.filled <- fillMap(map = plant[, , i], na.map = na.map, nearest.function = getNearestMean)
  harvest.filled <- fillMap(map = harvest[, , i], na.map = na.map, nearest.function = getNearestMean)
  tsum1.filled <- fillMap(map = tsum1[, , i], na.map = na.map, nearest.function = getNearestMean)
  tsum2.filled <- fillMap(map = tsum2[, , i], na.map = na.map, nearest.function = getNearestMean)
  fert.dvs.filled <- fillMap(map = fert.dvs[, , i, ], na.map = na.map, nearest.function = getNearestMean)
  fert.n.filled <- fillMap(map = fert.n[, , i, ], na.map = na.map, nearest.function = getNearestMean)
  fert.p.filled <- fillMap(map = fert.p[, , i, ], na.map = na.map, nearest.function = getNearestMean)
  fert.k.filled <- fillMap(map = fert.k[, , i, ], na.map = na.map, nearest.function = getNearestMean)
  period.filled <- fillMap(map = tsum1.filled * 0 + 120, na.map = na.map, nearest.function = getNearestMean)

  ncvar_put(nc = nc, varid = var.Cc, vals = cc.filled, start = c(1, 1, i, 1), count = c(-1, -1, 1, -1))
  ncvar_put(nc = nc, varid = var.crop_veg_class, vals = veg.class.filled, start = c(1, 1, i), count = c(-1, -1, 1))
  ncvar_put(nc = nc, varid = var.plant_day, vals = plant.filled, start = c(1, 1, i), count = c(-1, -1, 1))
  ncvar_put(nc = nc, varid = var.harvest_day, vals = harvest.filled, start = c(1, 1, i), count = c(-1, -1, 1))
  ncvar_put(nc = nc, varid = var.TSUM1, vals = tsum1.filled, start = c(1, 1, i), count = c(-1, -1, 1))
  ncvar_put(nc = nc, varid = var.TSUM2, vals = tsum2.filled, start = c(1, 1, i), count = c(-1, -1, 1))
  ncvar_put(nc = nc, varid = var.DVS_point, vals = fert.dvs.filled, start = c(1, 1, i, 1), count = c(-1, -1, 1, -1))
  ncvar_put(nc = nc, varid = var.N_amount, vals = fert.n.filled, start = c(1, 1, i, 1), count = c(-1, -1, 1, -1))
  ncvar_put(nc = nc, varid = var.P_amount, vals = fert.p.filled, start = c(1, 1, i, 1), count = c(-1, -1, 1, -1))
  ncvar_put(nc = nc, varid = var.K_amount, vals = fert.k.filled, start = c(1, 1, i, 1), count = c(-1, -1, 1, -1))
  ncvar_put(nc = nc, varid = var.mineralization_period, vals = period.filled, start = c(1, 1, i), count = c(-1, -1, 1))
}

ncatt_put(
  nc = nc,
  varid = 0,
  attname = "Description",
  attval = "Crop parameters for VIC. Created by Bram Droppers"
)
nc_close(nc)
