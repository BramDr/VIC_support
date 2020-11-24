library(ncdf4)
library(fields)
rm(list = ls())

# Input
function.script <- "../../../../Scripts/Support/mapFunctions.R"
mask.file <- "../../../../Data/Primary/VIC/domain_global.nc"
crop.file <- "./Saves/crop_mapping.csv"
cc.file <- "./Saves/cc_30min_global.RDS"
Ncrop.file <- "./Saves/Ncrop_30min_global.RDS"
veg.class.file <- "./Saves/cropVegClass_30min_global.RDS"
plant.file <- "./Saves/plantDay_30min_global.RDS"
harvest.file <- "./Saves/harvestDay_30min_global.RDS"
tsum1.file <- "./Saves/tsum1_30min_global.RDS"
tsum2.file <- "./Saves/tsum2_30min_global.RDS"
tfactor.file <- "./Saves/tfactor_30min_global.RDS"
fert.dvs.file <- "./Saves/fertilizerDVS_30min_global.RDS"
fert.n.file <- "./Saves/fertilizerN_30min_global.RDS"
fert.p.file <- "./Saves/fertilizerP_30min_global.RDS"
fert.k.file <- "./Saves/fertilizerK_30min_global.RDS"
min.n.file <- "./Saves/mineralizationN_30min_global.RDS"
min.p.file <- "./Saves/mineralizationP_30min_global.RDS"
min.k.file <- "./Saves/mineralizationK_30min_global.RDS"
rec.n.file <- "./Saves/recoveryN_30min_global.RDS"
rec.p.file <- "./Saves/recoveryP_30min_global.RDS"
rec.k.file <- "./Saves/recoveryK_30min_global.RDS"
crop.out <- "../../../../Data/VIC/Parameters/global/VICWOFOST_SA/crop_params_global.nc"

# Load
crops <- read.csv(crop.file, stringsAsFactors = F)
cc <- readRDS(cc.file)
Ncrop <- readRDS(Ncrop.file)
veg.class <- readRDS(veg.class.file)
plant <- readRDS(plant.file)
harvest <- readRDS(harvest.file)
tsum1 <- readRDS(tsum1.file)
tsum2 <- readRDS(tsum2.file)
tfactor <- readRDS(tfactor.file)
fert.dvs <- readRDS(fert.dvs.file)
fert.n <- readRDS(fert.n.file)
fert.p <- readRDS(fert.p.file)
fert.k <- readRDS(fert.k.file)
min.n <- readRDS(min.n.file)
min.p <- readRDS(min.p.file)
min.k <- readRDS(min.k.file)
rec.n <- readRDS(rec.n.file)
rec.p <- readRDS(rec.p.file)
rec.k <- readRDS(rec.k.file)

nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

# Setup
source(function.script)
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
noptions <- 16

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
dim.crop <- ncdim_def(
  name = "crop_class",
  units = "#",
  vals = 1,
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
  vals = 1,
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
var.N_mins <- ncvar_def(
  name = "N_mins",
  units = "kg ha-1",
  dim = list(dim.lon, dim.lat, dim.crop),
  missval = -1,
  longname = "N amount for mineralization",
  compression = 5
)
var.N_recovery <- ncvar_def(
  name = "N_recovery",
  units = "kg kg-1",
  dim = list(dim.lon, dim.lat, dim.crop),
  missval = -1,
  longname = "N mineralization recovery rate",
  compression = 5
)
var.P_mins <- ncvar_def(
  name = "P_mins",
  units = "kg ha-1",
  dim = list(dim.lon, dim.lat, dim.crop),
  missval = -1,
  longname = "N amount for mineralization",
  compression = 5
)
var.P_recovery <- ncvar_def(
  name = "P_recovery",
  units = "kg kg-1",
  dim = list(dim.lon, dim.lat, dim.crop),
  missval = -1,
  longname = "N mineralization recovery rate",
  compression = 5
)
var.K_mins <- ncvar_def(
  name = "K_mins",
  units = "kg ha-1",
  dim = list(dim.lon, dim.lat, dim.crop),
  missval = -1,
  longname = "N amount for mineralization",
  compression = 5
)
var.K_recovery <- ncvar_def(
  name = "K_recovery",
  units = "kg kg-1",
  dim = list(dim.lon, dim.lat, dim.crop),
  missval = -1,
  longname = "N mineralization recovery rate",
  compression = 5
)

na.map <- apply(X = Ncrop, MARGIN = c(1, 2), FUN = sum, na.rm = T)
na.map <- is.na(na.map) | na.map == 0
# image.plot(na.map)

## Calculate
tfactor.filled <- fillMap(map = tfactor, na.map = na.map, nearest.function = getNearestZero)

i <- 1
for (i in 1:nrow(crops)) {
  print(crops$name[i])

  # if (crops$name[i] != "maize" || crops$water[i] != "irrigated") {
  # next
  # }

  na.map <- is.na(Ncrop[, , i]) | Ncrop[, , i] == 0
  # image.plot(na.map)

  ## Calculate
  Ncrop.filled <- fillMap(map = Ncrop[, , i], na.map = na.map, nearest.function = getNearestZero)
  fert.dvs.filled <- fillMap(map = fert.dvs[, , i], na.map = na.map, nearest.function = getNearestMean)
  fert.n.filled <- fillMap(map = fert.n[, , i], na.map = na.map, nearest.function = getNearestMean)
  fert.p.filled <- fillMap(map = fert.p[, , i], na.map = na.map, nearest.function = getNearestMean)
  fert.k.filled <- fillMap(map = fert.k[, , i], na.map = na.map, nearest.function = getNearestMean)
  min.n.filled <- fillMap(map = min.n[, , i], na.map = na.map, nearest.function = getNearestMean)
  min.p.filled <- fillMap(map = min.p[, , i], na.map = na.map, nearest.function = getNearestMean)
  min.k.filled <- fillMap(map = min.k[, , i], na.map = na.map, nearest.function = getNearestMean)
  rec.n.filled <- fillMap(map = rec.n[, , i], na.map = na.map, nearest.function = getNearestMean)
  rec.p.filled <- fillMap(map = rec.p[, , i], na.map = na.map, nearest.function = getNearestMean)
  rec.k.filled <- fillMap(map = rec.k[, , i], na.map = na.map, nearest.function = getNearestMean)
  cc.filled <- fillMap(map = cc[, , i, ], na.map = na.map, nearest.function = getNearestZero)
  veg.class.filled <- fillMap(map = veg.class[, , i], na.map = na.map, nearest.function = getNearestCount)

  z <- 1
  for (z in 1:noptions) {
    print(z)

    # if (z != 5) {
    # next
    # }

    crop.out.tmp <- gsub(x = crop.out, pattern = "crop_params_", replacement = paste0("crop_params_", crops$name[i], "_", crops$water[i], "_", crops$season[i], "_", z, "_"))
    dir.create(dirname(crop.out.tmp))
    nc <- nc_create(
      crop.out.tmp,
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
        var.N_mins,
        var.N_recovery,
        var.P_mins,
        var.P_recovery,
        var.K_mins,
        var.K_recovery
      )
    )
    nc_close(nc)

    ## Calculate
    plant.filled <- fillMap(map = plant[, , i, z], na.map = na.map, nearest.function = getNearestMean)
    harvest.filled <- fillMap(map = harvest[, , i, z], na.map = na.map, nearest.function = getNearestMean)
    tsum1.filled <- fillMap(map = tsum1[, , i, z], na.map = na.map, nearest.function = getNearestMean)
    tsum2.filled <- fillMap(map = tsum2[, , i, z], na.map = na.map, nearest.function = getNearestMean)

    nc <- nc_open(crop.out.tmp, write = T)
    ncvar_put(nc = nc, varid = var.Ncrop, vals = Ncrop.filled)
    ncvar_put(nc = nc, varid = var.Cc, vals = cc.filled)
    ncvar_put(nc = nc, varid = var.crop_veg_class, vals = veg.class.filled)
    ncvar_put(nc = nc, varid = var.plant_day, vals = plant.filled)
    ncvar_put(nc = nc, varid = var.harvest_day, vals = harvest.filled)
    ncvar_put(nc = nc, varid = var.TSUM1, vals = tsum1.filled)
    ncvar_put(nc = nc, varid = var.TSUM2, vals = tsum2.filled)
    ncvar_put(nc = nc, varid = var.Tfactor, vals = tfactor.filled)
    ncvar_put(nc = nc, varid = var.DVS_point, vals = fert.dvs.filled)
    ncvar_put(nc = nc, varid = var.N_amount, vals = fert.n.filled)
    ncvar_put(nc = nc, varid = var.P_amount, vals = fert.p.filled)
    ncvar_put(nc = nc, varid = var.K_amount, vals = fert.k.filled)
    ncvar_put(nc = nc, varid = var.N_mins, vals = min.n.filled)
    ncvar_put(nc = nc, varid = var.N_recovery, vals = rec.n.filled)
    ncvar_put(nc = nc, varid = var.P_mins, vals = min.p.filled)
    ncvar_put(nc = nc, varid = var.P_recovery, vals = rec.p.filled)
    ncvar_put(nc = nc, varid = var.K_mins, vals = min.k.filled)
    ncvar_put(nc = nc, varid = var.K_recovery, vals = rec.k.filled)

    ncatt_put(
      nc = nc,
      varid = 0,
      attname = "Description",
      attval = "Crop parameters for VIC. Created by Bram Droppers"
    )
    nc_close(nc)
  }
}
