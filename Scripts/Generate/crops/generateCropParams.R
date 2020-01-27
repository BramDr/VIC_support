library(ncdf4)
library(fields)
rm(list = ls())

# Input
function.script = "../../Support/mapFunctions.R"
crop.file = "./Saves/crop_mapping_MIRCA.csv"
Cc.dir = "./Saves"
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
Ncrop.file = "./Saves/Ncrop_MIRCA_30min_global.RDS"
veg.class.file = "./Saves/cropVegClass_MIRCA_30min_global.RDS"
plant.file = "./Saves/plantDay_MIRCA_30min_global.RDS"
harvest.file = "./Saves/harvestDay_MIRCA_30min_global.RDS"
tsum1.dir = "./Saves"
tsum2.dir = "./Saves"
crop.out = "../../../Data/VIC/Parameters/global/crop_params_global.nc"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
Ncrop = readRDS(Ncrop.file)
veg.class = readRDS(veg.class.file)
plant = readRDS(plant.file)
harvest = readRDS(harvest.file)

na.map.c = Ncrop == 0

nc = nc_open(mask.file)
na.map = ncvar_get(nc, nc$var$mask)
nc_close(nc)
na.map = is.na(na.map) | na.map != 1
image.plot(na.map)

Cc.files = list.files(path = Cc.dir, pattern = "Cc_.*_MIRCA_", full.names = T)
tsum1.files = list.files(path = tsum1.dir, pattern = "tsum1_.*_single_", full.names = T)
tsum2.files = list.files(path = tsum2.dir, pattern = "tsum2_.*_single_", full.names = T)

# Setup
source(function.script)
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)

vic.id.u = unique(crops$vic.id)
Ncrop = Ncrop[,,1:(dim(Ncrop)[3] - 1)]
veg.class = veg.class[,,1:(dim(veg.class)[3] - 1)]

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
dim.nchar <- ncdim_def(
  name = "nchar",
  units = "#",
  vals = 1:254,
  longname = "Maximum number of string characters"
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

var.Ncrop <- ncvar_def(
  name = "Ncrop",
  units = "#",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "Number of active crop classes",
  compression = 5
)
var.crop_desc <- ncvar_def(
  name = "crop_desc",
  units = "N/A",
  dim = list(dim.nchar, dim.crop),
  longname = "Crop description",
  compression = 5,
  prec = "char"
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

# Calculate
description = rep("", dim.crop$len)
for(i in 1:nrow(crops)){
  description[i] = paste0(crops$mirca.name[i], "_", crops$water[i], "_s", crops$season[i])
}

Cc.veg = array(0, dim = c(dim(Ncrop)[1], dim(Ncrop)[2], length(vic.id.u), 12))
for(i in 1:nrow(crops)) {
  vic.id = crops$vic.id[i]
  vic.idx = which(vic.id.u == vic.id)
  
  Cc.file = grep(x = Cc.files, pattern = paste0("Cc_", i, "_"), value = T)
  print(basename(Cc.file))
  Cc.crop = readRDS(Cc.file)
  
  Cc.veg[,,vic.idx,] = Cc.veg[,,vic.idx,] + Cc.crop
}
image.plot(Cc.veg[,,1,1])
image.plot(Cc.veg[,,dim(Cc.veg)[3],1])

for(i in 1:nrow(crops)){
  print(i)
  
  plant[,,i] <- fillMap(map = plant[,,i], na.map = na.map.c[,,i], nearest.function = getNearestMean)
  harvest[,,i] <- fillMap(map = harvest[,,i], na.map = na.map.c[,,i], nearest.function = getNearestMean)
  veg.class[,,i] <- fillMap(map = veg.class[,,i], na.map = na.map.c[,,i], nearest.function = getNearestCount)
}
Ncrop.sum = apply(X = Ncrop, MARGIN = c(1,2), FUN = sum)
Ncrop.sum <- fillMap(map = Ncrop.sum, na.map = na.map, nearest.function = getNearestZero)
image.plot(Ncrop.sum)

# Save
print(basename(crop.out))
dir.create(dirname(crop.out))
nc <- nc_create(
  crop.out,
  list(
    var.Ncrop,
    var.crop_desc,
    var.Cc,
    var.crop_veg_class,
    var.plant_day,
    var.harvest_day,
    var.TSUM1,
    var.TSUM2
  )
)
nc_close(nc)

nc <- nc_open(crop.out, write = T)
ncatt_put(
  nc = nc,
  varid = 0,
  attname = "Description",
  attval = "Crop parameters for VIC. Created by Bram Droppers"
)
ncvar_put(nc = nc, varid = nc$var$Ncrop, vals = Ncrop.sum)
ncvar_put(nc = nc, varid = nc$var$crop_desc, vals = description)
ncvar_put(nc = nc, varid = nc$var$crop_veg_class, vals = veg.class)
ncvar_put(nc = nc, varid = nc$var$plant_day, vals = plant)
ncvar_put(nc = nc, varid = nc$var$harvest_day, vals = harvest)

for(i in 1:nrow(crops)){
  print(crops$mirca.name[i])
  
  vic.idx = which(vic.id.u == crops$vic.id[i])
      
  Cc.file = grep(x = Cc.files, pattern = paste0("Cc_", i, "_"), value = T)
  print(basename(Cc.file))
  tsum1.file = grep(x = tsum1.files, pattern = paste0("tsum1_", i, "_"), value = T)
  print(basename(tsum1.file))
  tsum2.file = grep(x = tsum2.files, pattern = paste0("tsum2_", i, "_"), value = T)
  print(basename(tsum2.file))
  
  Cc.c = readRDS(Cc.file)
  tsum1.c = readRDS(tsum1.file)
  tsum2.c = readRDS(tsum2.file)
  
  tsum1.c <- fillMap(map = tsum1.c, na.map = na.map.c[,,i], nearest.function = getNearestMean)
  tsum2.c <- fillMap(map = tsum2.c, na.map = na.map.c[,,i], nearest.function = getNearestMean)
  for(z in 1:dim(Cc.c)[3]){
    Cc.c[,,z] <- fillMap(map = Cc.c[,,z] / Cc.veg[,,vic.idx,z], na.map = na.map.c[,,i], nearest.function = getNearestZero)
  }
  #image.plot(Cc.c[,,5])
  
  ncvar_put(nc = nc, varid = nc$var$Cc, vals = Cc.c, start = c(1,1,i,1), count = c(-1,-1,1,-1))
  ncvar_put(nc = nc, varid = nc$var$TSUM1, vals = tsum1.c, start = c(1,1,i), count = c(-1,-1,1))
  ncvar_put(nc = nc, varid = nc$var$TSUM2, vals = tsum2.c, start = c(1,1,i), count = c(-1,-1,1))
  print("done")
}
nc_close(nc)
