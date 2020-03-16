library(ncdf4)
library(fields)
rm(list = ls())

# Input
function.script = "../../Support/mapFunctions.R"
mask.file = "../../../Data/Primary/VIC/domain_global.nc"
crop.file = "./Saves/crop_mapping_single.csv"
Cc.dir = "./Saves"
Ncrop.file = "./Saves/Ncrop_single_30min_global.RDS"
veg.class.file = "./Saves/cropVegClass_single_30min_global.RDS"
plant.file = "./Saves/plantDay_SAGE_30min_global.RDS"
harvest.file = "./Saves/harvestDay_SAGE_30min_global.RDS"
tsum1.dir = "./Saves"
tsum2.dir = "./Saves"
crop.out = "../../../Data/VIC/Parameters/global/WOFOST_SA/crop_params_single_global.nc"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
Ncrop = readRDS(Ncrop.file)
veg.class = readRDS(veg.class.file)
plant = readRDS(plant.file)
harvest = readRDS(harvest.file)

nc = nc_open(mask.file)
na.map = ncvar_get(nc, "mask")
nc_close(nc)
na.map = is.na(na.map) | na.map != 1
image.plot(na.map)

Cc.files = list.files(path = Cc.dir, pattern = "Cc_.*_single_", full.names = T)
tsum1.files = list.files(path = tsum1.dir, pattern = "tsum1_.*_single_", full.names = T)
tsum2.files = list.files(path = tsum2.dir, pattern = "tsum2_.*_single_", full.names = T)

# Setup
source(function.script)
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)

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

for(i in 1:nrow(crops)){
  print(crops$mirca.name[i])
  crop.name = crops$mirca.name[i]
  season = crops$season[i]
      
  Cc.file = grep(x = Cc.files, pattern = paste0("Cc_", i, "_"), value = T)
  print(basename(Cc.file))
  tsum1.file = grep(x = tsum1.files, pattern = paste0("tsum1_", i, "_"), value = T)
  print(basename(tsum1.file))
  tsum2.file = grep(x = tsum2.files, pattern = paste0("tsum2_", i, "_"), value = T)
  print(basename(tsum2.file))
  
  Cc.c = readRDS(Cc.file)
  tsum1 = readRDS(tsum1.file)
  tsum2 = readRDS(tsum2.file)
  
  Ncrop.c <- fillMap(map = Ncrop[,,i], na.map = na.map, nearest.function = getNearestZero)
  plant.c <- fillMap(map = plant[,,i], na.map = na.map, nearest.function = getNearestMean)
  harvest.c <- fillMap(map = harvest[,,i], na.map = na.map, nearest.function = getNearestMean)
  veg.class.c <- fillMap(map = veg.class[,,i], na.map = na.map, nearest.function = getNearestCount)
  tsum1.c <- fillMap(map = tsum1, na.map = na.map, nearest.function = getNearestMean)
  tsum2.c <- fillMap(map = tsum2, na.map = na.map, nearest.function = getNearestMean)
  for(z in 1:dim(Cc.c)[3]){
    Cc.c[,,z] <- fillMap(map = Cc.c[,,z], na.map = na.map, nearest.function = getNearestZero)
  }
    
  crop.out.tmp = gsub(x = crop.out, pattern = "params_", replacement = paste0("params_", crop.name, "_"))
  print(basename(crop.out.tmp))

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
      var.TSUM2
    )
  )
  nc_close(nc)
  
  # Save
  nc <- nc_open(crop.out.tmp, write = T)
  ncatt_put(
    nc = nc,
    varid = 0,
    attname = "Description",
    attval = "Crop parameters for VIC. Created by Bram Droppers"
  )
  
  ncvar_put(nc = nc, varid = nc$var$Cc, vals = Cc.c, start = c(1,1,1,1), count = c(-1,-1,1,-1))
  ncvar_put(nc = nc, varid = nc$var$Ncrop, vals = Ncrop.c)
  ncvar_put(nc = nc, varid = nc$var$crop_veg_class, vals = veg.class.c)
  ncvar_put(nc = nc, varid = nc$var$plant_day, vals = plant.c)
  ncvar_put(nc = nc, varid = nc$var$harvest_day, vals = harvest.c)
  ncvar_put(nc = nc, varid = nc$var$TSUM1, vals = tsum1.c)
  ncvar_put(nc = nc, varid = nc$var$TSUM2, vals = tsum2.c)
  nc_close(nc)
}
