library(ncdf4)
rm(list = ls())

# Input
crop.file = "./Saves/crop_mapping.csv"
Cc.dir = "./saves"
Ncrop.file = "./saves/Ncrop_30min_global.RDS"
veg.class.file = "./saves/cropVegClass_30min_global.RDS"
plant.file = "./Saves/plantDay_30min_global.RDS"
harvest.file = "./Saves/harvestDay_30min_global.RDS"
tsum1.file = "./Saves/tsum1_30min_global.RDS"
tsum2.file = "./Saves/tsum2_30min_global.RDS"
crop.out = "../../../Data/VIC/Parameters/global/crop_params_global.nc"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
Cc.files = list.files(path = Cc.dir, pattern = "Cc_", full.names = T)
Ncrop = readRDS(Ncrop.file)
veg.class = readRDS(veg.class.file)
plant = readRDS(plant.file)
harvest = readRDS(harvest.file)
#tsum1 = readRDS(tsum1.file)
#tsum2 = readRDS(tsum2.file)
tsum1 = array(10, dim = dim(veg.class))
tsum2 = array(10, dim = dim(veg.class))

# Setup
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
  vals = 1:dim(veg.class)[3],
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
    var.TSUM2
  )
)
nc_close(nc)

# Save
nc <- nc_open(crop.out, write = T)
ncatt_put(
  nc = nc,
  varid = 0,
  attname = "Description",
  attval = "Crop parameters for VIC. Created by Bram Droppers"
)

for(i in 1:dim(veg.class)[3]) {
  Cc.file = grep(x = Cc.files, pattern = paste0("Cc_", i, "_"), value = T)
  print(basename(Cc.file))
  
  Cc = readRDS(Cc.file)
  ncvar_put(nc = nc, varid = nc$var$Cc, vals = Cc, start = c(1,1,i,1), count = c(-1,-1,1,-1))
}
ncvar_put(nc = nc, varid = nc$var$Ncrop, vals = Ncrop)
ncvar_put(nc = nc, varid = nc$var$crop_veg_class, vals = veg.class)
ncvar_put(nc = nc, varid = nc$var$plant_day, vals = plant)
ncvar_put(nc = nc, varid = nc$var$harvest_day, vals = harvest)
ncvar_put(nc = nc, varid = nc$var$TSUM1, vals = tsum1)
ncvar_put(nc = nc, varid = nc$var$TSUM2, vals = tsum2)
nc_close(nc)
