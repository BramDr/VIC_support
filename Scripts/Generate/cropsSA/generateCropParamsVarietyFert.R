library(ncdf4)
library(fields)
rm(list = ls())

# Input
function.script = "../../Support/mapFunctions.R"
crop.file = "./Saves/crop_mapping_MIRCA.csv"
Cc.dir = "./Saves"
Ncrop.file = "./Saves/Ncrop_MIRCA_30min_global.RDS"
veg.class.file = "./Saves/cropVegClass_MIRCA_30min_global.RDS"
plant.file = "./Saves/plantDay_MIRCA_30min_global.RDS"
harvest.file = "./Saves/harvestDay_MIRCA_30min_global.RDS"
fert.n.file = "./Saves/fertilizerN_30min_global.RDS"
fert.p.file = "./Saves/fertilizerP_30min_global.RDS"
fert.k.file = "./Saves/fertilizerK_30min_global.RDS"
crop.out = "../../../Data/VIC/Parameters/global/WOFOST_SA/crop_params_variety_global.nc"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
Ncrop = readRDS(Ncrop.file)
veg.class = readRDS(veg.class.file)
plant = readRDS(plant.file)
harvest = readRDS(harvest.file)
fert.n = readRDS(fert.n.file)
fert.p = readRDS(fert.p.file)
fert.k = readRDS(fert.k.file)

na.map = Ncrop == 0

Cc.files = list.files(path = Cc.dir, pattern = "Cc_.*_MIRCA_", full.names = T)

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

for(i in 1:nrow(crops)){
  crop.name = crops$mirca.name[i]
  season = crops$season[i]
  
  if(!crop.name %in% c("wheat", "maize", "rice", "soybean")){
    next
  }
      
  Cc.file = grep(x = Cc.files, pattern = paste0("Cc_", i, "_"), value = T)
  print(basename(Cc.file))
  
  Cc.c = readRDS(Cc.file)
  
  Ncrop.c <- fillMap(map = Ncrop[,,i], na.map = na.map[,,i], nearest.function = getNearestZero)
  plant.c <- fillMap(map = plant[,,i], na.map = na.map[,,i], nearest.function = getNearestMean)
  harvest.c <- fillMap(map = harvest[,,i], na.map = na.map[,,i], nearest.function = getNearestMean)
  veg.class.c <- fillMap(map = veg.class[,,i], na.map = na.map[,,i], nearest.function = getNearestCount)
  dvs.c <- fillMap(map = array(0, dim = dim(Ncrop[,,i])), na.map = na.map[,,i], nearest.function = getNearestMean)
  fert.n.c <- fillMap(map = fert.n[,,i], na.map = na.map[,,i], nearest.function = getNearestMean)
  fert.p.c <- fillMap(map = fert.p[,,i], na.map = na.map[,,i], nearest.function = getNearestMean)
  fert.k.c <- fillMap(map = fert.k[,,i], na.map = na.map[,,i], nearest.function = getNearestMean)
  for(z in 1:dim(Cc.c)[3]){
    Cc.c[,,z] <- fillMap(map = Cc.c[,,z], na.map = na.map[,,i], nearest.function = getNearestZero)
  }
    
  for(plant.offset in c(5,15,25)) {
    crop.out.tmp = gsub(x = crop.out, pattern = "params_", replacement = paste0("params_", crop.name, "_", crops$water[i], "_", season, "_", plant.offset, "_"))
    print(basename(crop.out.tmp))
    
    plant.c.adj = plant.c + plant.offset - 1
    plant.c.adj[!is.na(plant.c.adj) & plant.c.adj > 365] = plant.c.adj[!is.na(plant.c.adj) & plant.c.adj > 365] - 365
  
    if(file.exists(crop.out.tmp)){
      print("Done")
      #next
    }
  
    dir.create(dirname(crop.out.tmp))
    nc <- nc_create(
      crop.out.tmp,
      list(
        var.Ncrop,
        var.Cc,
        var.crop_veg_class,
        var.plant_day,
        var.harvest_day,
        var.DVS_point,
        var.N_amount,
        var.P_amount,
        var.K_amount
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
    ncatt_put(
      nc = nc,
      varid = 0,
      attname = "Comment",
      attval = "Fertilizer data based on GGCMI simulations"
    )
    
    ncvar_put(nc = nc, varid = nc$var$Cc, vals = Cc.c, start = c(1,1,1,1), count = c(-1,-1,1,-1))
    ncvar_put(nc = nc, varid = nc$var$Ncrop, vals = Ncrop.c)
    ncvar_put(nc = nc, varid = nc$var$crop_veg_class, vals = veg.class.c)
    ncvar_put(nc = nc, varid = nc$var$plant_day, vals = plant.c.adj)
    ncvar_put(nc = nc, varid = nc$var$harvest_day, vals = harvest.c)
    ncvar_put(nc = nc, varid = nc$var$DVS_point, vals = dvs.c)
    ncvar_put(nc = nc, varid = nc$var$N_amount, vals = fert.n.c)
    ncvar_put(nc = nc, varid = nc$var$P_amount, vals = fert.p.c)
    ncvar_put(nc = nc, varid = nc$var$K_amount, vals = fert.k.c)
    nc_close(nc)
  }
}
