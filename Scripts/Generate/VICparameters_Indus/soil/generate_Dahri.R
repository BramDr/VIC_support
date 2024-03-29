library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
map.support.file <- "../../../../Scripts/Support/mapFunctions.R"
generate.support.file <- "../../../../Scripts/Support/generateFunctions.R"
mask.file <- "../../../../Data/Transformed/Parameters/mask_Dahri_5min_Indus.RDS"

quartz.file <- "../../../../Data/Transformed/Parameters/soilQuartz_Dahri_5min_Indus.RDS"
expt.file <- "../../../../Data/Transformed/Parameters/soilExpt_Dahri_5min_Indus.RDS"
ksat.file <- "../../../../Data/Transformed/Parameters/soilKsat_Dahri_5min_Indus.RDS"
bulk.dens.file <- "../../../../Data/Transformed/Parameters/soilBulkDensity_Dahri_5min_Indus.RDS"
Wcr.file <- "../../../../Data/Transformed/Parameters/soilWcr_Dahri_5min_Indus.RDS"
Wpwp.file <- "../../../../Data/Transformed/Parameters/soilWp_Dahri_5min_Indus.RDS"
bubble.file <- "../../../../Data/Transformed/Parameters/soilBubble_Dahri_5min_Indus.RDS"
soil.dens.file <- "../../../../Data/Transformed/Parameters/soilSoilDensity_Dahri_5min_Indus.RDS"
resid.moist.file <- "../../../../Data/Transformed/Parameters/soilResidual_Dahri_5min_Indus.RDS"
init.moist.file <- "../../../../Data/Transformed/Parameters/soilInitMoist_Dahri_5min_Indus.RDS"
depth.file <- "../../../../Data/Transformed/Parameters/depth_Dahri_5min_Indus.RDS"
dp.file <- "../../../../Data/Transformed/Parameters/soilDp_Dahri_5min_Indus.RDS"
fs.active.file <- "../../../../Data/Transformed/Parameters/soilFrozenFlag_Dahri_5min_Indus.RDS"
rough.file <- "../../../../Data/Transformed/Parameters/soilSurfaceRoughness_Dahri_5min_Indus.RDS"
snow.rough.file <- "../../../../Data/Transformed/Parameters/soilSnowRoughness_Dahri_5min_Indus.RDS"

vic.orig = "../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out <- "../../../../Data/VIC/Parameters/Indus_5min/soil_params_Dahri_Indus.nc"

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

quartz <- readRDS(quartz.file)
expt <- readRDS(expt.file)
ksat <- readRDS(ksat.file)
bulk.dens <- readRDS(bulk.dens.file)
Wcr <- readRDS(Wcr.file)
Wpwp <- readRDS(Wpwp.file)
bubble <- readRDS(bubble.file)
soil.dens <- readRDS(soil.dens.file)
resid.moist <- readRDS(resid.moist.file)
init.moist <- readRDS(init.moist.file)
depth = readRDS(depth.file)
fs.active = readRDS(fs.active.file)
dp = readRDS(dp.file)
rough = readRDS(rough.file)
snow.rough = readRDS(snow.rough.file)

Nlayer = array(3, dim = dim(depth)[1:2])

# Setup
na.map <- is.na(mask) | mask == 0

# Calculate
Nlayer.fill <- fillMap(Nlayer, na.map, getNearestZero)
quartz.fill <- fillMap(quartz, na.map, getNearestMean)
expt.fill <- fillMap(expt, na.map, getNearestMean)
ksat.fill <- fillMap(ksat, na.map, getNearestMean)
bulk.dens.fill <- fillMap(bulk.dens, na.map, getNearestMean)
Wcr.fill <- fillMap(Wcr, na.map, getNearestMean)
Wpwp.fill <- fillMap(Wpwp, na.map, getNearestMean)
bubble.fill <- fillMap(bubble, na.map, getNearestMean)
soil.dens.fill <- fillMap(soil.dens, na.map, getNearestMean)
resid.moist.fill <- fillMap(resid.moist, na.map, getNearestMean)
init.moist.fill <- fillMap(init.moist, na.map, getNearestMean)
depth.fill <- fillMap(depth, na.map, getNearestMean)

fs.active.fill <- fillMap(fs.active, na.map, getNearestMean)
dp.fill <- fillMap(dp, na.map, getNearestMean)
rough.fill <- fillMap(rough, na.map, getNearestMean)
snow.rough.fill <- fillMap(snow.rough, na.map, getNearestMean)

#porosity <- 1 - bulk.dens.fill / soil.dens.fill
#Wpwp.fill[!is.na(Wpwp.fill) & Wpwp.fill < resid.moist.fill / porosity] <- resid.moist.fill[!is.na(Wpwp.fill) & Wpwp.fill < resid.moist.fill / porosity] / porosity - 1e-6
#Wcr.fill[!is.na(Wcr.fill) & Wcr.fill < Wpwp.fill] <- Wpwp.fill[!is.na(Wcr.fill) & Wcr.fill < Wpwp.fill] + 1e-6

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

nc = nc_open(vic.orig)
dim.layer = nc$dim$nlayer
nc_close(nc)

var.list = list()
for(var in c(soil.vars, "depth")){
  print(var)
  if(var %in% c("fs_active", "dp", "rough", "snow_rough")){
    var.var = cropyCreateVariable(base.file = vic.orig, base.name = var,
                                   dim = list(dim.lon, dim.lat), 
                                   chunksizes = c(length(out.lons), length(out.lats)))
  } else {
    var.var = cropyCreateVariable(base.file = vic.orig, base.name = var,
                                  dim = list(dim.lon, dim.lat, dim.layer), 
                                  chunksizes = c(length(out.lons), length(out.lats), 1))
  }
  
  var.list[[var]] = var.var
}
var.list[["Nlayer"]] = 
  Nlayer.var <- ncvar_def(name = "Nlayer", 
                          units = "#", 
                          dim = list(dim.lon, dim.lat), 
                          longname = "Number of active layers", 
                          prec = "integer", missval = -1)

nc = nc_create(vic.out, vars = var.list)
nc_close(nc)

# Save
nc <- nc_open(vic.out, write = T)
ncvar_put(nc, "Nlayer", Nlayer.fill)
ncvar_put(nc, "quartz", quartz.fill)
ncvar_put(nc, "expt", expt.fill)
ncvar_put(nc, "Ksat", ksat.fill)
ncvar_put(nc, "bulk_density", bulk.dens.fill)
ncvar_put(nc, "Wcr_FRACT", Wcr.fill)
ncvar_put(nc, "Wpwp_FRACT", Wpwp.fill)
ncvar_put(nc, "bubble", bubble.fill)
ncvar_put(nc, "soil_density", soil.dens.fill)
ncvar_put(nc, "resid_moist", resid.moist.fill)
ncvar_put(nc, "init_moist", init.moist.fill)
ncvar_put(nc, "fs_active", fs.active.fill)
ncvar_put(nc, "dp", dp.fill)
ncvar_put(nc, "rough", rough.fill)
ncvar_put(nc, "snow_rough", snow.rough.fill)
ncvar_put(nc, "depth", depth.fill)
nc_close(nc)
