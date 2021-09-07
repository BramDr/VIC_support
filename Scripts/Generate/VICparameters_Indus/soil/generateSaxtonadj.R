library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
map.support.file <- "../../../../Scripts/Support/mapFunctions.R"
generate.support.file <- "../../../../Scripts/Support/generateFunctions.R"
mask.file <- "../../../../Data/Transformed/Routing/mask_5min_Indus.RDS"

cv.file <- "../vegetation/Saves/Cv_5min_Indus.RDS"

quartz.file <- "../../../../Data/Transformed/Soil/ISRIC/quartz_5min_Indus.RDS"
expt.file <- "../../../../Data/Transformed/Soil/ISRIC/expt_5min_Indus.RDS"
ksat.file <- "../../../../Data/Transformed/Soil/ISRIC/ksat_5min_Indus.RDS"
bulk.dens.file <- "../../../../Data/Transformed/Soil/ISRIC/bulk_dens_5min_Indus.RDS"
Wfc.file <- "../../../../Data/Transformed/Soil/ISRIC/Wfc_5min_Indus.RDS"
Wcr.file <- "../../../../Data/Transformed/Soil/ISRIC/Wcr_5min_Indus.RDS"
Wpwp.file <- "../../../../Data/Transformed/Soil/ISRIC/Wpwp_5min_Indus.RDS"
bubble.file <- "../../../../Data/Transformed/Soil/ISRIC/bubble_5min_Indus.RDS"
max.moist.file <- "../../../../Data/Transformed/Soil/ISRIC/max_moist_5min_Indus.RDS"
resid.moist.file <- "../../../../Data/Transformed/Soil/ISRIC/resid_moist_5min_Indus.RDS"
depth.file <- "../../../../Data/Transformed/Soil/Pelletier2016/depth_5min_Indus.RDS"

vic.orig = "../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out <- "../../../../Data/VIC/Parameters/Indus_5min/soil_params_Saxton_Indus.nc"
toplayer.depth = 0.3
crop.rootlayer.maxdepth = 1.5
veg.rootlayer.maxdepth = 3

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

cv <- readRDS(cv.file)

quartz <- readRDS(quartz.file)
expt <- readRDS(expt.file)
ksat <- readRDS(ksat.file)
bulk.dens <- readRDS(bulk.dens.file)
Wfc <- readRDS(Wfc.file)
Wcr <- readRDS(Wcr.file)
Wpwp <- readRDS(Wpwp.file)
bubble <- readRDS(bubble.file)
max.moist <- readRDS(max.moist.file)
resid.moist <- readRDS(resid.moist.file)
depth = readRDS(depth.file)

Nlayer = array(3, dim = dim(depth))
fs.active = array(0, dim = dim(depth))
dp = array(4, dim = dim(depth))
rough = array(0.001, dim = dim(depth))
snow.rough = array(0.0005, dim = dim(depth))

# Setup
na.map <- is.na(mask) | mask == 0

cv.sum = apply(X = cv[,,c(1:11, 13)], MARGIN = c(1,2), FUN = sum)
crop.mask = cv[,,12] > 0.5
#image.plot(cv.sum)
#image.plot(crop.mask)

depth.tmp = depth - toplayer.depth
depth.tmp[depth.tmp < toplayer.depth] = toplayer.depth
depth.tmp[crop.mask & depth.tmp > (crop.rootlayer.maxdepth - toplayer.depth)] = crop.rootlayer.maxdepth - toplayer.depth
depth.tmp[!crop.mask & depth.tmp > (veg.rootlayer.maxdepth - toplayer.depth)] = veg.rootlayer.maxdepth - toplayer.depth
image.plot(depth.tmp)

depth = array(NA, dim = c(dim(depth.tmp), 3))
depth[, , 1] <- toplayer.depth
depth[, , 2] <- depth.tmp
depth[, , 3] <- 1000 / (max.moist[,,3] * 1000)

# Calculate
Nlayer.fill <- fillMap(Nlayer, na.map, getNearestZero)
quartz.fill <- fillMap(quartz, na.map, getNearestMean)
expt.fill <- fillMap(expt, na.map, getNearestMean)
ksat.fill <- fillMap(ksat, na.map, getNearestMean)
bulk.dens.fill <- fillMap(bulk.dens, na.map, getNearestMean)
Wfc.fill <- fillMap(Wfc, na.map, getNearestMean)
Wcr.fill <- fillMap(Wcr, na.map, getNearestMean)
Wpwp.fill <- fillMap(Wpwp, na.map, getNearestMean)
bubble.fill <- fillMap(bubble, na.map, getNearestMean)
max.moist.fill <- fillMap(max.moist, na.map, getNearestMean)
resid.moist.fill <- fillMap(resid.moist, na.map, getNearestMean)
depth.fill <- fillMap(depth, na.map, getNearestMean)

fs.active.fill <- fillMap(fs.active, na.map, getNearestMean)
dp.fill <- fillMap(dp, na.map, getNearestMean)
rough.fill <- fillMap(rough, na.map, getNearestMean)
snow.rough.fill <- fillMap(snow.rough, na.map, getNearestMean)

# Set third layer depth to contain 100 mm
depth.fill[, , 3] <- 1000 / max.moist.fill[,,3] * 1e-3

# Calculate misc
init.moist.fill <- Wcr.fill * max.moist.fill * depth.fill * 1e3
soil.dens.fill <- bulk.dens.fill / (1 - max.moist.fill)

# Correct fractions
# sel = !is.na(Wpwp.fill) & Wpwp.fill < resid.moist.fill / max.moist
# Wpwp.fill[sel] <- resid.moist.fill[sel] / max.moist + 1e-6
# sel = !is.na(Wcr.fill) & Wcr.fill < Wpwp.fill
# Wcr.fill[sel] <- Wpwp.fill[sel] + 1e-6
# sel = !is.na(Wfc.fill) & Wfc.fill < Wcr.fill
# Wfc.fill[sel] <- Wcr.fill[sel] + 1e-6

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
var.list[["Wfc_FRACT"]] = 
  Nlayer.var <- ncvar_def(name = "Wfc_FRACT", 
                          units = "fraction", 
                          dim = list(dim.lon, dim.lat, dim.layer), 
                          longname = "Fractional soil moisture content at field capacity (fraction of maximum moisture)", 
                          prec = "double", missval = -1)
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
ncvar_put(nc, "Wfc_FRACT", Wfc.fill)
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

