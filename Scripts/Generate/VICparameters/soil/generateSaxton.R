library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
map.support.file <- "../../../../Scripts/Support/mapFunctions.R"
generate.support.file <- "../../../../Scripts/Support/generateFunctions.R"
mask.file <- "../../../../Data/Primary/VIC/domain_global.nc"

quartz.file <- "../../../../Data/Transformed/Soil/ISRIC/quartz_30min_global.RDS"
expt.file <- "../../../../Data/Transformed/Soil/ISRIC/expt_30min_global.RDS"
ksat.file <- "../../../../Data/Transformed/Soil/ISRIC/ksat_30min_global.RDS"
bulk.dens.file <- "../../../../Data/Transformed/Soil/ISRIC/bulk_dens_30min_global.RDS"
Wfc.file <- "../../../../Data/Transformed/Soil/ISRIC/Wfc_30min_global.RDS"
Wcr.file <- "../../../../Data/Transformed/Soil/ISRIC/Wcr_30min_global.RDS"
Wpwp.file <- "../../../../Data/Transformed/Soil/ISRIC/Wpwp_30min_global.RDS"
bubble.file <- "../../../../Data/Transformed/Soil/ISRIC/bubble_30min_global.RDS"
max.moist.file <- "../../../../Data/Transformed/Soil/ISRIC/max_moist_30min_global.RDS"
resid.moist.file <- "../../../../Data/Transformed/Soil/ISRIC/resid_moist_30min_global.RDS"
depth.file <- "../../../../Data/Transformed/Soil/Pelletier2016/depth_30min_global.RDS"

vic.out <- "../../../../Data/VIC/Parameters/global/soil_params_Saxton_global.nc"
toplayer.depth = 0.3
rootlayer.maxdepth = 3

# Load
source(map.support.file)
source(generate.support.file)

nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
lon.dim = nc$dim$lon
lat.dim = nc$dim$lat
nc_close(nc)

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
phi.s = array(-9999, dim = dim(Wfc))
dp = array(4, dim = dim(depth))
rough = array(0.001, dim = dim(depth))
snow.rough = array(0.0005, dim = dim(depth))

# Setup
na.map <- is.na(mask) | mask == 0

depth.tmp = depth - toplayer.depth
depth.tmp[depth.tmp < toplayer.depth] = toplayer.depth
depth.tmp[depth.tmp > (rootlayer.maxdepth - toplayer.depth)] = rootlayer.maxdepth - toplayer.depth
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
phi.s.fill <- fillMap(phi.s, na.map, getNearestMean)
dp.fill <- fillMap(dp, na.map, getNearestMean)
rough.fill <- fillMap(rough, na.map, getNearestMean)
snow.rough.fill <- fillMap(snow.rough, na.map, getNearestMean)

# Set third layer depth to contain 1000 mm
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

# Save
soil.vars = loadSoilVars(lon.dim, lat.dim)
                          
nc = nc_create(vic.out, vars = soil.vars)
ncvar_put(nc, "Nlayer", Nlayer.fill)
ncvar_put(nc, "quartz", quartz.fill)
ncvar_put(nc, "expt", expt.fill)
ncvar_put(nc, "Ksat", ksat.fill)
ncvar_put(nc, "bulk_density", bulk.dens.fill)
ncvar_put(nc, "Wfc_FRACT", Wcr.fill)
ncvar_put(nc, "Wcr_FRACT", Wcr.fill)
ncvar_put(nc, "Wpwp_FRACT", Wpwp.fill)
ncvar_put(nc, "bubble", bubble.fill)
ncvar_put(nc, "soil_density", soil.dens.fill)
ncvar_put(nc, "resid_moist", resid.moist.fill)
ncvar_put(nc, "init_moist", init.moist.fill)
ncvar_put(nc, "depth", depth.fill)
ncvar_put(nc, "fs_active", fs.active.fill)
ncvar_put(nc, "phi_s", phi.s.fill)
ncvar_put(nc, "rough", rough.fill)
ncvar_put(nc, "snow_rough", snow.rough.fill)
ncvar_put(nc, "dp", dp.fill)
nc_close(nc)
