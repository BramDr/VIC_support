library(ncdf4)

library(fields)
library(raster)
rm(list = ls())

support.file = "../../../../Scripts/Support/mapFunctions.R"
expt.file = "./Saves/expt_30min_global.RDS"
ksat.file = "./Saves/ksat_30min_global.RDS"
bulk.dens.file = "./Saves/bulk_dens_30min_global.RDS"
Wcr.file = "./Saves/Wcr_30min_global.RDS"
Wpwp.file = "./Saves/Wpwp_30min_global.RDS"
bubble.file = "./Saves/bubble_30min_global.RDS"
soil.dens.file = "./Saves/soil_dens_30min_global.RDS"
mask.file = "../../../../Data/Primary/VIC/domain_global.nc"
vic.orig = "../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out = "../../../../Data/Transformed/VIC/Parameters/VIC_params_Saxton_global.nc"

source(support.file)

expt = readRDS(expt.file)
ksat = readRDS(ksat.file)
bulk.dens = readRDS(bulk.dens.file)
Wcr = readRDS(Wcr.file)
Wpwp = readRDS(Wpwp.file)
bubble = readRDS(bubble.file)
soil.dens = readRDS(soil.dens.file)

nc = nc_open(mask.file)
mask = ncvar_get(nc, "mask")
nc_close(nc)

nc = nc_open(vic.orig)
depth = ncvar_get(nc, "depth")
avg.T = ncvar_get(nc, "avg_T")
nc_close(nc)

# Setup
resid.moist = array(0, dim = dim(expt))
fs.active = avg.T < 5

na.map = is.na(mask) | mask == 0

# Calculate
expt.fill = fillMap(expt, na.map, getNearestMean)
ksat.fill = fillMap(ksat, na.map, getNearestMean)
bulk.dens.fill = fillMap(bulk.dens, na.map, getNearestMean)
Wcr.fill = fillMap(Wcr, na.map, getNearestMean)
Wpwp.fill = fillMap(Wpwp, na.map, getNearestMean)
bubble.fill = fillMap(bubble, na.map, getNearestMean)
soil.dens.fill = fillMap(soil.dens, na.map, getNearestMean)
resid.moist.fill = fillMap(resid.moist, na.map, getNearestMean)
fs.active.fill = fillMap(fs.active, na.map, getNearestMax)

Wcr.fill[!is.na(Wcr.fill) & Wcr.fill < Wpwp.fill] = Wpwp.fill[!is.na(Wcr.fill) & Wcr.fill < Wpwp.fill] + 1e-6

# Set third layer depth to contain 100 mm
depth[,,3] = 100 / ((1 - bulk.dens.fill[,,3] / soil.dens.fill[,,3]) * 1000)

max.moist.fill = (1 - bulk.dens.fill / soil.dens.fill) * depth * 1000
init.moist.fill = max.moist.fill / 2

# Save
dir.create(dirname(vic.out))
file.copy(vic.orig, vic.out, overwrite = T)

nc = nc_open(vic.out, write = T)
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
nc_close(nc)

