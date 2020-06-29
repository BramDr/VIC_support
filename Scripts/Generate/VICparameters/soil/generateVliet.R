library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
map.support.file = "../../../../Scripts/Support/mapFunctions.R"
generate.support.file = "../../../../Scripts/Support/generateFunctions.R"
quartz.file = "../../../../Data/Transformed/Parameters/soilQuartz_Vliet30min_30min_global.RDS"
expt.file = "../../../../Data/Transformed/Parameters/soilExpt_Vliet30min_30min_global.RDS"
ksat.file = "../../../../Data/Transformed/Parameters/soilKsat_Vliet30min_30min_global.RDS"
bulk.dens.file = "../../../../Data/Transformed/Parameters/soilBulkDensity_Vliet30min_30min_global.RDS"
Wcr.file = "../../../../Data/Transformed/Parameters/soilWcr_Vliet30min_30min_global.RDS"
Wpwp.file = "../../../../Data/Transformed/Parameters/soilWp_Vliet30min_30min_global.RDS"
bubble.file = "../../../../Data/Transformed/Parameters/soilBubble_Vliet30min_30min_global.RDS"
soil.dens.file = "../../../../Data/Transformed/Parameters/soilSoilDensity_Vliet30min_30min_global.RDS"
resid.moist.file = "../../../../Data/Transformed/Parameters/soilResidual_Vliet30min_30min_global.RDS"
mask.file = "../../../../Data/Primary/VIC/domain_global.nc"
vic.orig = "../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out = "../../../../Data/VIC/Parameters/global/soil_params_Vliet_global.nc"

# Load
source(map.support.file)
source(generate.support.file)

quartz = readRDS(quartz.file)
expt = readRDS(expt.file)
ksat = readRDS(ksat.file)
bulk.dens = readRDS(bulk.dens.file)
Wcr = readRDS(Wcr.file)
Wpwp = readRDS(Wpwp.file)
bubble = readRDS(bubble.file)
soil.dens = readRDS(soil.dens.file)
resid.moist = readRDS(resid.moist.file)

nc = nc_open(mask.file)
mask = ncvar_get(nc, "mask")
nc_close(nc)

nc = nc_open(vic.orig)
depth = ncvar_get(nc, "depth")
nc_close(nc)

# Setup
na.map = is.na(mask) | mask == 0

Nlayer.var = ncvar_def(name = "Nlayer", units = "#", dim = list(nc$dim$lon, nc$dim$lat), longname = "Number of active layers", prec = "integer", missval = -1)

# Calculate
Nlayer = apply(X = expt, MARGIN = c(1,2), FUN = function(x){sum(x > 0)})
  
Nlayer.fill = fillMap(Nlayer, na.map, getNearestZero)
quartz.fill = fillMap(quartz, na.map, getNearestMean)
expt.fill = fillMap(expt, na.map, getNearestMean)
ksat.fill = fillMap(ksat, na.map, getNearestMean)
bulk.dens.fill = fillMap(bulk.dens, na.map, getNearestMean)
Wcr.fill = fillMap(Wcr, na.map, getNearestMean)
Wpwp.fill = fillMap(Wpwp, na.map, getNearestMean)
bubble.fill = fillMap(bubble, na.map, getNearestMean)
soil.dens.fill = fillMap(soil.dens, na.map, getNearestMean)
resid.moist.fill = fillMap(resid.moist, na.map, getNearestMean)

porosity = 1 - bulk.dens.fill / soil.dens.fill
Wpwp.fill[!is.na(Wpwp.fill) & Wpwp.fill < resid.moist.fill / porosity] = resid.moist.fill[!is.na(Wpwp.fill) & Wpwp.fill < resid.moist.fill / porosity] / porosity - 1e-6
Wcr.fill[!is.na(Wcr.fill) & Wcr.fill < Wpwp.fill] = Wpwp.fill[!is.na(Wcr.fill) & Wcr.fill < Wpwp.fill] + 1e-6

# Set third layer depth to contain 100 mm
depth[,,3] = 100 / ((1 - bulk.dens.fill[,,3] / soil.dens.fill[,,3]) * 1000)

max.moist = (1 - bulk.dens.fill / soil.dens.fill) * 1000 * depth
init.moist.fill = (Wcr.fill + (1 - Wcr.fill) * 0.5) * max.moist

# Save
dir.create(dirname(vic.out))
system(command = paste0("ncks -h -v ", paste0(soil.vars, collapse = ","), " ", vic.orig, " -O ", vic.out))
for(att in unused.atts){
  system(command = paste0("ncatted -h -a ",att, ",global,d,, -O ", vic.out))
}

nc = nc_open(vic.out, write = T)
nc = ncvar_add(nc, Nlayer.var)
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
nc_close(nc)

