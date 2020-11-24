library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
map.support.file <- "../../../../Scripts/Support/mapFunctions.R"
generate.support.file <- "../../../../Scripts/Support/generateFunctions.R"
Ds.file <- "../../../../Data/Transformed/Parameters/baseflowD1_Vliet30min_30min_global.RDS"
Dsmax.file <- "../../../../Data/Transformed/Parameters/baseflowD2_Vliet30min_30min_global.RDS"
Ws.file <- "../../../../Data/Transformed/Parameters/baseflowD3_Vliet30min_30min_global.RDS"
c.file <- "../../../../Data/Transformed/Parameters/baseflowD4_Vliet30min_30min_global.RDS"
depth.file <- "../../../../Data/Transformed/Parameters/depth_Vliet30min_30min_global.RDS"
infilt.file <- "../../../../Data/Transformed/Parameters/infilt_Vliet30min_30min_global.RDS"
mask.file <- "../../../../Data/Primary/VIC/domain_global.nc"
vic.orig <- "../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out <- "../../../../Data/VIC/Parameters/global/calibration_params_Vliet_global.nc"

# Load
source(map.support.file)
source(generate.support.file)

Ds <- readRDS(Ds.file)
Dsmax <- readRDS(Dsmax.file)
Ws <- readRDS(Ws.file)
c <- readRDS(c.file)
depth <- readRDS(depth.file)
infilt <- readRDS(infilt.file)

nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

# Setup
na.map <- is.na(mask) | mask == 0

# Calculate
Ds.fill <- fillMap(Ds, na.map, getNearestMean)
Dsmax.fill <- fillMap(Dsmax, na.map, getNearestMean)
Ws.fill <- fillMap(Ws, na.map, getNearestMean)
c.fill <- fillMap(c, na.map, getNearestMean)
depth.fill <- fillMap(depth, na.map, getNearestMean)
infilt.fill <- fillMap(infilt, na.map, getNearestMean)

# Save
dir.create(dirname(vic.out))
system(command = paste0("ncks -h -v ", paste0(calib.vars, collapse = ","), " ", vic.orig, " -O ", vic.out))
for (att in unused.atts) {
  system(command = paste0("ncatted -h -a ", att, ",global,d,, -O ", vic.out))
}

nc <- nc_open(vic.out, write = T)
ncvar_put(nc, "Ds", Ds.fill)
ncvar_put(nc, "Dsmax", Dsmax.fill)
ncvar_put(nc, "Ws", Ws.fill)
ncvar_put(nc, "c", c.fill)
ncvar_put(nc, "depth", depth.fill)
ncvar_put(nc, "infilt", infilt.fill)
nc_close(nc)
