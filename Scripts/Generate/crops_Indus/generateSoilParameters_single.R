library(fields)
library(ncdf4)
rm(list = ls())

# Input
soil.file <- "../../../Data/VIC/Parameters/Indus_5min/soil_params_Saxton_Indus.nc"
soil.out <- "../../../Data/VIC/Parameters/Indus_5min/single/soil_params_single_Indus.nc"
toplayer.depth = 0.3
crop.rootlayer.maxdepth = 1.5

# Load
nc <- nc_open(soil.file)
depth <- ncvar_get(nc, "depth")
nc_close(nc)

# Setup
depth2 = depth[,,2]
depth2[depth2 > crop.rootlayer.maxdepth - toplayer.depth] = crop.rootlayer.maxdepth - toplayer.depth
depth[,,2] = depth2

# Save
dir.create(dirname(soil.out))
file.copy(soil.file, soil.out, overwrite = T)

nc <- nc_open(soil.out, write = T)
ncvar_put(nc, "depth", depth)
nc_close(nc)
