rm(list = ls())
library(fields)
library(ncdf4)

# Input
vegetation.file <- "../../../../Data/VIC/Parameters/Indus_5min/vegetation_params_Modis_Indus.nc"
crop.grid.file = "./Saves/coverageMIRCA_grid_monthly_5min_Indus.RDS"
crop.rainfed.file = "./Saves/coverageMIRCA_rainfed_monthly_5min_Indus.RDS"
crop.irrigated.file = "./Saves/coverageMIRCA_irrigated_monthly_5min_Indus.RDS"
crop.paddy.file = "./Saves/coverageMIRCA_paddy_monthly_5min_Indus.RDS"
vegetation.crop.idx = 12
fraction.out = "./Saves/canopy_fraction_crops_monthly_5min_Indus.RDS"

# Load
nc = nc_open(vegetation.file)
vegetation.cv = ncvar_get(nc, "Cv")
nc_close(nc)

crop.grid.cv = readRDS(crop.grid.file)
crop.rainfed.cv = readRDS(crop.rainfed.file)
crop.irrigated.cv = readRDS(crop.irrigated.file)
crop.paddy.cv = readRDS(crop.paddy.file)

# Setup
vegetation.crop.cv = vegetation.cv[,,vegetation.crop.idx]
sel = !is.na(vegetation.crop.cv) & !is.na(crop.grid.cv) & vegetation.crop.cv < crop.grid.cv
vegetation.crop.cv[sel] = crop.grid.cv[sel]

# Calculate
total.fraction = crop.grid.cv / vegetation.crop.cv
total.fraction[vegetation.crop.cv == 0] = 0
image.plot(total.fraction)

fraction = crop.rainfed.cv
for(z in 1:dim(fraction)[3]){
  fraction[,,z] = (crop.rainfed.cv[,,z] + crop.irrigated.cv[,,z] + crop.paddy.cv[,,z])  / vegetation.crop.cv
}
for(z in 1:dim(fraction)[3]){
  fraction.tmp = fraction[,,z]
  fraction.tmp[vegetation.crop.cv == 0] = 0
  fraction[,,z] = fraction.tmp
}

for(z in 1:dim(fraction)[3]){
  image.plot(fraction[,,z], main = z)
}

# Save
dir.create(dirname(fraction.out))
saveRDS(fraction, fraction.out)
