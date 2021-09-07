library(fields)
library(raster)
library(ncdf4)
rm(list = ls())

ocar.file <- "../../../../Data/Transformed/Soil/ISRIC/ocar_30min_global.RDS"
tnit.file <- "../../../../Data/Transformed/Soil/ISRIC/tnit_30min_global.RDS"
cnrat.file <- "../../../../Data/Transformed/Soil/ISRIC/cnrat_30min_global.RDS"
clay.file <- "../../../../Data/Transformed/Soil/ISRIC/clay_30min_global.RDS"
ph.file <- "../../../../Data/Transformed/Soil/ISRIC/ph_30min_global.RDS"
carbon.out <- "./Saves/carbon_30min_global.RDS"
ph.out <- "./Saves/ph_30min_global.RDS"

# Load
clay <- readRDS(clay.file)
ocar <- readRDS(ocar.file)
tnit <- readRDS(tnit.file)
cnrat <- readRDS(cnrat.file)
ph <- readRDS(ph.file)

# Setup
lons <- seq(-179.75, 179.75, by = 0.5)
lats <- seq(-89.75, 89.75, by = 0.5)

clay[clay <= 0] <- NA
ocar[ocar <= 0] <- NA
tnit[tnit <= 0] <- NA
cnrat[cnrat <= 0] <- NA
ph[ph <= 0] <- NA

# Calculate
clay <- t(clay[dim(clay)[1]:1, , 1])
ocar <- t(ocar[dim(ocar)[1]:1, , 1])
tnit <- t(tnit[dim(tnit)[1]:1, , 1])
cnrat <- t(cnrat[dim(cnrat)[1]:1, , 1])
ph <- t(ph[dim(ph)[1]:1, , 1])
onit <- ocar / cnrat

# Save
dir.create(dirname(carbon.out))
saveRDS(ocar, carbon.out)
dir.create(dirname(ph.out))
saveRDS(ph, ph.out)
