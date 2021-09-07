library(ncdf4)
library(fields)
rm(list = ls())

# Input
crop.file <- "./Saves/crop_mapping.csv"
plant.file <- "./Saves/plantDay_30min_global.RDS"
harvest.file <- "./Saves/harvestDay_30min_global.RDS"
Ncrop.out <- "./saves/Ncrop_30min_global.RDS"

# Load
crops <- read.csv(crop.file, stringsAsFactors = F)
plant <- readRDS(plant.file)
harvest <- readRDS(harvest.file)

# Setup
lons <- seq(-179.75, 179.75, by = 0.5)
lats <- seq(-89.75, 89.75, by = 0.5)
noptions <- dim(plant.file)[4]

# Calculate
Ncrop <- !is.na(plant) & !is.na(harvest)
image.plot(Ncrop[, , 1, 1])
image.plot(Ncrop[, , 1, 9])

# Save
dir.create(dirname(Ncrop.out))
saveRDS(Ncrop, Ncrop.out)
