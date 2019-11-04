library(ncdf4)
library(fields)
rm(list = ls())

# Input
crop.file = "./Saves/crop_mapping.csv"
plant.file = "./saves/plantDay_30min_global.RDS"
Ncrop.out = "./saves/Ncrop_30min_global.RDS"

# Load
plant = readRDS(plant.file)

# Calculate
Ncrop = apply(X = plant, MARGIN = c(1,2), FUN = function(x){sum(!is.na(x))})
image.plot(Ncrop)

# Save
dir.create(dirname(Ncrop.out))
saveRDS(Ncrop, Ncrop.out)
