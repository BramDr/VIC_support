library(ncdf4)
library(fields)
rm(list = ls())

# Input
crop.file <- "./Saves/crop_mapping.RDS"
Cc.file <- "./Saves/season_fraction_crops_monthly_5min_Indus.RDS"
Ncrop.out <- "./Saves/Ncrop_crops_5min_Indus.RDS"

# Load
crops <- readRDS(crop.file)
Cc <- readRDS(Cc.file)

# Calculate
Cc.max <- apply(X = Cc, MARGIN = c(1, 2, 3), FUN = max)
Ncrop <- apply(X = Cc.max, MARGIN = c(1, 2), FUN = function(x){ sum(x > 0) })
image.plot(Ncrop)

# Save
dir.create(dirname(Ncrop.out))
saveRDS(Ncrop, Ncrop.out)
