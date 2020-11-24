library(ncdf4)
library(fields)
rm(list = ls())

# Input
crop.file <- "./Saves/crop_mapping.csv"
cc.file <- "./Saves/cc_30min_global.RDS"
Ncrop.out <- "./saves/Ncrop_30min_global.RDS"

# Load
crops <- read.csv(crop.file, stringsAsFactors = F)
cc <- readRDS(cc.file)

# Setup
lons <- seq(-179.75, 179.75, by = 0.5)
lats <- seq(-89.75, 89.75, by = 0.5)

# Calculate
Ncrop <- array(0, dim = c(length(lons), length(lats), nrow(crops)))
for (i in 1:nrow(crops)) {
  print(crops$name[i])

  cc.sum <- apply(X = cc[, , i, ], MARGIN = c(1, 2), FUN = sum)

  Ncrop[, , i] <- Ncrop[, , i] + (cc.sum > 0)
}
image.plot(Ncrop[, , 11])

# Save
dir.create(dirname(Ncrop.out))
saveRDS(Ncrop, Ncrop.out)
