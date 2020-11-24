library(ncdf4)
library(fields)
rm(list = ls())

# Input
crop.file <- "./Saves/crop_mapping.csv"
Cc.file <- "./Saves/Cc_30min_global.RDS"
Ncrop.out <- "./saves/Ncrop_30min_global.RDS"

# Load
crops <- read.csv(crop.file, stringsAsFactors = F)
Cc <- readRDS(Cc.file)

# Setup
lons <- seq(-179.75, 179.75, by = 0.5)
lats <- seq(-89.75, 89.75, by = 0.5)

# Calculate
Ncrop <- array(0, dim = c(length(lons), length(lats)))
Cc.sum <- apply(X = Cc, MARGIN = c(1, 2, 3), FUN = sum)
for (i in 1:nrow(crops)) {
  print(crops$name[i])

  Ncrop[Cc.sum[, , i] > 0] <- Ncrop[Cc.sum[, , i] > 0] + 1
}
image.plot(Ncrop)

# Save
dir.create(dirname(Ncrop.out))
saveRDS(Ncrop, Ncrop.out)
