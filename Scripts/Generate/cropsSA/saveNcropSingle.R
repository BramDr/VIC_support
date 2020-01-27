library(ncdf4)
library(fields)
rm(list = ls())

# Input
crop.file = "./Saves/crop_mapping_single.csv"
Cc.dir = "./Saves/"
Ncrop.out = "./saves/Ncrop_single_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
Cc.files = list.files(path = Cc.dir, pattern = "Cc_.*_single", full.names = T)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

# Calculate
Ncrop = array(0, dim = c(length(lons), length(lats), nrow(crops)))
for(i in 1:nrow(crops)) {
  print(crops$mirca.name[i])

  Cc.file = grep(x = Cc.files, pattern = paste0("_", i, "_"), value = T)
  Cc = readRDS(Cc.file)
  Cc.sum = apply(X = Cc, MARGIN = c(1,2), FUN = sum)
  
  Ncrop[,,i] = Ncrop[,,i] + (Cc.sum > 0)
}
image.plot(Ncrop[,,i])

# Save
dir.create(dirname(Ncrop.out))
saveRDS(Ncrop, Ncrop.out)
