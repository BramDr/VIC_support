library(fields)
library(plyr)
rm(list = ls())

# Input
crop.file = "./Saves/crop_mapping_single.csv"
Cc.out = "./Saves/Cc_single_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

# Save
for(i in 1:nrow(crops)) {
  Cc.crop.out = gsub(x = Cc.out, pattern = "Cc_", replacement = paste0("Cc_", i, "_"))
  print(basename(Cc.crop.out))
  
  Cc.map = array(1, dim = c(length(lons), length(lats), 12))
  
  dir.create(dirname(Cc.crop.out))
  saveRDS(Cc.map, Cc.crop.out)
}
