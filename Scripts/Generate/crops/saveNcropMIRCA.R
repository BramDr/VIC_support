library(ncdf4)
library(fields)
rm(list = ls())

# Input
crop.file = "./Saves/crop_mapping_MIRCA.csv"
Cc.dir = "./Saves/"
Cc.other.file = "./Saves/Cc_other_MIRCA_30min_global.RDS"
Cc.bare.file = "./Saves/Cc_bare_MIRCA_30min_global.RDS"
Ncrop.out = "./saves/Ncrop_MIRCA_30min_global.RDS"
Nbare.out = "./saves/Nbare_MIRCA_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
Cc.files = list.files(path = Cc.dir, pattern = "Cc_.*_MIRCA", full.names = T)
Cc.other = readRDS(Cc.other.file)
Cc.bare = readRDS(Cc.bare.file)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

vic.id.u = unique(crops$vic.id)

# Calculate
Cc.sum = array(0, dim = c(length(lons), length(lats), nrow(crops) + 1))
for(i in 1:nrow(crops)) {
  print(crops$mirca.name[i])
  
  Cc.file = grep(x = Cc.files, pattern = paste0("_", i, "_"), value = T)
  Cc = readRDS(Cc.file)
  
  Cc.sum[,,i] = Cc.sum[,,i] + apply(X = Cc, MARGIN = c(1,2), FUN = sum)
}
Cc.sum[,,dim(Cc.sum)[3]] = Cc.sum[,,dim(Cc.sum)[3]] + apply(X = Cc.other, MARGIN = c(1,2), FUN = sum)

Ncrop = Cc.sum > 0
image.plot(Ncrop[,,1])
image.plot(Ncrop[,,dim(Cc.sum)[3]])

Cc.sum = apply(X = Cc.bare, MARGIN = c(1,2), FUN = sum)
Nbare = Cc.sum > 0
image.plot(Nbare)

# Save
dir.create(dirname(Ncrop.out))
saveRDS(Ncrop, Ncrop.out)
dir.create(dirname(Nbare.out))
saveRDS(Nbare, Nbare.out)
