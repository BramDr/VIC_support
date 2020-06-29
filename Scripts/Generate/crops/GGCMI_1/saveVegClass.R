library(fields)
rm(list = ls())

# Input
crop.file = "./Saves/crop_mapping.csv"
veg.class.out = "./Saves/cropVegClass_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

# Calculate
veg.class = array(NA, dim = c(length(lons), length(lats), nrow(crops)))
for (i in 1:nrow(crops)){
  print(crops$name[i])
  
  veg.class[,,i] = 1
}
image.plot(veg.class[,,1])

# Save
dir.create(dirname(veg.class.out))
saveRDS(veg.class, veg.class.out)
