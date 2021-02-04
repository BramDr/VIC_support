library(ncdf4)
library(fields)
rm(list = ls())

# Input
crop.file <- "./Saves/crop_mapping.csv"
plant.file <- "./Saves/plantDay_30min_global.RDS"
harvest.file <- "./Saves/harvestDay_30min_global.RDS"
cc.out <- "./saves/cc_30min_global.RDS"

# Load
crops <- read.csv(crop.file, stringsAsFactors = F)
plant <- readRDS(plant.file)
harvest <- readRDS(harvest.file)

# Setup
lons <- seq(-179.75, 179.75, by = 0.5)
lats <- seq(-89.75, 89.75, by = 0.5)
noptions <- dim(plant.file)[4]
nmonths <- 12

# Calculate
cc.map <- array(NA, dim = c(length(lons), length(lats), nrow(crops), nmonths))
i = 1
for (i in 1:nrow(crops)){
  print(crops$name[i])
  
  has.crop = apply(X = plant[,,i,], MARGIN = c(1,2), FUN = function(x){sum(!is.na(x)) > 0})
  for(m in 1:nmonths){
    cc.map.tmp <- cc.map[,,i,m]
    cc.map.tmp[has.crop] = 1
    cc.map[,,i,m] <- cc.map.tmp
  }
}

# Save
dir.create(dirname(cc.out))
saveRDS(cc.map, cc.out)
