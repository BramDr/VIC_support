library(fields)
rm(list = ls())

# Input
gs.dir = "../../../Data/Transformed/GrowingSeason/"
crop.file = "./Saves/crop_mapping_single.csv"
plant.out = "./Saves/plantDay_SAGE_30min_global.RDS"
harvest.out = "./Saves/harvestDay_SAGE_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
gs.files = list.files(path = gs.dir, pattern = "growingSeasonFill_", full.names = T)
gs.names = gsub(x = basename(gs.files), pattern = "growingSeasonFill_", replacement = "")
gs.names = gsub(x = gs.names, pattern = "_30min.*", replacement = "")

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

# Calculate
plant_day = array(NA, dim = c(length(lons), length(lats), nrow(crops)))
harvest_day = array(NA, dim = c(length(lons), length(lats), nrow(crops)))
for(i in 1:nrow(crops)){
  print(crops$mirca.name[i])
  
  gs.idx = which(gs.names == tolower(crops$sage.name[i]))
  gs.file = gs.files[gs.idx]
  
  gs = readRDS(gs.file)
  
  plant_day[,,i] = floor(gs$plant + 0.99)
  harvest_day[,,i] = floor(gs$harvest + 0.99)
}
image.plot(plant_day[,,1])
image.plot(harvest_day[,,1])

# Save
dir.create(dirname(plant.out))
saveRDS(plant_day, plant.out)
dir.create(dirname(harvest.out))
saveRDS(harvest_day, harvest.out)

