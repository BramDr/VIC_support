library(fields)
library(ncdf4)
rm(list = ls())

# Input
crop.file = "./Saves/crop_mapping.csv"
season.dir = "../../../../Data/Primary/GGCMI/Phase1/AGMIP_GROWING_SEASON.HARM.version1.25"
plant.out = "./Saves/plantDay_30min_global.RDS"
harvest.out = "./Saves/harvestDay_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
season.files = list.files(path = season.dir, pattern = ".nc4$", full.names = T)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

# Calculate
plant_day = array(NA, dim = c(length(lons), length(lats), nrow(crops)))
harvest_day = array(NA, dim = c(length(lons), length(lats), nrow(crops)))
for(i in 1:nrow(crops)){
  print(crops$name[i])
  
  if(crops$priority[i] > 2){
    next
  }
  if(is.na(crops$season[i])){
    next
  }
  
  season.idx = grep(x = basename(season.files), pattern = crops$season[i])
  season.file = season.files[season.idx]
  
  nc = nc_open(season.file)
  plant = ncvar_get(nc, "planting day")
  harvest = ncvar_get(nc, "harvest day")
  nc_close(nc)
  
  plant_day[,,i] = ceiling(plant)
  harvest_day[,,i] = ceiling(harvest)
}
plant_day[plant_day < 0] = NA
harvest_day[harvest_day < 0] = NA
image.plot(plant_day[,,1])
image.plot(harvest_day[,,1])

# Save
dir.create(dirname(plant.out))
saveRDS(plant_day, plant.out)
dir.create(dirname(harvest.out))
saveRDS(harvest_day, harvest.out)

