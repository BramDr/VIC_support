library(fields)
rm(list = ls())

# Input
scc.file = "../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
crop.file = "./Saves/crop_mapping.csv"
plant.out = "./Saves/plantDay_30min_global.RDS"
harvest.out = "./Saves/harvestDay_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
scc = read.csv(scc.file, stringsAsFactors = F, header = T)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)
scc = scc[scc$crop %in% crops$crop.id,]

# Calculate
plant_day = array(NA, dim = c(length(lons), length(lats), nrow(crops)))
harvest_day = array(NA, dim = c(length(lons), length(lats), nrow(crops)))
for(i in 1:nrow(scc)){
  print(i)
  
  cid = scc$crop[i]
  scid = scc$subcrop[i]
  
  x = which(lons == scc$lon[i])
  y = which(lats == scc$lat[i])
  z = which(crops$crop.id == cid & crops$season == scid)
  
  start = as.numeric(format.Date(as.Date(paste0("1970-", scc$start[i], "-01")), "%j"))
  end = as.numeric(format.Date(as.Date(paste0("1970-", scc$end[i], "-28")), "%j"))
  plant_day[x,y,z] = start
  harvest_day[x,y,z] = end
}
image.plot(plant_day[,,2])
image.plot(harvest_day[,,2])

# Save
dir.create(dirname(plant.out))
saveRDS(plant_day, plant.out)
dir.create(dirname(harvest.out))
saveRDS(harvest_day, harvest.out)
