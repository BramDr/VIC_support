library(fields)
library(ncdf4)
rm(list = ls())

# Input
crop.file = "./Saves/crop_mapping.csv"
season.dir = "../../../../Data/Primary/GGCMI/Phase3/crop_calendar"
cc.out = "./Saves/cc_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
season.files = list.files(path = season.dir, pattern = "v1.01.nc4$", full.names = T)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

# Calculate
cc = array(NA, dim = c(length(lons), length(lats), nrow(crops), 12))
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
  plant = ncvar_get(nc, "planting_day")
  nc_close(nc)
  
  cc[,,i,] = plant[,dim(plant)[2]:1] > 0
}
cc[cc <= 0] = NA
image.plot(cc[,,1,5])

# Save
dir.create(dirname(cc.out))
saveRDS(cc, cc.out)

