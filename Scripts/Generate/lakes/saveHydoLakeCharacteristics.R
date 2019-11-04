library(ncdf4)
rm(list = ls())

# Input
hydrolake.file = "../../../Data/Primary/HydroLakes/HydroLAKES.csv"
selection.file = "../../../Data/Primary/HydroLakes/HydrolakeSelection_AnnetteJanssen.txt"
mask.file = "../../../Data/Primary/VIC/domain_global.nc"
char.out = "Saves/hydrolake_AnnetteJanssen_chars.csv"

# Load
nc = nc_open(mask.file)
mask = ncvar_get(nc, nc$var$mask)
nc_close(nc)

hydrolake = read.csv(hydrolake.file, stringsAsFactors = F)
selection = read.csv(selection.file, sep = ";", stringsAsFactors = F)

# Setup
lakes = hydrolake[hydrolake$Hylak_id %in% selection[,1],]
lakes = lakes[complete.cases(lakes),]

# Calculate
chars = data.frame(ID = numeric(), sourceID = numeric(), depth = numeric(), area = numeric(), elevation = numeric(), lon = numeric(), lat = numeric())
id.counter = 0
for(i in 1:nrow(lakes)){
  x = which.min(abs(nc$dim$lon$vals - lakes$Pour_long[i]))
  y = which.min(abs(nc$dim$lat$vals - lakes$Pour_lat[i]))
  if(is.na(mask[x,y]) || mask[x,y] == 0){
    next
  }
  
  id = id.counter
  sourceid = lakes$Hylak_id[i]
  depth = lakes$Depth_avg[i]
  area = lakes$Lake_area[i] * 1e6
  elevation = lakes$Elevation[i]
  lon = lakes$Pour_long[i]
  lat = lakes$Pour_lat[i]
  
  chars[nrow(chars) + 1, ] = c(id, sourceid, depth, area, elevation, lon, lat)
  id.counter = id.counter + 1
}

# Save
dir.create(dirname(char.out))
write.csv(x = chars, file = char.out, row.names = F)

