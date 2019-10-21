rm(list = ls())

# Input
hydrolake.file = "../../../Data/Primary/HydroLakes/HydroLAKES.csv"
selection.file = "../../../Data/Primary/HydroLakes/PClake_selection.csv"
char.out = "Saves/hydrolake_chars.csv"

# Load
hydrolake = read.csv(hydrolake.file, stringsAsFactors = F)
selection = read.csv(selection.file, sep = ";", stringsAsFactors = F)

# Setup
lakes = hydrolake[hydrolake$Hylak_id %in% selection$Hylak_ID,]
lakes = lakes[complete.cases(lakes),]

# Calculate
chars = data.frame(ID = numeric(), sourceID = numeric(), depth = numeric(), area = numeric(), elevation = numeric(), lon = numeric(), lat = numeric())
for(i in 1:nrow(lakes)){
  id = i - 1
  sourceid = lakes$Hylak_id[i]
  depth = lakes$Depth_avg[i]
  area = lakes$Lake_area[i] * 1e6
  elevation = lakes$Elevation[i]
  lon = lakes$Pour_long[i]
  lat = lakes$Pour_lat[i]
  
  chars[nrow(chars) + 1, ] = c(id, sourceid, depth, area, elevation, lon, lat)
}

# Save
dir.create(dirname(char.out))
write.csv(x = chars, file = char.out, row.names = F)
