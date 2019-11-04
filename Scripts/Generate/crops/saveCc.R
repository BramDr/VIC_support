library(fields)
library(plyr)
rm(list = ls())

# Input
scc.file = "../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
cc.file = "../../../Data/Transformed/LandUse/cropCalendar_30min_global.csv"
crop.file = "./Saves/crop_mapping.csv"
Cc.out = "./Saves/Cc_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
scc = read.csv(scc.file, stringsAsFactors = F, header = T)
cc = read.csv(cc.file, stringsAsFactors = F, header = T)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)
scc = scc[scc$crop %in% crops$crop.id,]
cc = cc[cc$crop %in% crops$crop.id,]

scc.cols = colnames(scc)
cc.cols = colnames(cc)
scc.cols = gsub(x = scc.cols, pattern = "area", replacement = "subcrop.area")
cc.cols = gsub(x = cc.cols, pattern = "area", replacement = "crop.area")
colnames(scc) = scc.cols
colnames(cc) = cc.cols

calendar = join(x = scc,y = cc, by = c("cell_ID", "row", "column", "lat", "lon", "crop"))

get.cc = function(x, Cc.cols, subcrop.cols, crop.cols){
  return(x[subcrop.cols] / x[crop.cols])
}

# Calculate
subcrop.cols = which(colnames(calendar) %in% paste0("subcrop.area.", 1:12))
crop.cols = which(colnames(calendar) %in% paste0("crop.area.", 1:12))
Cc = apply(X = calendar, MARGIN = 1, FUN = get.cc, Cc.cols = Cc.cols, subcrop.cols = subcrop.cols, crop.cols = crop.cols)

calendar[,paste0("Cc.", 1:12)] = NA
Cc.cols = which(colnames(calendar) %in% paste0("Cc.", 1:12))
calendar[,Cc.cols] = t(Cc)
calendar[is.na(calendar)] = 0

# Save
for(j in 1:nrow(crops)) {
  Cc.crop.out = gsub(x = Cc.out, pattern = "Cc_", replacement = paste0("Cc_", j, "_"))
  print(basename(Cc.crop.out))
  
  calendar.crop = calendar[crops$crop.id[j] == calendar$crop & crops$season[j] == calendar$subcrop,]
  
  Cc.map = array(NA, dim = c(length(lons), length(lats), 12))
  for(i in  1:nrow(calendar.crop)) {
    x = which(lons == calendar.crop$lon[i])
    y = which(lats == calendar.crop$lat[i])
    
    Cc.map[x,y,] = as.numeric(calendar.crop[i, Cc.cols])
  }
  image.plot(Cc.map[,,1])
  
  dir.create(dirname(Cc.crop.out))
  saveRDS(Cc.map, Cc.crop.out)
}
