library(fields)
rm(list = ls())

# Input
scc.file = "../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
cc.file = "../../../Data/Transformed/LandUse/cellCalendar_30min_global.csv"
area.file = "../../../Data/Primary/MIRCA2000/Cell area grid/cell_area_ha_30mn.asc"
crop.file = "./Saves/crop_mapping_MIRCA.csv"
Cc.out = "./Saves/Cc_MIRCA_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
scc = read.csv(scc.file, stringsAsFactors = F, header = T)
cc = read.csv(cc.file, stringsAsFactors = F, header = T)
area = t(as.matrix(read.table(file = area.file, skip = 6)))

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

scc$x = scc$column
scc$y = 360 - scc$row
cc$x = cc$column
cc$y = 360 - cc$row

set.Cc.map = function(x, columns) {
  x <- as.numeric(x)
  #print(x[columns == "rowname"])
  
  x.idx = x[columns == "x"]
  y.idx = x[columns == "y"]
  
  Cc.map[x.idx,y.idx,] <<- x[grep(x = columns, pattern = "area.")] / area[x.idx,y.idx]
  return(0)
}

set.Cc.max.map = function(x, columns) {
  x <- as.numeric(x)
  #print(x[columns == "rowname"])
  
  x.idx = x[columns == "x"]
  y.idx = x[columns == "y"]
  
  Cc.max.map[x.idx,y.idx] <<- x[grep(x = columns, pattern = "maxarea")] / area[x.idx,y.idx]
  return(0)
}

# Save
## Crops
Cc.crop.map = array(0, dim = c(length(lons), length(lats), 12))
for(i in 1:nrow(crops)) {
  print(crops$mirca.name[i])
  
  Cc.crop.out = gsub(x = Cc.out, pattern = "Cc_", replacement = paste0("Cc_", i, "_"))
  print(basename(Cc.crop.out))
  
  scc.c = scc[scc$crop == crops$mirca.id[i] & scc$subcrop == crops$season[i], ]
  scc.c$rowname = 1:nrow(scc.c)
  
  Cc.map = array(0, dim = c(length(lons), length(lats), 12))
  apply(X = scc.c, MARGIN = 1, FUN = set.Cc.map, columns = colnames(scc.c))
  #image.plot(Cc.map[,,1])
  
  Cc.crop.map = Cc.crop.map + Cc.map
  
  dir.create(dirname(Cc.crop.out))
  saveRDS(Cc.map, Cc.crop.out)
}
#image.plot(Cc.crop.map[,,1])

## Total
Cc.crop.out = gsub(x = Cc.out, pattern = "Cc_", replacement = paste0("Cc_", "total", "_"))
print(basename(Cc.crop.out))

Cc.map = array(0, dim = c(length(lons), length(lats), 12))
apply(X = cc, MARGIN = 1, FUN = set.Cc.map, columns = colnames(cc))
Cc.map[Cc.map < Cc.crop.map] = Cc.crop.map[Cc.map < Cc.crop.map]
Cc.max.map = apply(X = Cc.map, MARGIN = c(1,2), FUN = max)
#image.plot(Cc.map[,,1])

dir.create(dirname(Cc.crop.out))
saveRDS(Cc.map, Cc.crop.out)

## Other
Cc.crop.out = gsub(x = Cc.out, pattern = "Cc_", replacement = paste0("Cc_", "other", "_"))
print(basename(Cc.crop.out))

Cc.other.map = Cc.map - Cc.crop.map
#sum(Cc.other.map < 0)
#image.plot(Cc.other.map[,,1])

dir.create(dirname(Cc.crop.out))
saveRDS(Cc.other.map, Cc.crop.out)

## Bare
Cc.crop.out = gsub(x = Cc.out, pattern = "Cc_", replacement = paste0("Cc_", "bare", "_"))
print(basename(Cc.crop.out))

Cc.bare.map = array(0, dim = c(length(lons), length(lats), 12))
for(i in 1:12) {
  Cc.bare.map[,,i] = Cc.max.map - Cc.crop.map[,,i] - Cc.other.map[,,i]
}
#sum(Cc.bare.map < 0)
#image.plot(Cc.bare.map[,,1])

dir.create(dirname(Cc.crop.out))
saveRDS(Cc.bare.map, Cc.crop.out)
