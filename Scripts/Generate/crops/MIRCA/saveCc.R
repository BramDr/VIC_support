library(fields)
rm(list = ls())

# Input
scc.file = "../../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
cc.file = "../../../../Data/Transformed/LandUse/cellCalendar_30min_global.csv"
area.file = "../../../../Data/Primary/MIRCA2000/Cell area grid/cell_area_ha_30mn.asc"
crop.file = "./Saves/crop_mapping.csv"
Cc.out = "./Saves/Cc_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
scc = read.csv(scc.file, stringsAsFactors = F, header = T)
cc = read.csv(cc.file, stringsAsFactors = F, header = T)
area = t(as.matrix(read.table(file = area.file, skip = 6)))

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

scc$x = sapply(X = scc$lon, FUN = function(x){which(lons == x)})
scc$y = sapply(X = scc$lat, FUN = function(x){which(lats == x)})

# Calculate
cc.map = array(NA, dim = c(length(lons), length(lats), nrow(crops), 12))
for(vic in unique(crops$vic)){
  crops.vic = crops[crops$vic == vic,]
  
  ## Generate tile area map
  for(i in 1:nrow(crops.vic)){
    if(i == 1){
      scc.vic = scc[scc$crop == crops.vic$mirca[i] & scc$subcrop == crops.vic$season[i],]
    } else {
      scc.vic = rbind(scc.vic, scc[scc$crop == crops.vic$mirca[i] & scc$subcrop == crops.vic$season[i],])
    }
  }
  
  area.cell = aggregate(
    formula = cbind(area.1, area.2, area.3, area.4, area.5, area.6, area.7, area.8, area.9, area.10, area.11, area.12) ~ cell_ID + x + y,
    data = scc.vic, FUN = sum
  )
  
  area.map = array(0, dim = c(length(lons), length(lats), 12))
  for(i in 1:nrow(area.cell)){
      area.map[area.cell$x[i], area.cell$y[i],] = as.numeric(area.cell[i, paste0("area.", 1:12)])
  }
  image.plot(area.map[,,1], main = paste0("Cultivation area ", crops.vic$name[1], " ", crops.vic$water[1]))
  
  ## Generate crop coverage map
  for(i in 1:nrow(crops)){
    if(crops$vic[i] != vic){
      next
    }
    cc.map[,,i,] = 0
    
    scc.crop = scc[scc$crop == crops$mirca[i] & scc$subcrop == crops$season[i],]
    for (j in 1:nrow(scc.crop)){
      cc.map[scc.crop$x[j], scc.crop$y[j], i, ] = 
        as.numeric(scc.crop[j,paste0("area.", 1:12)]) / 
        area.map[scc.crop$x[j],scc.crop$y[j],]
      cc.map[scc.crop$x[j], scc.crop$y[j], i, area.map[scc.crop$x[j],scc.crop$y[j],] == 0] = 0
      if(is.na(cc.map[scc.crop$x[j], scc.crop$y[j], i, 1])){
        break
      }
    }
    
    image.plot(cc.map[,,i,1], main = paste0("Cc ", crops$name[i], " ", crops$water[i], " s", crops$season[i]))
  }
}

# Save
dir.create(dirname(Cc.out))
saveRDS(cc.map, Cc.out)
