library(fields)
rm(list = ls())

# Input
crop.file <- "./Saves/crop_mapping.RDS"
scc.file <- "./Saves/subcropCalendar_5min_Indus.RDS"
cc.extent.file <- "./Saves/cropCalendar_extent_5min_Indus.RDS"
scc.extent.file <- "./Saves/subcropCalendar_extent_5min_Indus.RDS"
fraction.out <- "./Saves/season_fraction_crops_monthly_5min_Indus.RDS"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
crops <- readRDS(crop.file)
scc <- readRDS(scc.file)
cc.extent <- readRDS(cc.extent.file)
scc.extent <- readRDS(scc.extent.file)

cc.extent = merge(cc.extent, unique(scc[,c("Cell_ID", "lat", "long")]))
scc.extent = merge(scc.extent, unique(scc[,c("Cell_ID", "crop", "lat", "long")]))

split.fraction = array(0, dim = c(length(out.lons), length(out.lats), nrow(crops), 12))

i = 1
for(i in 1:nrow(crops)) {
  print(crops$name[i])
  
  if(crops$season[i] == "max"){
    scc.season.max = aggregate(formula = subcrop ~ Cell_ID + crop, data = scc.extent[scc.extent$crop == crops$mirca[i],], FUN = max)
    colnames(scc.season.max) = c("Cell_ID", "crop", "maxseason")
    scc.extent.tmp = merge(scc.extent, scc.season.max)
    
    scc.extent.crop = scc.extent.tmp[scc.extent.tmp$crop == crops$mirca[i] & scc.extent.tmp$subcrop == scc.extent.tmp$maxseason,]
    cc.extent.crop = aggregate(
      formula = cbind(area.1, area.2, area.3, area.4, area.5, area.6, area.7, area.8, area.9, area.10, area.11, area.12) ~ Cell_ID + lat + long,
      data = scc.extent.crop, FUN = sum
    )
  } else {
    scc.extent.crop = scc.extent[scc.extent$crop == crops$mirca[i] & scc.extent$subcrop == crops$season[i],]
    
    allseasons = unique(crops$season[crops$mirca == crops$mirca[i]])
    cc.extent.crop = aggregate(
      formula = cbind(area.1, area.2, area.3, area.4, area.5, area.6, area.7, area.8, area.9, area.10, area.11, area.12) ~ Cell_ID + lat + long,
      data = scc.extent[scc.extent$crop == crops$mirca[i] & scc.extent$subcrop %in% allseasons,], FUN = sum
    )
  }
  
  id = unique(scc.extent.crop$Cell_ID)[1]
  for(id in unique(scc.extent.crop$Cell_ID)){
    row.cc = which(cc.extent.crop$Cell_ID == id)
    row.scc = which(scc.extent.crop$Cell_ID == id)
    
    x = which.min(abs(out.lons - cc.extent.crop$lon[row.cc]))
    y = which.min(abs(out.lats - cc.extent.crop$lat[row.cc]))
    
    subcrop.area = scc.extent.crop[row.scc, paste0("area.", 1:12)]
    crop.area = cc.extent.crop[row.cc, paste0("area.", 1:12)]
      
    fraction = subcrop.area / crop.area
    fraction[crop.area == 0] = 0
    
    split.fraction[x,y,i,] = as.numeric(fraction)
  }
}

image.plot(split.fraction[,,1,7])
image.plot(split.fraction[,,2,7])
image.plot(split.fraction[,,1,1])
image.plot(split.fraction[,,2,1])

dir.create(dirname(fraction.out))
saveRDS(split.fraction, fraction.out)
