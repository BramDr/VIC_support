rm(list = ls())
library(fields)
library(raster)

# Input
mapping.file <- "./Saves/subcropCalendar_5min_Indus.RDS"
scc.file <- "./Saves/subcropCalendar_extent_5min_Indus.RDS"
gc.file <- "./Saves/gridCalendar_extent_5min_Indus.RDS"
cc.file <- "./Saves/cropCalendar_extent_5min_Indus.RDS"
cropland.out <- "./Saves/croplandMIRCA_monthly_5min_Indus.RDS"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

extent.out <- extent(min(out.lons) - resolution / 2, 
                     max(out.lons) + resolution / 2, 
                     min(out.lats) - resolution / 2, 
                     max(out.lats) + resolution / 2)

crop.split <- data.frame(
  name = c(
    "wheatRainfed", "wheatIrrigated",
    "riceRainfed", "riceIrrigated"
  ),
  id = c(
    27, 1,
    29, 3
  ),
  water = c(FALSE, TRUE,
            FALSE, TRUE),
  stringsAsFactors = F
)

# Load
mapping <- readRDS(mapping.file)
scc.extent <- readRDS(scc.file)
gc.extent <- readRDS(gc.file)
cc.extent <- readRDS(cc.file)

scc.extent = merge(scc.extent, unique(mapping[,c("Cell_ID", "crop", "subcrop", "lat", "long")]))
gc.extent = merge(gc.extent, unique(mapping[,c("Cell_ID", "lat", "long")]))
cc.extent = merge(cc.extent, unique(mapping[,c("Cell_ID", "crop", "lat", "long")]))

# Setup
cc.extent.sel = cc.extent[cc.extent$crop %in% 27:52,]
gc.extent.rainfed <- aggregate(
  formula = cbind(area.1, area.2, area.3, area.4, area.5, area.6, area.7, area.8, area.9, area.10, area.11, area.12) ~ Cell_ID + lat + long,
  data = cc.extent.sel, FUN = sum
)
gc.extent.rainfed$area.max <- apply(X = gc.extent.rainfed[, paste0("area.", 1:12)], MARGIN = 1, FUN = max)

cc.extent.sel = cc.extent[cc.extent$crop %in% c(1:2, 4:26),]
gc.extent.irrigated <- aggregate(
  formula = cbind(area.1, area.2, area.3, area.4, area.5, area.6, area.7, area.8, area.9, area.10, area.11, area.12) ~ Cell_ID + lat + long,
  data = cc.extent.sel, FUN = sum
)
gc.extent.irrigated$area.max <- apply(X = gc.extent.irrigated[, paste0("area.", 1:12)], MARGIN = 1, FUN = max)

cc.extent.sel = cc.extent[cc.extent$crop %in% 3,]
gc.extent.paddy <- aggregate(
  formula = cbind(area.1, area.2, area.3, area.4, area.5, area.6, area.7, area.8, area.9, area.10, area.11, area.12) ~ Cell_ID + lat + long,
  data = cc.extent.sel, FUN = sum
)
gc.extent.paddy$area.max <- apply(X = gc.extent.paddy[, paste0("area.", 1:12)], MARGIN = 1, FUN = max)

# Calculate
rainfed.cropland = array(0, dim = c(length(out.lons), length(out.lats), 12))
for(i in 1:nrow(gc.extent.rainfed)){
  x = which.min(abs(out.lons - gc.extent.rainfed$lon[i]))
  y = which.min(abs(out.lats - gc.extent.rainfed$lat[i]))
  rainfed.cropland[x,y,] = rainfed.cropland[x,y,] + as.numeric(gc.extent.rainfed[i, paste0("area.", 1:12)])
}
image.plot(rainfed.cropland[,,6], zlim = c(0,8500))

irrigated.cropland = array(0, dim = c(length(out.lons), length(out.lats), 12))
for(i in 1:nrow(gc.extent.irrigated)){
  x = which.min(abs(out.lons - gc.extent.irrigated$lon[i]))
  y = which.min(abs(out.lats - gc.extent.irrigated$lat[i]))
  irrigated.cropland[x,y,] = irrigated.cropland[x,y,] + as.numeric(gc.extent.irrigated[i, paste0("area.", 1:12)])
}
image.plot(irrigated.cropland[,,6], zlim = c(0,8500))

paddy.cropland = array(0, dim = c(length(out.lons), length(out.lats), 12))
for(i in 1:nrow(gc.extent.paddy)){
  x = which.min(abs(out.lons - gc.extent.paddy$lon[i]))
  y = which.min(abs(out.lats - gc.extent.paddy$lat[i]))
  paddy.cropland[x,y,] = paddy.cropland[x,y,] + as.numeric(gc.extent.paddy[i, paste0("area.", 1:12)])
}
image.plot(paddy.cropland[,,6], zlim = c(0,8500))

grid.cropland = apply(X = rainfed.cropland + irrigated.cropland, MARGIN = c(1,2), FUN = max) + 
  apply(X = paddy.cropland, MARGIN = c(1,2), FUN = max)
image.plot(grid.cropland, zlim = c(0,8500))

split.croplands = list()
for(j in 1:nrow(crop.split)){
  scc.extent.sel = scc.extent[scc.extent$crop == crop.split$id[j],]
  
  if(crop.split$id[j] == 1) {
    ## Select only 1 growing season for wheat (2nd season in India)
    cc.maxseason = aggregate(formula = subcrop ~ crop + Cell_ID, data = scc.extent.sel, FUN = max)
    colnames(cc.maxseason) = c("crop", "Cell_ID", "maxseason")
    scc.extent.sel = merge(scc.extent.sel, cc.maxseason)
    scc.extent.sel = scc.extent.sel[scc.extent.sel$subcrop == scc.extent.sel$maxseason,]
  } else if(crop.split$id[j] == 3) {
    ## Select only 2 growing seasons for rice
    scc.extent.sel = scc.extent.sel[scc.extent.sel$subcrop %in% c(1,2),]
  }
  
  split.cropland = array(0, dim = c(length(out.lons), length(out.lats), 12)) 
  for(i in 1:nrow(scc.extent.sel)){
    x = which.min(abs(out.lons - scc.extent.sel$lon[i]))
    y = which.min(abs(out.lats - scc.extent.sel$lat[i]))
    split.cropland[x,y,] = split.cropland[x,y,] + as.numeric(scc.extent.sel[i, paste0("area.", 1:12)])
  }
  image.plot(split.cropland[,,12], zlim = c(0,8500))
  
  split.croplands[[crop.split$name[j]]] = split.cropland
}

cropland.out.tmp = gsub(x = cropland.out, pattern = "_monthly_5min_Indus", replacement = paste0("_", "grid", "_monthly_5min_Indus"))
dir.create(dirname(cropland.out.tmp))
saveRDS(grid.cropland, cropland.out.tmp)
cropland.out.tmp = gsub(x = cropland.out, pattern = "_monthly_5min_Indus", replacement = paste0("_", "rainfed", "_monthly_5min_Indus"))
dir.create(dirname(cropland.out.tmp))
saveRDS(rainfed.cropland, cropland.out.tmp)
cropland.out.tmp = gsub(x = cropland.out, pattern = "_monthly_5min_Indus", replacement = paste0("_", "irrigated", "_monthly_5min_Indus"))
dir.create(dirname(cropland.out.tmp))
saveRDS(irrigated.cropland, cropland.out.tmp)
cropland.out.tmp = gsub(x = cropland.out, pattern = "_monthly_5min_Indus", replacement = paste0("_", "paddy", "_monthly_5min_Indus"))
dir.create(dirname(cropland.out.tmp))
saveRDS(paddy.cropland, cropland.out.tmp)
for(j in 1:nrow(crop.split)){
  split.cropland = split.croplands[[ crop.split$name[j]]]
  cropland.out.tmp = gsub(x = cropland.out, pattern = "_monthly_5min_Indus", replacement = paste0("_", crop.split$name[j], "_monthly_5min_Indus"))
  dir.create(dirname(cropland.out.tmp))
  saveRDS(split.cropland, cropland.out.tmp)
}

