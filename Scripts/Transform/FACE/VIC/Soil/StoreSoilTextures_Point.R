library(raster)
rm(list = ls())

soil.dir <- "../../../../Data/Primary/WISE/wise_30sec_v1/Interchangeable_format"
map.file <- "../../../../Data/Primary/WISE/wise_30sec_v1/Interchangeable_format/wise_30sec_v1.tif"
mapping.file <- "../../../../Data/Primary/WISE/wise_30sec_v1/Interchangeable_format/wise_30sec_v1.tsv"
out.clay <- "./Saves_point/clay_point.RDS"
out.sand <- "./Saves_point/sand_point.RDS"
out.silt <- "./Saves_point/silt_point.RDS"
out.bulk <- "./Saves_point/bulk_point.RDS"

point <- c(33.0628, -111.9826) # lat-lon

soil.files <- list.files(soil.dir, pattern = "_wD[0-9]", full.names = T)
map <- raster(map.file)
mapping <- read.table(mapping.file, header = T, stringsAsFactors = F)

# Setup
map.lats = seq(from = extent(map)[3] + res(map)[2] / 2, 
               to = extent(map)[4] - res(map)[2] / 2, 
               by = res(map)[2])
map.lons = seq(from = extent(map)[1] + res(map)[1] / 2, 
               to = extent(map)[2] - res(map)[1] / 2, 
               by = res(map)[1])
map.y = which.min(abs(map.lats - point[1]))
map.x = which.min(abs(map.lons - point[2]))
map = crop(map, extent(map.lons[map.x-1],map.lons[map.x+1],map.lats[map.y-1],map.lats[map.y+1]))
map = mean(c(as.matrix(map)))

mapping = mapping[mapping$pixel_vaue == map,]

layers <- gsub(x = basename(soil.files), pattern = ".txt", replacement = "")
layers <- gsub(x = layers, pattern = ".*wD", replacement = "")
layers <- as.numeric(layers)

# Calculate
clay = rep(NA, length(layers))
sand = rep(NA, length(layers))
silt = rep(NA, length(layers))
bulk = rep(NA, length(layers))

i = 1
for (i in 1:length(soil.files)) {
  soil.file <- soil.files[i]
  print(soil.file)
  soil <- read.table(soil.file, sep = ",", header = T, stringsAsFactors = F)
  
  layer <- layers[i]
  
  row = which(soil$NEWSUID == mapping$description)
  
  clay[layer] <- soil[row, "CLPC"]
  sand[layer] <- soil[row, "SDTO"]
  silt[layer] <- soil[row, "STPC"]
  bulk[layer] <- soil[row, "BULK"]
}

dir.create(dirname(out.clay))
dir.create(dirname(out.sand))
dir.create(dirname(out.silt))
dir.create(dirname(out.bulk))

saveRDS(clay, out.clay)
saveRDS(sand, out.sand)
saveRDS(silt, out.silt)
saveRDS(bulk, out.bulk)

