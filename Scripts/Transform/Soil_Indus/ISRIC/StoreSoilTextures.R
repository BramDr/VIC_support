library(fields)
library(raster)
rm(list = ls())

soil.dir <- "../../../../Data/Primary/WISE/wise_30sec_v1/Interchangeable_format"
map.file <- "../../../../Data/Primary/WISE/wise_30sec_v1/Interchangeable_format/wise_30sec_v1.tif"
mapping.file <- "../../../../Data/Primary/WISE/wise_30sec_v1/Interchangeable_format/wise_30sec_v1.tsv"
out.clay <- "./Saves/clay_5min_Indus.RDS"
out.sand <- "./Saves/sand_5min_Indus.RDS"
out.silt <- "./Saves/silt_5min_Indus.RDS"
out.bulk <- "./Saves/bulk_5min_Indus.RDS"
factor <- 10
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

extent.isel <- extent(min(out.lons) - resolution / 2, 
                      max(out.lons) + resolution / 2, 
                      min(out.lats) - resolution / 2, 
                      max(out.lats) + resolution / 2)
extent.out <- extent(min(out.lons) - resolution / 2, 
                     max(out.lons) + resolution / 2, 
                     min(out.lats) - resolution / 2, 
                     max(out.lats) + resolution / 2)

soil.files <- list.files(soil.dir, pattern = "_wD[0-9]", full.names = T)
map <- raster(map.file)
map <- crop(map, extent.isel)
extent.sel <- extent(map)
map <- as.matrix(map)
mapping <- read.table(mapping.file, header = T, stringsAsFactors = F)

get.char <- function(map, mapping, soil, var) {
  newmap <- array(NA, dim = dim(map))

  map.u <- unique(c(map))
  for (i in 1:length(map.u)) {
    desc <- mapping$description[mapping$pixel_vaue == map.u[i]]

    if (desc == "nodata") {
      next
    }

    row <- which(soil$NEWSUID == desc)
    newmap[map == map.u[i]] <- soil[row, var]
  }
  return(newmap)
}
agg.map <- function(map, factor) {
  r <- raster(map)
  extent(r) <- extent.sel
  r.agg <- aggregate(r, fact = factor, fun = mean, na.rm = T)
  r.agg <- extend(r.agg, extent.out)
  return(as.matrix(r.agg))
}

i = 1
for (i in 1:length(soil.files)) {
  soil.file <- soil.files[i]
  print(soil.file)
  soil <- read.table(soil.file, sep = ",", header = T, stringsAsFactors = F)

  layer <- gsub(x = basename(soil.file), pattern = ".txt", replacement = "")
  layer <- gsub(x = layer, pattern = ".*wD", replacement = "")
  layer <- as.numeric(layer)

  clay <- get.char(map, mapping, soil, "CLPC")
  sand <- get.char(map, mapping, soil, "SDTO")
  silt <- get.char(map, mapping, soil, "STPC")
  bulk <- get.char(map, mapping, soil, "BULK")

  clay.agg <- agg.map(clay, factor)
  sand.agg <- agg.map(sand, factor)
  silt.agg <- agg.map(silt, factor)
  bulk.agg <- agg.map(bulk, factor)

  extent.print <- paste0(extent.isel[1], "_", extent.isel[2], "_", extent.isel[3], "_", extent.isel[4])

  out.clay.tmp <- gsub(out.clay, pattern = "_5min", replacement = paste0("_", layer, "_", extent.print, "_5min"))
  out.sand.tmp <- gsub(out.sand, pattern = "_5min", replacement = paste0("_", layer, "_", extent.print, "_5min"))
  out.silt.tmp <- gsub(out.silt, pattern = "_5min", replacement = paste0("_", layer, "_", extent.print, "_5min"))
  out.bulk.tmp <- gsub(out.bulk, pattern = "_5min", replacement = paste0("_", layer, "_", extent.print, "_5min"))

  dir.create(dirname(out.clay.tmp))
  dir.create(dirname(out.sand.tmp))
  dir.create(dirname(out.silt.tmp))
  dir.create(dirname(out.bulk.tmp))

  saveRDS(clay.agg, out.clay.tmp)
  saveRDS(sand.agg, out.sand.tmp)
  saveRDS(silt.agg, out.silt.tmp)
  saveRDS(bulk.agg, out.bulk.tmp)
}
