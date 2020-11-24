library(fields)
library(raster)
rm(list = ls())

gen.dir <- "./Saves"
out.clay <- "../../../../Data/Transformed/Soil/ISRIC/clay_30min_global.RDS"
out.sand <- "../../../../Data/Transformed/Soil/ISRIC/sand_30min_global.RDS"
out.silt <- "../../../../Data/Transformed/Soil/ISRIC/silt_30min_global.RDS"
out.bulk <- "../../../../Data/Transformed/Soil/ISRIC/bulk_30min_global.RDS"

clay.files <- list.files(gen.dir, pattern = "clay_", full.names = T)
sand.files <- list.files(gen.dir, pattern = "sand_", full.names = T)
silt.files <- list.files(gen.dir, pattern = "silt_", full.names = T)
bulk.files <- list.files(gen.dir, pattern = "bulk_", full.names = T)

combine.files <- function(file.list) {
  map <- array(NA, dim = c(360, 720, 7))

  for (layer in 1:7) {
    file.list.layer <- grep(file.list, pattern = paste0("_", layer, "_"), value = T)

    map.layer <- array(NA, dim = c(360, 720))
    for (file in file.list.layer) {
      print(file)

      map.new <- readRDS(file)
      if (sum(!is.na(map.new)) == 0) {
        next
      }

      sel <- !is.na(map.new) & is.na(map.layer)
      map.layer[sel] <- map.new[sel]

      sel.overlap <- !is.na(map.new) & !is.na(map.layer)
      map.layer[sel.overlap] <- (map.layer[sel.overlap] + map.new[sel.overlap]) / 2
    }

    map[, , layer] <- map.layer
  }

  return(map)
}

clay <- combine.files(clay.files)
sand <- combine.files(sand.files)
silt <- combine.files(silt.files)
bulk <- combine.files(bulk.files)

image.plot(clay[, , 1])
image.plot(sand[, , 1])
image.plot(silt[, , 1])
image.plot(bulk[, , 1])

dir.create(dirname(out.clay))
dir.create(dirname(out.sand))
dir.create(dirname(out.silt))
dir.create(dirname(out.bulk))

saveRDS(clay, out.clay)
saveRDS(sand, out.sand)
saveRDS(silt, out.silt)
saveRDS(bulk, out.bulk)
