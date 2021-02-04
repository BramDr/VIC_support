library(fields)
library(raster)
rm(list = ls())

gen.dir <- "./Saves"
out.ocar <- "../../../../Data/Transformed/Soil/ISRIC/ocar_5min_Indus.RDS"
out.ph <- "../../../../Data/Transformed/Soil/ISRIC/ph_5min_Indus.RDS"
out.tnit <- "../../../../Data/Transformed/Soil/ISRIC/tnit_5min_Indus.RDS"
out.cnrat <- "../../../../Data/Transformed/Soil/ISRIC/cnrat_5min_Indus.RDS"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

ocar.files <- list.files(gen.dir, pattern = "ocar_.*5min_Indus", full.names = T)
ph.files <- list.files(gen.dir, pattern = "ph_.*5min_Indus", full.names = T)
tnit.files <- list.files(gen.dir, pattern = "tnit_.*5min_Indus", full.names = T)
cnrat.files <- list.files(gen.dir, pattern = "cnrat_.*5min_Indus", full.names = T)

combine.files <- function(file.list) {
  map <- array(NA, dim = c(length(out.lats), length(out.lons), 7))

  for (layer in 1:7) {
    file.list.layer <- grep(file.list, pattern = paste0("_", layer, "_"), value = T)

    map.layer <- array(NA, dim = c(length(out.lats), length(out.lons)))
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

ocar <- combine.files(ocar.files)
ph <- combine.files(ph.files)
tnit <- combine.files(tnit.files)
cnrat <- combine.files(cnrat.files)

image.plot(ocar[, , 1])
image.plot(ph[, , 1])
image.plot(tnit[, , 1])
image.plot(cnrat[, , 1])

dir.create(dirname(out.ocar))
dir.create(dirname(out.ph))
dir.create(dirname(out.tnit))
dir.create(dirname(out.cnrat))

saveRDS(ocar, out.ocar)
saveRDS(ph, out.ph)
saveRDS(tnit, out.tnit)
saveRDS(cnrat, out.cnrat)
