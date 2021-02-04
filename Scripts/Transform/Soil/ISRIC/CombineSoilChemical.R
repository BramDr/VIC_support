library(fields)
library(raster)
rm(list = ls())

gen.dir <- "./Saves"
out.ocar <- "../../../../Data/Transformed/Soil/ISRIC/ocar_30min_global.RDS"
out.ph <- "../../../../Data/Transformed/Soil/ISRIC/ph_30min_global.RDS"
out.tnit <- "../../../../Data/Transformed/Soil/ISRIC/tnit_30min_global.RDS"
out.cnrat <- "../../../../Data/Transformed/Soil/ISRIC/cnrat_30min_global.RDS"

ocar.files <- list.files(gen.dir, pattern = "ocar_.*30min_global", full.names = T)
ph.files <- list.files(gen.dir, pattern = "ph_.*30min_global", full.names = T)
tnit.files <- list.files(gen.dir, pattern = "tnit_.*30min_global", full.names = T)
cnrat.files <- list.files(gen.dir, pattern = "cnrat_.*30min_global", full.names = T)

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
