library(fields)
library(raster)
rm(list = ls())

map.support.file <- "../../../../Scripts/Support/mapFunctions.R"
clay.file <- "../../../../Data/Transformed/Soil/ISRIC/clay_30min_global.RDS"
sand.file <- "../../../../Data/Transformed/Soil/ISRIC/sand_30min_global.RDS"
silt.file <- "../../../../Data/Transformed/Soil/ISRIC/silt_30min_global.RDS"
bulk.file <- "../../../../Data/Transformed/Soil/ISRIC/bulk_30min_global.RDS"

quartz.out <- "../../../../Data/Transformed/Soil/ISRIC/quartz_30min_global.RDS"
expt.out <- "../../../../Data/Transformed/Soil/ISRIC/expt_30min_global.RDS"
ksat.out <- "../../../../Data/Transformed/Soil/ISRIC/ksat_30min_global.RDS"
bulk.dens.out <- "../../../../Data/Transformed/Soil/ISRIC/bulk_dens_30min_global.RDS"
Wfc.out <- "../../../../Data/Transformed/Soil/ISRIC/Wfc_30min_global.RDS"
Wcr.out <- "../../../../Data/Transformed/Soil/ISRIC/Wcr_30min_global.RDS"
Wpwp.out <- "../../../../Data/Transformed/Soil/ISRIC/Wpwp_30min_global.RDS"
bubble.out <- "../../../../Data/Transformed/Soil/ISRIC/bubble_30min_global.RDS"
max.moist.out <- "../../../../Data/Transformed/Soil/ISRIC/max_moist_30min_global.RDS"
resid.moist.out <- "../../../../Data/Transformed/Soil/ISRIC/resid_moist_30min_global.RDS"

# Load
source(map.support.file)
clay <- readRDS(clay.file)
sand <- readRDS(sand.file)
silt <- readRDS(silt.file)
bulk <- readRDS(bulk.file)

# Setup
clay.adj = clay
sand.adj = sand
silt.adj = silt
bulk.adj = bulk

## Fill lower layers
clay.last = clay.adj[,,1]
sand.last = sand.adj[,,1]
silt.last = silt.adj[,,1]
bulk.last = bulk.adj[,,1]
for(z in 1:dim(clay)[3]){
  clay.tmp = clay.adj[,,z]
  sand.tmp = sand.adj[,,z]
  silt.tmp = silt.adj[,,z]
  bulk.tmp = bulk.adj[,,z]
  
  sel = is.na(clay.tmp)
  clay.tmp[sel] = clay.last[sel]
  sel = is.na(sand.tmp)
  sand.tmp[sel] = sand.last[sel]
  sel = is.na(silt.tmp)
  silt.tmp[sel] = silt.last[sel]
  sel = is.na(bulk.tmp)
  bulk.tmp[sel] = bulk.last[sel]
    
  clay.last[!sel] = clay.tmp[!sel]
  clay.adj[,,z] = clay.tmp
  sand.last[!sel] = sand.tmp[!sel]
  sand.adj[,,z] = sand.tmp
  silt.last[!sel] = silt.tmp[!sel]
  silt.adj[,,z] = silt.tmp
  bulk.last[!sel] = bulk.tmp[!sel]
  bulk.adj[,,z] = bulk.tmp
}

## Scale known soils
for(z in 1:dim(clay)[3]){
  clay.tmp = clay.adj[,,z]
  sand.tmp = sand.adj[,,z]
  silt.tmp = silt.adj[,,z]
  
  sum = clay.tmp + sand.tmp + silt.tmp
  
  sel = !is.na(sum) & sum > 33
  clay.tmp[sel] = clay.tmp[sel] / (sum[sel] / 100)
  sand.tmp[sel] = sand.tmp[sel] / (sum[sel] / 100)
  silt.tmp[sel] = silt.tmp[sel] / (sum[sel] / 100)
  
  sel = !is.na(sum) & sum <= 33
  clay.tmp[sel] = NA
  sand.tmp[sel] = NA
  silt.tmp[sel] = NA
  
  clay.adj[,,z] = clay.tmp
  sand.adj[,,z] = sand.tmp
  silt.adj[,,z] = silt.tmp
}

# Fill spatial
na.map = is.na(clay[,,1])
clay.adj = fillMap(clay.adj, na.map, getNearestMean)
sand.adj = fillMap(sand.adj, na.map, getNearestMean)
silt.adj = fillMap(silt.adj, na.map, getNearestMean)
bulk.adj = fillMap(bulk.adj, na.map, getNearestMean)

image.plot(clay.adj[,,1])
image.plot(clay.adj[,,7])

# Calculate
calc.saxton <- function(clay, silt, sand) {
  clay[clay < 1] = 1
  silt[silt < 1] = 1
  sand[sand < 1] = 1
  maps <- list()

  maps[["a"]] <- exp(-4.396 + clay * -0.0715 + sand^2 * -4.880e-4 + sand^2 * clay * -4.285e-5) * 100.0
  maps[["b"]] <- -3.140 + (clay^2) * -0.00222 + (sand^2) * clay * -3.484e-5
  maps[["vs"]] <- 0.332 + sand * -7.251e-4 + log(clay, base = 10) * 0.1276
  maps[["vs"]][maps[["vs"]] <= 0] <- NA
  maps[["ys"]] <- 100.0 * (-0.108 + maps[["vs"]] * 0.341)
  maps[["ks"]] <- 2.778e-6 * exp(12.012 + sand * -0.0755 + ((-3.8950 + sand * 0.03671 + clay * -0.1103 + (clay^2) * 8.7546e-4) / maps[["vs"]]))

  return(maps)
}

saxton <- calc.saxton(clay.adj, silt.adj, sand.adj)

trans.map <- function(map) {
  map.t <- array(NA, dim = c(dim(map)[2], dim(map)[1], dim(map)[3]))
  for (z in 1:dim(map)[3]) {
    map.t[, , z] <- t(map[dim(map)[1]:1, , z])
  }
  return(map.t)
}
agg.layers <- function(maps) {
  agg.map <- array(NA, dim = c(360, 720, 3))
  maps[, , 2] <- maps[, , 2] * 0.5
  agg.map[, , 1] <- apply(X = maps[, , 1:2], MARGIN = c(1, 2), FUN = sum) / 1.5
  agg.map[, , 2] <- apply(X = maps[, , 2:5], MARGIN = c(1, 2), FUN = sum) / 3.5
  agg.map[, , 3] <- agg.map[, , 2]
  agg.map[agg.map == 0] <- NA
  return(trans.map(agg.map))
}
plot.layers <- function(map) {
  for (z in 1:dim(map)[3]) {
    image.plot(map[, , z], main = paste0("l = ", z))
  }
}

quartz <- sand.adj / 100
quartz <- agg.layers(quartz)
# plot.layers(quartz)

expt <- 2 * -saxton$b + 3
expt <- agg.layers(expt)
# plot.layers(expt)

ksat <- saxton$ks * 1000 * 60 * 60 * 24
ksat <- agg.layers(ksat)
# plot.layers(ksat)

resid.moist <- (1000000 / saxton$a)^(1 / saxton$b)
Wpwp <- (1500 / saxton$a)^(1 / saxton$b)
Wcr <- (100 / saxton$a)^(1 / saxton$b)
Wfc <- (33 / saxton$a)^(1 / saxton$b)
max.moist <- saxton$vs

resid.moist <- agg.layers(resid.moist)
max.moist <- agg.layers(max.moist)
# plot.layers(resid.moist)
# plot.layers(max.moist)

Wpwp <- agg.layers(Wpwp)
Wcr <- agg.layers(Wcr)
Wfc <- agg.layers(Wfc)

# Correct fractions
sel = !is.na(Wpwp) & !is.na(max.moist) & Wpwp > max.moist
Wpwp[sel] = max.moist[sel]
sel = !is.na(Wcr) & !is.na(max.moist) & Wcr > max.moist
Wcr[sel] = max.moist[sel]
sel = !is.na(Wfc) & !is.na(max.moist) & Wfc > max.moist
Wfc[sel] = max.moist[sel]

sel = !is.na(Wpwp) & !is.na(resid.moist) & Wpwp < resid.moist
Wpwp[sel] = resid.moist[sel] + 1e-6
sel = !is.na(Wcr) & !is.na(Wpwp) & Wcr < Wpwp
Wcr[sel] = Wpwp[sel] + 1e-6
sel = !is.na(Wfc) & !is.na(Wcr) & Wfc < Wcr
Wfc[sel] = Wcr[sel] + 1e-6

Wpwp <- Wpwp / max.moist
Wcr <- Wcr / max.moist
Wfc <- Wfc / max.moist
# plot.layers(Wpwp)
# plot.layers(Wcr)
# plot.layers(Wfc)

bulk.dens <- agg.layers(bulk.adj * 1e3)
# plot.layers(bulk.dens)

bubble <- saxton$ys / 0.0980665
bubble[bubble < 0] <- 0
bubble <- agg.layers(bubble)
# plot.layers(bubble)

dir.create(dirname(quartz.out))
dir.create(dirname(expt.out))
dir.create(dirname(ksat.out))
dir.create(dirname(Wfc.out))
dir.create(dirname(Wcr.out))
dir.create(dirname(Wpwp.out))
dir.create(dirname(bulk.dens.out))
dir.create(dirname(bubble.out))
dir.create(dirname(max.moist.out))
dir.create(dirname(resid.moist.out))

saveRDS(quartz, quartz.out)
saveRDS(expt, expt.out)
saveRDS(ksat, ksat.out)
saveRDS(Wfc, Wfc.out)
saveRDS(Wcr, Wcr.out)
saveRDS(Wpwp, Wpwp.out)
saveRDS(bulk.dens, bulk.dens.out)
saveRDS(bubble, bubble.out)
saveRDS(max.moist, max.moist.out)
saveRDS(resid.moist, resid.moist.out)
