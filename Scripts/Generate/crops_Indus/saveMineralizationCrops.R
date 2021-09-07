library(fields)
library(raster)
rm(list = ls())

map.support.file <- "../../Support/mapFunctions.R"
ocar.file <- "../../../Data/Transformed/Soil/ISRIC/ocar_5min_Indus.RDS"
ph.file <- "../../../Data/Transformed/Soil/ISRIC/ph_5min_Indus.RDS"
tnit.file <- "../../../Data/Transformed/Soil/ISRIC/tnit_5min_Indus.RDS"
cnrat.file <- "../../../Data/Transformed/Soil/ISRIC/cnrat_5min_Indus.RDS"

carbon.out <- "./Saves/carbon_crops_5min_Indus.RDS"
ph.out <- "./Saves/ph_crops_5min_Indus.RDS"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
source(map.support.file)
ocar <- readRDS(ocar.file)
ph <- readRDS(ph.file)
tnit <- readRDS(tnit.file)
cnrat <- readRDS(cnrat.file)

# Setup
ocar.adj = ocar
ph.adj = ph
tnit.adj = tnit
cnrat.adj = cnrat

## Fill spatial
na.map = is.na(array(0, dim = dim(ph)[1:2]))
ocar.adj = fillMap(ocar.adj, na.map, getNearestMean)
ph.adj = fillMap(ph.adj, na.map, getNearestMean)
tnit.adj = fillMap(tnit.adj, na.map, getNearestMean)
cnrat.adj = fillMap(cnrat.adj, na.map, getNearestMean)

trans.map <- function(map) {
  map.t <- t(map[dim(map)[1]:1, ])
  return(map.t)
}
ocar.adj = trans.map(ocar.adj)
ph.adj = trans.map(ph.adj)
tnit.adj = trans.map(tnit.adj)
cnrat.adj = trans.map(cnrat.adj)

dir.create(dirname(carbon.out))
dir.create(dirname(ph.out))

saveRDS(ocar.adj, carbon.out)
saveRDS(ph.adj, ph.out)
