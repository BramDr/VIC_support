rm(list = ls())
library(fields)
library(raster)

# Input
area.file = "../../../../Data/Primary/MIRCA2000/Cell area grid/cell_area_ha_05mn.asc"
cropland.grid.file <- "./Saves/croplandMIRCA_grid_monthly_5min_Indus.RDS"
cropland.files <- c("./Saves/croplandMIRCA_rainfed_monthly_5min_Indus.RDS",
                    "./Saves/croplandMIRCA_irrigated_monthly_5min_Indus.RDS",
                    "./Saves/croplandMIRCA_paddy_monthly_5min_Indus.RDS",
                    "./Saves/croplandMIRCA_wheatRainfed_monthly_5min_Indus.RDS",
                    "./Saves/croplandMIRCA_wheatIrrigated_monthly_5min_Indus.RDS",
                    "./Saves/croplandMIRCA_riceRainfed_monthly_5min_Indus.RDS",
                    "./Saves/croplandMIRCA_riceIrrigated_monthly_5min_Indus.RDS")
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

# Load
area <- raster(area.file)

cropland.grid = readRDS(cropland.grid.file)

# Setup
area = crop(area, extent.out)
area = as.matrix(area)
area = t(area[nrow(area):1,])

coverage.grid = cropland.grid / area
coverage.cor = coverage.grid
coverage.cor[coverage.cor < 1] = 1
coverage.grid[coverage.grid > 1] = 1
image.plot(coverage.cor)

cropland.file = cropland.files[1]
for(cropland.file in cropland.files){
  cropland = readRDS(cropland.file)
  
  coverage = cropland
  for(z in 1:dim(coverage)[3]){
    coverage[,,z] = cropland[,,z] / area / coverage.cor
  }
  
  coverage[coverage > 1] = 1
  
  coverage.out = gsub(cropland.file, pattern = "cropland", replacement = "coverage")
  dir.create(dirname(coverage.out))
  saveRDS(coverage, coverage.out)
}

coverage.out = gsub(cropland.grid.file, pattern = "cropland", replacement = "coverage")
dir.create(dirname(coverage.out))
saveRDS(coverage.grid, coverage.out)
