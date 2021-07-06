rm(list = ls())
library(fields)
library(raster)

# Input
area.file <- "../../../Data/Primary/MIRCA2000/Cell area grid/cell_area_ha_05mn.asc"
scc.file <- "../../../Data/Primary/MIRCA2000/Growing periods listed/cropping_calendars_5min.txt"
scc.out <- "./Saves/subcropCalendar_5min_Indus.RDS"
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
scc <- read.table(scc.file, stringsAsFactors = F, header = T)

# Setup
area = crop(area, extent.out)
area = as.matrix(area)
area = t(area[nrow(area):1,])

# Calculate
scc.sel = scc[scc$long < max(out.lons) + resolution / 2 &
              scc$long > min(out.lons) - resolution / 2 &
              scc$lat < max(out.lats) + resolution / 2 &
              scc$lat > min(out.lats) - resolution / 2,]

scc.sel2 = scc.sel[scc.sel$area > mean(area) * 1e-3, ] # Omit areas < 0.1% of gridcell

sum.diff = sum(scc.sel$area) - sum(scc.sel2$area)
sum.rel = sum.diff / sum(scc.sel$area) * 100
print(paste0("Changed areas by ", sum.rel, "% - ", sum.diff, "ha"))

# Save
dir.create(dirname(scc.out))
saveRDS(scc.sel2, scc.out)
