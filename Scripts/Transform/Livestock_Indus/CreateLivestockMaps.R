library(raster)
library(fields)
rm(list = ls())

livestock.dir <- "../../../Data/Primary/FAO/GLW/"
livestock.out <- "../../../Data/Transformed/Livestock/livestock_5min_Indus.RDS"

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
livestock.files <- list.files(livestock.dir, pattern = ".tif", full.names = T)
print(livestock.files)

# Setup
save.livestock = function(file){
  livestock = raster(file)
  livestock = crop(livestock, extent.out)
  
  livestock = as.matrix(livestock)
  livestock = t(livestock[nrow(livestock):1,])
  
  return(livestock)
}

livestock = list()
livestock$cattle = save.livestock(grep(x = livestock.files, pattern = "Ct_.*_Da.tif$", value = T))
livestock$buffalos = save.livestock(grep(x = livestock.files, pattern = "Bf_.*_Da.tif$", value = T))
livestock$ducks = save.livestock(grep(x = livestock.files, pattern = "Dk_.*_Da.tif$", value = T))
livestock$chickens = save.livestock(grep(x = livestock.files, pattern = "Ch_.*_Da.tif$", value = T))
livestock$goats = save.livestock(grep(x = livestock.files, pattern = "Gt_.*_Da.tif$", value = T))
livestock$sheep = save.livestock(grep(x = livestock.files, pattern = "Sh_.*_Da.tif$", value = T))
livestock$horses = save.livestock(grep(x = livestock.files, pattern = "Ho_.*_Da.tif$", value = T))
livestock$pigs = save.livestock(grep(x = livestock.files, pattern = "Pg_.*_Da.tif$", value = T))

# Save
dir.create(dirname(livestock.out))
saveRDS(livestock, livestock.out)
