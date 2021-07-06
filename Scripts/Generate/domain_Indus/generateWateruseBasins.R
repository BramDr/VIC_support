library(ncdf4)
library(fields)
rm(list = ls())

# Input
map.support.file <- "../../../Scripts/Support/mapFunctions.R"
generate.support.file <- "../../../Scripts/Support/generateFunctions.R"
downstream.file = "../../../Data/Transformed/Routing/downstream_5min_Indus.RDS"
path.area <- "../../../Data/Transformed/Routing/area_5min_Indus.RDS"
basin.file = "../../../Data/Transformed/Routing/basins_5min_Indus.RDS"
id.file = "../waterUse_Indus/Saves/idMap.RDS"
wateruse.1.file = "../waterUse_Indus/Saves/receivingCommand.RDS"
wateruse.2.file = "../waterUse_Indus/Saves/receivingDelta.RDS"
vic.orig = "../../../../Data/Primary/VIC/domain_global.nc"
domain.out <- "../../../Data/VIC/Parameters/Indus_5min/domain_Indus.nc"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

point <- data.frame(lat = 29.75, lon = 70.75, name = "Indus", stringsAsFactors = F)

# Load
source(map.support.file)
source(generate.support.file)

area <- readRDS(path.area)
basin <- readRDS(basin.file)
downstream <- readRDS(downstream.file)
id = readRDS(id.file)
wateruse.1 <- readRDS(wateruse.1.file)
wateruse.2 <- readRDS(wateruse.2.file)

# Setup
x <- which.min(abs(out.lons - point$lon))
y <- which.min(abs(out.lats - point$lat))
point$basin <- basin[x, y]

# Calculate
basin.mask = basin
basin.mask[basin == basin[x,y]] = 1
basin.mask[basin != basin[x,y]] = 0
image.plot(basin.mask)

wateruse.1.ids = unique(na.omit(c(wateruse.1)))
wateruse.2.ids = unique(na.omit(c(wateruse.2)))
basin.ids = unique(na.omit(c(id[!is.na(basin.mask) & basin.mask == 1])))
basin.ids = basin.ids[basin.ids %in% wateruse.1.ids | basin.ids %in% wateruse.2.ids]

wateruse.mask = basin.mask
wateruse.mask[!is.na(wateruse.1) & basin.mask == 0 & wateruse.1 %in% basin.ids] = 2
wateruse.mask[!is.na(wateruse.2) & basin.mask == 0 & wateruse.2 %in% basin.ids] = 2
image.plot(wateruse.mask)

expanded.mask = wateruse.mask
for (x in 1:dim(expanded.mask)[1]) {
  for (y in 1:dim(expanded.mask)[2]) {
    if (is.na(expanded.mask[x, y]) || expanded.mask[x, y] != 0) {
      next
    }
    
    cur <- c(x, y)
    nex <- downstream[x, y, ]
    
    while (TRUE) {
      if(expanded.mask[cur[1], cur[2]] > 0){
        expanded.mask[x,y] = 3
        break
      }
      
      if (cur[1] == nex[1] && cur[2] == nex[2]) {
        break
      }
      
      cur <- nex
      nex <- downstream[cur[1], cur[2], ]
    }
  }
}
image.plot(expanded.mask)

final.mask = expanded.mask
final.mask[final.mask > 0] = 1
final.mask[final.mask == 0] = NA
image.plot(final.mask)

# Create
dim.lon <- ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = out.lons,
  longname = "longitude of grid cell center"
)
dim.lat <- ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = out.lats,
  longname = "latitude of grid cell center"
)

var.area = cropyCreateVariable(base.file = vic.orig, base.name = "area",
                               dim = list(dim.lon, dim.lat), 
                               chunksizes = c(length(out.lons), length(out.lats)))
var.mask = cropyCreateVariable(base.file = vic.orig, base.name = "mask",
                               dim = list(dim.lon, dim.lat), 
                               chunksizes = c(length(out.lons), length(out.lats)))
var.frac = cropyCreateVariable(base.file = vic.orig, base.name = "frac",
                               dim = list(dim.lon, dim.lat), 
                               chunksizes = c(length(out.lons), length(out.lats)))

# Save
dir.create(dirname(domain.out))
nc <- nc_create(
  domain.out,
  list(
    var.area,
    var.mask,
    var.frac
  )
)
ncvar_put(nc, var.area, area)
ncvar_put(nc, var.mask, final.mask)
ncvar_put(nc, var.frac, final.mask)
nc_close(nc)

