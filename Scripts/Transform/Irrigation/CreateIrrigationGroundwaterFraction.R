library(fields)
library(ncdf4)
rm(list = ls())

# Input
gw.file <- "../../../Data/Primary/Doll2012/G_FRACTGW_IRRIG.txt"
id.file <- "../../../Data/Primary/Doll2012/Arc_ID_lon_lat_continentalarea.txt"
gw.out <- "../../../Data/Transformed/Irrigation/irrigationGroundwaterFraction_30min_global.RDS"

# Load
id <- read.table(id.file, header = TRUE)
gw <- read.table(gw.file, header = TRUE)

# Setup
lats <- seq(-89.75, 89.75, by = 0.5)
lons <- seq(-179.75, 179.75, by = 0.5)

# Calculate
gw.map <- array(NA, dim = c(length(lons), length(lats)))
for (i in 1:nrow(gw)) {
  arc.id <- gw$Arc_ID[i]
  arc.row <- which(id$Arc_ID == arc.id)
  lat <- id$lat[arc.row]
  lon <- id$lon[arc.row]
  y <- which(lats == lat)
  x <- which(lons == lon)

  gw.map[x, y] <- gw$VALUE[i]
}
image.plot(gw.map)

# Save
dir.create(dirname(gw.out))
saveRDS(gw.map, gw.out)
