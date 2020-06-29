library(fields)
rm(list = ls())

# Input
soil.file <- "../../../Data/Primary/Lin2019/global_VIC_soil_0.25d.txt"
infilt.out <- "../../../Data/Transformed/Parameters/infilt_Lin15min_30min_global.RDS"

# Load
soil <- read.table(file = soil.file)
soil[soil[, 4] > 180, 4] <- soil[soil[, 4] > 180, 4] - 360

# Setup
lons.15min <- seq(from = -179.875, to = 179.875, by = 0.25)
lats.15min <- seq(from = -89.875, to = 89.875, by = 0.25)
lons.30min <- seq(from = -179.75, to = 179.75, by = 0.5)
lats.30min <- seq(from = -89.75, to = 89.75, by = 0.5)

mapping.map <- array(NA, dim = c(length(lons.15min), length(lats.15min), 2))
for (x in 1:dim(mapping.map)[1]) {
  for (y in 1:dim(mapping.map)[2]) {
    x.map <- which.min(abs(lons.30min - lons.15min[x]))
    y.map <- which.min(abs(lats.30min - lats.15min[y]))
    mapping.map[x, y, 1] <- x.map
    mapping.map[x, y, 2] <- y.map
  }
}
# image.plot(mapping.map[, , 1])
# image.plot(mapping.map[, , 2])

averageMap <- function(map, count) {
  mapi <- map
  mapi <- mapi / count
  mapi[count == 0] <- NA
  map <- mapi
  return(map)
}

# Calculate
infilt.map <- array(0, dim = c(length(lons.30min), length(lats.30min)))
count.map <- array(0, dim = c(length(lons.30min), length(lats.30min)))
for (i in 1:nrow(soil)) {
  x.15min <- which(lons.15min == soil[i, 4])
  y.15min <- which(lats.15min == soil[i, 3])

  x <- mapping.map[x.15min, y.15min, 1]
  y <- mapping.map[x.15min, y.15min, 2]

  infilt.map[x, y] <- infilt.map[x, y] + soil[i, 5]
  count.map[x, y] <- count.map[x, y] + 1
}

infilt.map.adj <- averageMap(infilt.map, count.map)

image.plot(infilt.map.adj)

# Save
dir.create(dirname(infilt.out))
saveRDS(infilt.map.adj, infilt.out)
