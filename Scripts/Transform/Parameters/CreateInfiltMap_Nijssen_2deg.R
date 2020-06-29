library(fields)
rm(list = ls())

# Input
soil.file <- "../../../Data/Primary/Nijssen2001/2deg/soil_cal_tran.merged.txt"
mapping.file <- "../../../Data/Primary/Nijssen2001/2deg/soil_cal_tran.merged.txt"
infilt.out <- "../../../Data/Transformed/Parameters/infilt_Nijssen120min_30min_global.RDS"

# Load
soil <- read.table(file = soil.file)
mapping <- read.table(file = mapping.file)
mapping <- mapping[, 2:4]

# Setup
lons.120min <- seq(from = -179, to = 179, by = 2)
lats.120min <- seq(from = -89, to = 89, by = 2)
lons.30min <- seq(from = -179.75, to = 179.75, by = 0.5)
lats.30min <- seq(from = -89.75, to = 89.75, by = 0.5)

mapping.map <- array(NA, dim = c(length(lons.30min), length(lats.30min), 2))
for (x in 1:dim(mapping.map)[1]) {
  for (y in 1:dim(mapping.map)[2]) {
    x.map <- which.min(abs(lons.120min - lons.30min[x]))
    y.map <- which.min(abs(lats.120min - lats.30min[y]))
    mapping.map[x, y, 1] <- x.map
    mapping.map[x, y, 2] <- y.map
  }
}
image.plot(mapping.map[, , 1])
image.plot(mapping.map[, , 2])

# Calculate
infilt.map <- array(NA, dim = c(length(lons.120min), length(lats.120min)))
for (i in 1:nrow(soil)) {
  x <- which(lons.120min == mapping[i, 3])
  y <- which(lats.120min == mapping[i, 2])

  infilt.map[x, y] <- soil[i, 5]
}
image.plot(infilt.map)

infilt.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min)))
for (x in 1:dim(infilt.map.adj)[1]) {
  for (y in 1:dim(infilt.map.adj)[2]) {
    x.map <- mapping.map[x, y, 1]
    y.map <- mapping.map[x, y, 2]
    infilt.map.adj[x, y] <- infilt.map[x.map, y.map]
  }
}
image.plot(infilt.map.adj)

# Save
dir.create(dirname(infilt.out))
saveRDS(infilt.map.adj, infilt.out)
