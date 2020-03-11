library(fields)
rm(list = ls())

# Input
soil.file <- "../../../Data/Primary/Nijssen2001/2deg/soil_cal_tran.merged.txt"
mapping.file <- "../../../Data/Primary/Nijssen2001/2deg/soil_cal_tran.merged.txt"
baseflow.out <- "./Saves/baseflow_Nijssen120min_30min_global.RDS"

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
d1.map <- array(NA, dim = c(length(lons.120min), length(lats.120min)))
d2.map <- array(NA, dim = c(length(lons.120min), length(lats.120min)))
d3.map <- array(NA, dim = c(length(lons.120min), length(lats.120min)))
d4.map <- array(NA, dim = c(length(lons.120min), length(lats.120min)))
for (i in 1:nrow(soil)) {
  x <- which(lons.120min == mapping[i, 3])
  y <- which(lats.120min == mapping[i, 2])

  d1.map[x, y] <- soil[i, 6]
  d2.map[x, y] <- soil[i, 7]
  d3.map[x, y] <- soil[i, 8]
  d4.map[x, y] <- soil[i, 9]
}
image.plot(d1.map)
image.plot(d2.map)
image.plot(d3.map)
image.plot(d4.map)

d1.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min)))
d2.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min)))
d3.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min)))
d4.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min)))
for (x in 1:dim(d1.map.adj)[1]) {
  for (y in 1:dim(d1.map.adj)[2]) {
    x.map <- mapping.map[x, y, 1]
    y.map <- mapping.map[x, y, 2]
    d1.map.adj[x, y] <- d1.map[x.map, y.map]
    d2.map.adj[x, y] <- d2.map[x.map, y.map]
    d3.map.adj[x, y] <- d3.map[x.map, y.map]
    d4.map.adj[x, y] <- d4.map[x.map, y.map]
  }
}
image.plot(d1.map.adj)
image.plot(d2.map.adj)
image.plot(d3.map.adj)
image.plot(d4.map.adj)

# Save
baseflow.out.tmp <- gsub(x = baseflow.out, pattern = "baseflow", replacement = "baseflowD1")
dir.create(dirname(baseflow.out.tmp))
saveRDS(d1.map.adj, baseflow.out.tmp)
baseflow.out.tmp <- gsub(x = baseflow.out, pattern = "baseflow", replacement = "baseflowD2")
dir.create(dirname(baseflow.out.tmp))
saveRDS(d2.map.adj, baseflow.out.tmp)
baseflow.out.tmp <- gsub(x = baseflow.out, pattern = "baseflow", replacement = "baseflowD3")
dir.create(dirname(baseflow.out.tmp))
saveRDS(d3.map.adj, baseflow.out.tmp)
baseflow.out.tmp <- gsub(x = baseflow.out, pattern = "baseflow", replacement = "baseflowD4")
dir.create(dirname(baseflow.out.tmp))
saveRDS(d4.map.adj, baseflow.out.tmp)
