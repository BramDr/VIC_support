library(fields)
rm(list = ls())

# Input
root.file <- "../../../Data/Primary/Lin2019/global_lai_0.25deg.txt"
mapping.file <- "../../../Data/Primary/Lin2019/global_VIC_soil_0.25d.txt"
root.depth.out <- "../../../Data/Transformed/Parameters/rootDepth_Lin15min_30min_global.RDS"
root.fract.out <- "../../../Data/Transformed/Parameters/rootFract_Lin15min_30min_global.RDS"

# Load
root.text <- readLines(con = root.file)

mapping <- read.table(mapping.file)
mapping[mapping[, 4] > 180, 4] <- mapping[mapping[, 4] > 180, 4] - 360

# Setup
lons.15min <- seq(from = -179.875, to = 179.875, by = 0.25)
lats.15min <- seq(from = -89.875, to = 89.875, by = 0.25)
lons.30min <- seq(from = -179.75, to = 179.75, by = 0.5)
lats.30min <- seq(from = -89.75, to = 89.75, by = 0.5)
nclasses <- 12
nroot <- 3

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
  for (i in 1:dim(map)[3]) {
    for (j in 1:dim(map)[4]) {
      mapi <- map[, , i, j]
      mapi <- mapi / count[, , i]
      mapi[count[, , i] == 0] <- NA
      map[, , i, j] <- mapi
    }
  }
  return(map)
}

# Calculate
root.depth.map <- array(0, dim = c(length(lons.30min), length(lats.30min), nclasses, nroot))
root.fract.map <- array(0, dim = c(length(lons.30min), length(lats.30min), nclasses, nroot))
count.map <- array(0, dim = c(length(lons.30min), length(lats.30min), nclasses))
mapping.row <- FALSE
root.row <- FALSE
for (i in 1:length(root.text)) {
  root.line <- root.text[i]
  root.fields <- strsplit(x = root.line, split = " ")[[1]]
  root.fields <- as.numeric(root.fields)

  if (!mapping.row) {
    cell <- root.fields[1]
    nveg <- root.fields[2]
    row <- which(mapping[, 2] == cell)

    x.15min <- which(lons.15min == mapping[row, 4])
    y.15min <- which(lats.15min == mapping[row, 3])

    x <- mapping.map[x.15min, y.15min, 1]
    y <- mapping.map[x.15min, y.15min, 2]

    if (nveg == 0) {
      next
    }

    j <- 1
    mapping.row <- TRUE
  } else {
    if (!root.row) {
      veg_class <- root.fields[1]
      Cv <- root.fields[2]
      root_depth <- c(root.fields[3], root.fields[5])
      root_fract <- c(root.fields[4], root.fields[6])

      root.depth.map[x, y, veg_class, 1:2] <- root.depth.map[x, y, veg_class, 1:2] + root_depth
      root.fract.map[x, y, veg_class, 1:2] <- root.fract.map[x, y, veg_class, 1:2] + root_fract
      count.map[x, y, veg_class] <- count.map[x, y, veg_class] + 1

      root.row <- TRUE
    } else {
      root.row <- FALSE
      if (j >= nveg) {
        mapping.row <- FALSE
      }
      j <- j + 1
    }
  }
}

root.depth.map.adj <- averageMap(root.depth.map, count.map)
root.fract.map.adj <- averageMap(root.fract.map, count.map)

for (i in 1:(nclasses - 1)) {
  image.plot(root.depth.map.adj[, , i, 1], main = i)
  image.plot(root.fract.map.adj[, , i, 1], main = i)
}

# Save
dir.create(dirname(root.depth.out))
saveRDS(root.depth.map.adj, root.depth.out)
dir.create(dirname(root.fract.out))
saveRDS(root.fract.map.adj, root.fract.out)
