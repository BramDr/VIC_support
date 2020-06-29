library(fields)
rm(list = ls())

# Input
cv.file <- "../../../Data/Primary/Lin2019/global_lai_0.25deg.txt"
mapping.file <- "../../../Data/Primary/Lin2019/global_VIC_soil_0.25d.txt"
cv.out <- "../../../Data/Transformed/Parameters/cv_Lin15min_30min_global.RDS"

# Load
cv.text <- readLines(con = cv.file)

mapping <- read.table(mapping.file)
mapping[mapping[, 4] > 180, 4] <- mapping[mapping[, 4] > 180, 4] - 360

# Setup
lons.15min <- seq(from = -179.875, to = 179.875, by = 0.25)
lats.15min <- seq(from = -89.875, to = 89.875, by = 0.25)
lons.30min <- seq(from = -179.75, to = 179.75, by = 0.5)
lats.30min <- seq(from = -89.75, to = 89.75, by = 0.5)
nclasses <- 12

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
    mapi <- map[, , i]
    mapi <- mapi / count[, , i]
    mapi[count[, , i] == 0] <- NA
    map[, , i] <- mapi
  }
  return(map)
}

# Calculate
cv.map <- array(0, dim = c(length(lons.30min), length(lats.30min), nclasses))
count.map <- array(0, dim = c(length(lons.30min), length(lats.30min), nclasses))
mapping.row <- FALSE
lai.row <- FALSE
for (i in 1:length(cv.text)) {
  cv.line <- cv.text[i]
  cv.fields <- strsplit(x = cv.line, split = " ")[[1]]
  cv.fields <- as.numeric(cv.fields)

  if (!mapping.row) {
    cell <- cv.fields[1]
    nveg <- cv.fields[2]
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
    if (!lai.row) {
      veg_class <- cv.fields[1]
      Cv <- cv.fields[2]
      root_depth <- c(cv.fields[3], cv.fields[5])
      root_fract <- c(cv.fields[4], cv.fields[6])

      cv.map[x, y, veg_class] <- cv.map[x, y, veg_class] + Cv
      count.map[x, y, veg_class] <- count.map[x, y, veg_class] + 1

      lai.row <- TRUE
    } else {
      lai.row <- FALSE
      if (j >= nveg) {
        mapping.row <- FALSE
      }
      j <- j + 1
    }
  }
}

cv.map.adj <- averageMap(cv.map, count.map)

for (i in 1:(nclasses - 1)) {
  image.plot(cv.map.adj[, , i], main = i)
}
cv.sum <- apply(cv.map.adj, c(1, 2), sum, na.rm = T)
image.plot(cv.sum)

cv.adj.map <- cv.map.adj
for (i in 1:(nclasses - 1)) {
  cv.tmp <- cv.map.adj[, , i]
  sel <- cv.sum > 1
  cv.tmp[sel] <- cv.tmp[sel] / cv.sum[sel]
  cv.adj.map[, , i] <- cv.tmp
}

cv.tmp <- cv.map.adj[, , 12]
sel <- cv.sum < 1
cv.tmp[sel] <- 1 - cv.sum[sel]
cv.adj.map[, , 12] <- cv.tmp

cv.sum <- apply(cv.adj.map, c(1, 2), sum, na.rm = T)
image.plot(cv.sum)

# Save
dir.create(dirname(cv.out))
saveRDS(cv.adj.map, cv.out)
