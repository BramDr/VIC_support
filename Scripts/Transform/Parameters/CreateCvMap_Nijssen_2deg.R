library(fields)
rm(list = ls())

# Input
cv.file <- "../../../Data/Primary/Nijssen2001/2deg/world_veg_parameters.0.01_threshold.txt"
mapping.file <- "../../../Data/Primary/Nijssen2001/2deg/soil_cal_tran.merged.txt"
cv.out <- "../../../Data/Transformed/Parameters/cv_Nijssen120min_30min_global.RDS"

# Load
cv.text <- readLines(con = cv.file)
mapping <- read.table(mapping.file)
mapping <- mapping[, 2:4]

# Setup
lons.120min <- seq(from = -179, to = 179, by = 2)
lats.120min <- seq(from = -89, to = 89, by = 2)
lons.30min <- seq(from = -179.75, to = 179.75, by = 0.5)
lats.30min <- seq(from = -89.75, to = 89.75, by = 0.5)
nclasses <- 12
nmonths <- 12

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
cv.map <- array(NA, dim = c(length(lons.120min), length(lats.120min), nclasses))
mapping.row <- FALSE
lai.row <- FALSE
for (i in 1:length(cv.text)) {
  cv.line <- cv.text[i]
  cv.fields <- strsplit(x = cv.line, split = " ")[[1]]
  cv.fields <- as.numeric(cv.fields)

  if (!mapping.row) {
    cell <- cv.fields[1]
    nveg <- cv.fields[2]
    row <- which(mapping[, 1] == cell)
    x <- which(lons.120min == mapping[row, 3])
    y <- which(lats.120min == mapping[row, 2])

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

      cv.map[x, y, veg_class] <- Cv

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
for (i in 1:(nclasses - 1)) {
  image.plot(cv.map[, , i], main = i)
}

cv.sum <- apply(cv.map, c(1, 2), sum, na.rm = T)
image.plot(cv.sum)

cv.adj.map <- cv.map
for (i in 1:(nclasses - 1)) {
  cv.tmp <- cv.map[, , i]
  sel <- cv.sum > 1
  cv.tmp[sel] <- cv.tmp[sel] / cv.sum[sel]
  cv.adj.map[, , i] <- cv.tmp
}

cv.tmp <- cv.map[, , 12]
sel <- cv.sum < 1
cv.tmp[sel] <- 1 - cv.sum[sel]
cv.adj.map[, , 12] <- cv.tmp

cv.sum <- apply(cv.adj.map, c(1, 2), sum, na.rm = T)
image.plot(cv.sum)

cv.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min), nclasses))
for (x in 1:dim(cv.map.adj)[1]) {
  for (y in 1:dim(cv.map.adj)[2]) {
    x.map <- mapping.map[x, y, 1]
    y.map <- mapping.map[x, y, 2]
    cv.map.adj[x, y, ] <- cv.adj.map[x.map, y.map, ]
  }
}
for (i in 1:nclasses) {
  image.plot(cv.map.adj[, , i], main = i)
}

# Save
dir.create(dirname(cv.out))
saveRDS(cv.map.adj, cv.out)
