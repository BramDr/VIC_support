library(fields)
rm(list = ls())

# Input
cv.file <- "../../../Data/Primary/Nijssen2001/0.5deg/global_veg_param_new"
mapping.file <- "../../../Data/Primary/Nijssen2001/0.5deg/global_soil_param_new"
cv.out <- "../../../Data/Transformed/Parameters/cv_Nijssen30min_30min_global.RDS"

# Load
cv.text <- readLines(con = cv.file)
mapping <- read.table(mapping.file)

# Setup
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
nclasses <- 12
nmonths <- 12

# Calculate
cv.map <- array(NA, dim = c(length(lons), length(lats), nclasses))
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
    x <- which(lons == mapping[row, 4])
    y <- which(lats == mapping[row, 3])

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

# Save
dir.create(dirname(cv.out))
saveRDS(cv.adj.map, cv.out)
