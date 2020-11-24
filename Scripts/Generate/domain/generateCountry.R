library(ncdf4)
library(fields)
rm(list = ls())

# Input
path.cell <- "../../../Data/Transformed/Country/country_30min_cellFractions.RDS"
path.domain <- "../../../Data/Primary/VIC/domain_global.nc"
dir.out <- "../../../Data/VIC/Parameters/"

# Load
cell <- readRDS(path.cell)

# Setup
lon <- seq(from = -179.75, to = 179.75, by = 0.5)
lat <- seq(from = -89.75, to = 89.75, by = 0.5)
combine <- FALSE

points <- data.frame(lat = numeric(), lon = numeric(), name = character(), stringsAsFactors = F)
# points[nrow(points) + 1, ] <- c(-37.75, -66.25, "Argentina")
# points[nrow(points) + 1, ] <- c(34.25, 105.75, "China")
# points[nrow(points) + 1, ] <- c(23.25, 77.75, "India")
# points[nrow(points) + 1, ] <- c(28.75, 30.75, "Egypt")
# points[nrow(points) + 1, ] <- c(46.25, 25.25, "Romania")
points[nrow(points) + 1, ] <- c(-9.75, -75.75, "Peru")

points$lat <- as.numeric(points$lat)
points$lon <- as.numeric(points$lon)

# Calculate
for (i in 1:nrow(points)) {
  x <- which.min(abs(lon - points$lon[i]))
  y <- which.min(abs(lat - points$lat[i]))

  id <- NA
  done <- F
  for (j in 1:length(cell)) {
    country <- names(cell)[j]
    cell.country <- cell[[j]]

    if (nrow(cell.country) == 0) {
      next
    }

    for (k in 1:nrow(cell.country)) {
      if (cell.country$x[k] == x && cell.country$y[k] == y) {
        id <- country
        break
      }
    }
    if (done) {
      break
    }
  }

  if (!is.na(id)) {
    points$country[i] <- id
  } else {
    print(paste0("Point ", points[i, ], " falls outside of country mask"))
  }
}

mask <- array(NA, dim = c(length(lon), length(lat), nrow(points)))
for (i in 1:nrow(points)) {
  country <- as.character(points$country[i])
  cell.country <- cell[[country]]
  for (j in 1:nrow(cell.country)) {
    x <- cell.country$x[j]
    y <- cell.country$y[j]
    mask[x, y, i] <- 1
  }
}

mask.combine <- array(NA, dim = c(dim(mask)[1:2]))
for (x in 1:dim(mask)[1]) {
  for (y in 1:dim(mask)[2]) {
    for (i in 1:nrow(points)) {
      if (is.na(mask[x, y, i])) {
        next
      }

      mask.combine[x, y] <- mask[x, y, i]
      break
    }
  }
}
image.plot(mask.combine)

# Save
if (combine) {
  newname <- paste0(dir.out, "/", paste0(points$name, collapse = ""), "/", "domain_", paste0(points$name, collapse = ""), ".nc")

  x.index <- which(!is.nan(apply(mask.combine, c(1), mean, na.rm = T)))
  y.index <- which(!is.nan(apply(mask.combine, c(2), mean, na.rm = T)))

  min.x <- min(x.index)
  max.x <- max(x.index)
  min.y <- min(y.index)
  max.y <- max(y.index)

  dir.create(dirname(newname))
  system(paste0("ncks -F -O -d lat,", min.y, ",", max.y, " -d lon,", min.x, ",", max.x, " ", path.domain, " ", newname))

  nc <- nc_open(newname, write = T)
  ncvar_put(nc, nc$var$mask, mask.combine[min.x:max.x, min.y:max.y])
  nc_close(nc)
} else {
  for (i in 1:nrow(points)) {
    newname <- paste0(dir.out, "/", points$name[i], "/", "domain_", points$name[i], ".nc")

    x.index <- which(!is.nan(apply(mask[, , i], c(1), mean, na.rm = T)))
    y.index <- which(!is.nan(apply(mask[, , i], c(2), mean, na.rm = T)))

    min.x <- min(x.index)
    max.x <- max(x.index)
    min.y <- min(y.index)
    max.y <- max(y.index)

    dir.create(dirname(newname))
    system(paste0("ncks -F -O -d lat,", min.y, ",", max.y, " -d lon,", min.x, ",", max.x, " ", path.domain, " ", newname))

    nc <- nc_open(newname, write = T)
    ncvar_put(nc, nc$var$mask, mask[, , i][min.x:max.x, min.y:max.y])
    nc_close(nc)
  }
}
