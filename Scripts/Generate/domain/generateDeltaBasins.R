library(ncdf4)
library(fields)
rm(list = ls())

# Input
path.basin <- "../../../Data/Transformed/Delta/deltaBasins_30min_global.RDS"
path.domain <- "../../../Data/Primary/VIC/domain_global.nc"
dir.out <- "../../../Data/VIC/Parameters/"

# Load
basin <- readRDS(path.basin)

# Setup
lon <- seq(from = -179.75, to = 179.75, by = 0.5)
lat <- seq(from = -89.75, to = 89.75, by = 0.5)
combine <- TRUE

points <- data.frame(lat = numeric(), lon = numeric(), name = character(), stringsAsFactors = F)
points[nrow(points) + 1, ] <- c(15.25, 105.75, "MekongDelta")
# points[nrow(points) + 1, ] <- c(18.75, 95.25, "Irrawaddy")
# points[nrow(points) + 1, ] <- c(1.75, 17.25, "Congo")
# points[nrow(points) + 1, ] <- c(68.75, 15.75, "Error")

points$lat <- as.numeric(points$lat)
points$lon <- as.numeric(points$lon)

# Calculate
for (i in 1:nrow(points)) {
  x <- which.min(abs(lon - points$lon[i]))
  y <- which.min(abs(lat - points$lat[i]))

  id <- basin[x, y]

  if (!is.na(id)) {
    points$basin[i] <- id
    points$size[i] <- sum(na.omit(c(basin)) == id)
  } else {
    print(paste0("Point ", points[i, ], " falls outside of basin mask"))
  }
}

mask <- array(NA, dim = c(dim(basin), nrow(points)))
for (i in 1:nrow(points)) {
  for (x in 1:dim(basin)[1]) {
    for (y in 1:dim(basin)[2]) {
      if (!is.na(basin[x, y]) && basin[x, y] == points$basin[i]) {
        mask[x, y, i] <- 1
      }
    }
  }
}

mask.combine <- array(NA, dim = c(dim(basin)))
for (x in 1:dim(basin)[1]) {
  for (y in 1:dim(basin)[2]) {
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
