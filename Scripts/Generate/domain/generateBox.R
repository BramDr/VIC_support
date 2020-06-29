library(ncdf4)
library(fields)
rm(list = ls())

# Input
path.domain <- "../../../Data/Primary/VIC/domain_global.nc"
dir.out <- "../../../Data/VIC/Parameters/"

# Setup
lon <- seq(from = -179.75, to = 179.75, by = 0.5)
lat <- seq(from = -89.75, to = 89.75, by = 0.5)
combine <- FALSE

boxes <- data.frame(lat.min = numeric(), lon.min = numeric(), lat.max = numeric(), lon.max = numeric(), name = character(), stringsAsFactors = F)
boxes[nrow(boxes) + 1, ] <- c(1.25, 1.25, 9.25, 9.25, "Error")
boxes[nrow(boxes) + 1, ] <- c(42.75, -4.75, 54.75, 8.25, "NWEurope")
boxes[nrow(boxes) + 1, ] <- c(20.75, 103.75, 44.75, 122.75, "EAsia")
boxes[nrow(boxes) + 1, ] <- c(30.25, -94.25, 37.75, -87.75, "LMississippi")

boxes$lat.min <- as.numeric(boxes$lat.min)
boxes$lon.min <- as.numeric(boxes$lon.min)
boxes$lat.max <- as.numeric(boxes$lat.max)
boxes$lon.max <- as.numeric(boxes$lon.max)

# Load
nc <- nc_open(path.domain)
mask.orig <- ncvar_get(nc, "mask")
nc_close(nc)

# Calculate
mask <- array(NA, dim = c(length(lon), length(lat), nrow(boxes)))
for (i in 1:nrow(boxes)) {
  x.min <- which.min(abs(lon - boxes$lon.min[i]))
  y.min <- which.min(abs(lat - boxes$lat.min[i]))
  x.max <- which.min(abs(lon - boxes$lon.max[i]))
  y.max <- which.min(abs(lat - boxes$lat.max[i]))

  mask.tmp <- array(NA, dim = dim(mask)[1:2])
  mask.tmp[x.min:x.max, y.min:y.max] <- 1
  mask.tmp[is.na(mask.orig)] <- NA

  mask[, , i] <- mask.tmp
}

mask.combine <- array(NA, dim = c(length(lon), length(lat)))
for (x in 1:dim(mask)[1]) {
  for (y in 1:dim(mask)[2]) {
    for (i in 1:nrow(boxes)) {
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
  newname <- paste0(dir.out, "/", paste0(boxes$name, collapse = ""), "/", "domain_", paste0(boxes$name, collapse = ""), ".nc")

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
  for (i in 1:nrow(boxes)) {
    newname <- paste0(dir.out, "/", boxes$name[i], "/", "domain_", boxes$name[i], ".nc")

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
