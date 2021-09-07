library(ncdf4)
library(fields)
rm(list = ls())

# Input
path.basin <- "../../../Data/Transformed/Routing/basins_Dahri_5min_Indus.RDS"
path.downstream <- "../../../Data/Transformed/Routing/downstream_Dahri_5min_Indus.RDS"
path.domain <- "../../../Data/VIC/Parameters/Indus_5min/domain_Dahri_Indus.nc"
dir.out <- "../../../Data/VIC/Parameters/"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
basin <- readRDS(path.basin)
downstream <- readRDS(path.downstream)

# Setup
combine <- FALSE

points <- data.frame(lon = numeric(), lat = numeric(), name = character(), stringsAsFactors = F)
points[nrow(points) + 1, ] <- c(73.70870, 33.20867, "Jhelm")
points[nrow(points) + 1, ] <- c(74.37537, 35.95867, "Hunza")
points[nrow(points) + 1, ] <- c(74.29203, 35.87533, "Gilgt")
points[nrow(points) + 1, ] <- c(71.79203, 35.87533, "Chitr")
points[nrow(points) + 1, ] <- c(75.62537, 35.45867, "Shigr")
points[nrow(points) + 1, ] <- c(72.04203, 34.62533, "Swatc")
points[nrow(points) + 1, ] <- c(72.20870, 33.95867, "Kabn")
points[nrow(points) + 1, ] <- c(74.70870, 35.54200, "Astor")

points$lat <- as.numeric(points$lat)
points$lon <- as.numeric(points$lon)

# Calculate
for (i in 1:nrow(points)) {
  x <- which.min(abs(out.lons - points$lon[i]))
  y <- which.min(abs(out.lats - points$lat[i]))

  id <- basin[x, y]

  if (!is.na(id)) {
    points$basin[i] <- id
    points$size[i] <- sum(na.omit(c(basin)) == id)
    points$x[i] <- x
    points$y[i] <- y
  } else {
    print(paste0("Point ", points[i, ], " falls outside of basin mask"))
  }
}

mask <- array(NA, dim = c(dim(basin), nrow(points)))
for (i in 1:nrow(points)) {
  for (x in 1:dim(basin)[1]) {
    for (y in 1:dim(basin)[2]) {
      if (!is.na(basin[x, y]) && basin[x, y] == points$basin[i]) {
        cur <- c(x, y)
        nex <- downstream[x, y, ]

        while (TRUE) {
          if (cur[1] == points$x[i] && cur[2] == points$y[i]) {
            mask[x, y, i] <- 1
          }

          if (cur[1] == nex[1] && cur[2] == nex[2]) {
            break
          }

          cur <- nex
          nex <- downstream[cur[1], cur[2], ]
        }
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
  newname <- paste0(dir.out, "/", paste0(points$name, collapse = ""), "_5min/", "domain_Dahri_", paste0(points$name, collapse = ""), ".nc")

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
    newname <- paste0(dir.out, "/", points$name[i], "_5min/", "domain_Dahri_", points$name[i], ".nc")

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
