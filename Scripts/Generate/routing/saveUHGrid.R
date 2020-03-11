library(fields)
library(ncdf4)
rm(list = ls())

# Input
slope.file <- "../../../Data/Transformed/Routing/slope_30min_global.RDS"
uh.file <- "../../../Data/Primary/SCS/dimensionless_unitHydrograph.csv"
mask.file <- "../../../Data/Transformed/Routing/mask_30min_global.RDS"
area.file <- "../../../Data/Transformed/Routing/area_30min_global.RDS"
distance.file <- "../../../Data/Transformed/Routing/distance_30min_global.RDS"
uh.out <- "Saves/UH_grid.RDS"

# Load
area <- readRDS(area.file)
distance <- readRDS(distance.file)
slope <- readRDS(slope.file)
mask <- readRDS(mask.file)
uh <- read.table(uh.file, sep = ";")

# Setup
times <- cumsum(rep(3600, 24 * 16))

interp <- function(x, x1, x2, y1, y2) {
  if (x < x1[1]) {
    return(y1[1])
  }
  if (x > x2[length(x2)]) {
    return(y2[length(y2)])
  }

  idx <- max(which(x1 - x <= 0))
  xdist <- x2[idx] - x1[idx]
  xfrac <- (x - x1[idx]) / xdist
  ydist <- y2[idx] - y1[idx]
  return(y1[idx] + ydist * xfrac)
}

get.grid.tp <- function(distance, slope) {
  if (slope == 0) {
    slope <- 1e-10
  }

  tc <- ((0.01947 * distance)^0.77) / (slope^0.385)
  tp <- 0.6 * tc
  tp <- tp * 60
  return(tp)
}

get.grid.uh <- function(tp) {
  uh.grid.temp <- uh
  uh.grid.temp[, 1] <- uh.grid.temp[, 1] * tp

  uh.grid <- data.frame(Time = times, Fraction = rep(NA, length(times)))
  uh.grid$Fraction <- apply(
    X = uh.grid[, 1, drop = F], MARGIN = 1, FUN = interp,
    x1 = uh.grid.temp[1:(nrow(uh.grid.temp) - 1), 1],
    x2 = uh.grid.temp[2:nrow(uh.grid.temp), 1],
    y1 = uh.grid.temp[1:(nrow(uh.grid.temp) - 1), 2],
    y2 = uh.grid.temp[2:nrow(uh.grid.temp), 2]
  )

  uh.grid$Fraction <- uh.grid$Fraction / sum(uh.grid$Fraction)
  return(uh.grid)
}

# Calculate
uh.grid.map <- array(NA, dim = c(dim(distance)[1], dim(distance)[2], length(times)))
tp.grid.map <- array(NA, dim = c(dim(distance)[1], dim(distance)[2]))
for (x in 1:dim(uh.grid.map)[1]) {
  print(x)
  for (y in 1:dim(uh.grid.map)[2]) {
    if (is.na(slope[x, y])) {
      next
    }

    tp.grid <- get.grid.tp(distance[x, y], slope[x, y])
    uh.grid <- get.grid.uh(tp.grid)
    # plot(uh.grid, type = "l")

    tp.grid.map[x, y] <- tp.grid
    uh.grid.map[x, y, ] <- uh.grid$Fraction
  }
}

calc.tp <- function(uh.fractions) {
  if (length(na.omit(uh.fractions)) == 0) {
    return(NA)
  } else {
    return(which(uh.fractions == max(uh.fractions)) / 24)
  }
}
test <- apply(uh.grid.map, MARGIN = c(1, 2), FUN = calc.tp)

image.plot(tp.grid.map / 60 / 60 / 24, zlim = c(0, 2))
image.plot(test, zlim = c(0, 2))

# Save
dir.create(dirname(uh.out))
saveRDS(uh.grid.map, uh.out)
