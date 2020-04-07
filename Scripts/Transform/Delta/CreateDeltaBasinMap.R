library(fields)
library(raster)
rm(list = ls())

# Input
count.file <- "../../../Data/Transformed/Routing/count_30min_global.RDS"
basin.file <- "../../../Data/Transformed/Routing/basins_30min_global.RDS"
delta.file <- "../../../Data/Transformed/Delta/delta_30min_global.RDS"
delta.out <- "../../../Data/Transformed/Delta/deltaBasins_30min_global.RDS"

# Load
count <- readRDS(count.file)
basin <- readRDS(basin.file)
delta <- readRDS(delta.file)

# Calculate
## Get the basin associated with the delta
delta.basin <- data.frame(delta = unique(na.omit(c(delta))), basin = NA, count = 0, x = NA, y = NA)
for (x in 1:dim(delta)[1]) {
  for (y in 1:dim(delta)[2]) {
    if (is.na(delta[x, y])) {
      next
    }

    row <- which(delta.basin$delta == delta[x, y])
    if (count[x, y] > delta.basin$count[row]) {
      delta.basin$basin[row] <- basin[x, y]
      delta.basin$count[row] <- count[x, y]
      delta.basin$x[row] <- x
      delta.basin$y[row] <- y
    }
  }
}

## Get the basin outflow point associated with the delta
for (x in 1:dim(basin)[1]) {
  for (y in 1:dim(basin)[2]) {
    if (is.na(basin[x, y])) {
      next
    }
    if (!basin[x, y] %in% delta.basin$basin) {
      next
    }

    row <- which(delta.basin$basin == basin[x, y])
    if (count[x, y] > delta.basin$count[row]) {
      delta.basin$count[row] <- count[x, y]
      delta.basin$x[row] <- x
      delta.basin$y[row] <- y
    }
  }
}

## Combine delta upstream area with basin upstream area
delta.basin.map <- basin
for (x in 1:dim(basin)[1]) {
  for (y in 1:dim(basin)[2]) {
    if (is.na(delta[x, y])) {
      next
    }

    row <- which(delta.basin$delta == delta[x, y])
    delta.basin.map[basin == basin[x, y]] <- delta.basin$basin[row]
  }
}

dir.create(dirname(delta.out))
saveRDS(object = delta.basin.map, file = delta.out)
