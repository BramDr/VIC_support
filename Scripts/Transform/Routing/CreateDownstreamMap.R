library(ncdf4)
library(fields)
rm(list = ls())

# Input
direction.file <- "../../../Data/Primary/VIC/RVIC_input_global.nc"
mask.file <- "../../../Data/Transformed/Routing/mask_30min_global.RDS"
downstream.out <- "../../../Data/Transformed/Routing/downstream_30min_global.RDS"

# Load
mask <- readRDS(mask.file)

nc <- nc_open(filename = direction.file)
direction <- ncvar_get(nc = nc, "flow_direction")
nc_close(nc = nc)
image.plot(direction)

# Setup
direction.to.index <- function(direction) {
  if (direction < 1 || direction > 9) {
    warning("Direction (" + direction + ") is outside of range [1-9], defaulting to 9 [outlet]")
    direction <- 9
  }

  index <- switch(direction,
    c(0, 1), # north
    c(1, 1), # north east
    c(1, 0), # east
    c(1, -1), # south east
    c(0, -1), # south
    c(-1, -1), # south west
    c(-1, 0), # west
    c(-1, 1), # north west
    c(0, 0)
  ) # outlet

  return(index)
}

# Calculate
downstream <- array(NA, dim = c(dim(direction), 2))
for (x in 1:dim(direction)[1]) {
  for (y in 1:dim(direction)[2]) {
    if (is.na(mask[x, y])) {
      next
    }

    if (is.na(direction[x, y])) {
      direction[x, y] <- 9
    }

    downstream[x, y, ] <- c(x, y) + direction.to.index(direction[x, y])

    if (is.na(mask[downstream[x, y, 1], downstream[x, y, 2]])) {
      direction[x, y] <- 9
      downstream[x, y, ] <- c(x, y) + direction.to.index(direction[x, y])
    }
  }
}
image.plot(downstream[, , 1])
image.plot(downstream[, , 2])

# Save
dir.create(dirname(downstream.out))
saveRDS(downstream, downstream.out)
