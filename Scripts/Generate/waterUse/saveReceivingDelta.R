library(ncdf4)
library(fields)
rm(list = ls())

# Input
count.file <- "../../../Data/Transformed/Routing/count_30min_global.RDS"
basin.file <- "../../../Data/Transformed/Delta/deltaBasins_30min_global.RDS"
delta.file <- "../../../Data/Transformed/Delta/deltaAdj_30min_global.RDS"
downstream.file <- "../../../Data/Transformed/Routing/downstream_30min_global.RDS"
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
receiving.out <- "Saves/receivingDelta.RDS"

# Load
nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

count <- readRDS(count.file)
basin <- readRDS(basin.file)
delta <- readRDS(delta.file)
downstream <- readRDS(downstream.file)

# Setup
check.in.stream <- function(xfrom, yfrom, xto, yto) {
  cur <- c(xfrom, yfrom)
  nex <- downstream[cur[1], cur[2], ]

  goes <- F
  while (TRUE) {
    if (cur[1] == xto && cur[2] == yto) {
      return(T)
    }
    if (cur[1] == nex[1] && cur[2] == nex[2]) {
      return(F)
    }

    cur <- nex
    nex <- downstream[cur[1], cur[2], ]
  }
}

# Calculate

## Get the basin associated with the delta
delta.basin <- data.frame(delta = numeric(), basin = numeric())
for (x in 1:dim(delta)[1]) {
  for (y in 1:dim(delta)[2]) {
    if (is.na(delta[x, y])) {
      next
    }

    delta.basin[nrow(delta.basin) + 1, ] <- c(delta[x, y], basin[x, y])
  }
}
delta.basin <- unique(delta.basin)

## Get the basin outflow points associated with the deltas
delta.basin$count <- 0
delta.basin$x <- NA
delta.basin$y <- NA
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

## Get delta cells
delta.receiving <- vector(mode = "list", length = 720)
for (x in 1:dim(downstream)[1]) {
  delta.receiving[[x]] <- vector(mode = "list", length = 360)
}
for (i in 1:nrow(delta.basin)) {
  deltas <- data.frame(x = numeric(), y = numeric())

  for (x in 1:dim(delta)[1]) {
    for (y in 1:dim(delta)[2]) {
      if (is.na(delta[x, y])) {
        next
      }
      if (is.na(basin[x, y])) {
        next
      }

      ## Check delta
      if (delta.basin$delta[i] != delta[x, y]) {
        next
      }

      ## Check basin
      if (delta.basin$basin[i] != basin[x, y]) {
        next
      }

      ## Check if delta goes to outflow
      if (check.in.stream(x, y, delta.basin$x[i], delta.basin$y[i])) {
        next
      }

      ## Check if outflow goes to delta
      if (check.in.stream(delta.basin$x[i], delta.basin$y[i], x, y)) {
        next
      }

      deltas[nrow(deltas) + 1, ] <- c(x, y)
    }
  }

  deltas <- unique(deltas)

  if (nrow(deltas) == 0) {
    next
  }

  delta.receiving[[delta.basin$x[i]]][[delta.basin$y[i]]]$df <- deltas
}

## Nreceiving
Nreceiving <- mask * 0
for (x in 1:dim(downstream)[1]) {
  for (y in 1:dim(downstream)[2]) {
    if (length(names(delta.receiving[[x]][[y]])) == 0) {
      next
    }

    df <- delta.receiving[[x]][[y]]$df

    Nreceiving[x, y] <- Nreceiving[x, y] + nrow(df)
  }
}
image.plot(Nreceiving)

# Save
dir.create(dirname(receiving.out))
saveRDS(delta.receiving, receiving.out)
