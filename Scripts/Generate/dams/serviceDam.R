library(fields)
library(ncdf4)
rm(list = ls())

# Input
ldam.file <- "Saves/localDamsMerge.csv"
gdam.file <- "Saves/globalDamsMerge.csv"
discharge.file <- "../../../Data/Transformed/VIC/fluxes_VlietAlt30min_WB.yearmean.nc"
downstream.file <- "../../../Data/Transformed/Routing/downstream_30min_global.RDS"
ldam.service.out <- "Saves/localDamsService.RDS"
gdam.service.out <- "Saves/globalDamsService.RDS"

# Load
nc <- nc_open(discharge.file)
discharge <- apply(X = ncvar_get(nc, "OUT_DISCHARGE", start = c(1, 1, 2), count = c(-1, -1, -1)), MARGIN = c(1, 2), FUN = mean)
nc_close(nc)

downstream <- readRDS(downstream.file)
ldam <- read.csv(ldam.file, stringsAsFactors = F)
gdam <- read.csv(gdam.file, stringsAsFactors = F)

# Setup
lats <- seq(-89.75, 89.75, by = 0.5)
lons <- seq(-179.75, 179.75, by = 0.5)

gdam$X <- NA
gdam$Y <- NA
for (i in 1:nrow(gdam)) {
  gdam$X[i] <- which(gdam$MODEL_LONG_DD[i] == lons)
  gdam$Y[i] <- which(gdam$MODEL_LAT_DD[i] == lats)
}
ldam$X <- NA
ldam$Y <- NA
for (i in 1:nrow(ldam)) {
  ldam$X[i] <- which(ldam$MODEL_LONG_DD[i] == lons)
  ldam$Y[i] <- which(ldam$MODEL_LAT_DD[i] == lats)
}

# Calculate
gdam.services <- list()
for (i in 1:nrow(gdam)) {
  x <- which(gdam$MODEL_LONG_DD[i] == lons)
  y <- which(gdam$MODEL_LAT_DD[i] == lats)

  gdam.service <- data.frame(
    X = numeric(),
    Y = numeric(),
    FRAC = numeric()
  )

  if (is.na(downstream[x, y, 1]) || !(gdam$USE_IRR[i] == 1)) {
    gdam.services[[length(gdam.services) + 1]] <- gdam.service
    next
  }

  cur <- c(x, y)
  nex <- downstream[x, y, ]
  frac <- discharge[x, y] / discharge [cur[1], cur[2]]
  while (TRUE) {
    # exclude fractions < 0.25
    if (frac < 0.25) {
      break
    }

    gdam.service <- rbind(gdam.service, c(cur[1], cur[2], frac))

    if (cur[1] == nex[1] && cur[2] == nex[2]) {
      break
    }

    cur <- nex
    nex <- downstream[cur[1], cur[2], ]
    frac <- discharge[x, y] / discharge [cur[1], cur[2]]
  }

  colnames(gdam.service) <- c("X", "Y", "FRAC")
  gdam.services[[length(gdam.services) + 1]] <- gdam.service
}

ldam.services <- list()
for (i in 1:nrow(ldam)) {
  ldam.service <- data.frame(
    X = numeric(),
    Y = numeric(),
    FRAC = numeric()
  )
  ldam.services[[length(ldam.services) + 1]] <- ldam.service
}

# visualize
gdam.service.map <- array(NA, dim = dim(discharge))
ldam.service.map <- array(NA, dim = dim(discharge))
for (x in 1:dim(discharge)[1]) {
  for (y in 1:dim(discharge)[2]) {
    if (is.na(discharge[x, y])) {
      next
    }
    gdam.service.map[x, y] <- 0
    ldam.service.map[x, y] <- 0
  }
}

for (i in 1:length(gdam.services)) {
  gdam.service <- gdam.services[[i]]

  if (nrow(gdam.service) == 0) {
    next
  }

  for (j in 1:nrow(gdam.service)) {
    x <- gdam.service$X[j]
    y <- gdam.service$Y[j]

    gdam.service.map[x, y] <- gdam.service.map[x, y] + gdam.service$FRAC[j]
  }
}
image.plot(gdam.service.map, zlim = c(0, 4))

for (i in 1:length(ldam.services)) {
  ldam.service <- ldam.services[[i]]

  if (nrow(ldam.service) == 0) {
    next
  }

  for (j in 1:nrow(ldam.service)) {
    x <- ldam.service$X[j]
    y <- ldam.service$Y[j]

    ldam.service.map[x, y] <- ldam.service.map[x, y] + ldam.service$FRAC[j]
  }
}
image.plot(ldam.service.map, zlim = c(0, 1))

# Save
dir.create(dirname(gdam.service.out))
saveRDS(gdam.services, gdam.service.out)
dir.create(dirname(ldam.service.out))
saveRDS(ldam.services, ldam.service.out)
