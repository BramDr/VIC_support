library(fields)
library(ncdf4)
rm(list = ls())

# Input
dam.file <- "../../../Data/Primary/GRanD/GRanDv1.1.csv"
area.file <- "../../../Data/Transformed/Routing/area_30min_global.RDS"
accumulation.file <- "../../../Data/Transformed/Routing/accumulation_30min_global.RDS"
dam.out <- "../../../Data/Transformed/Dams/GRanDcorrected.csv"

# Load
area <- readRDS(area.file)
accumulation <- readRDS(accumulation.file)

dams <- read.csv(dam.file, stringsAsFactors = F)
dams$LONG_DD <- as.numeric(dams$LONG_DD)
dams$LAT_DD <- as.numeric(dams$LAT_DD)
dams[dams == -99] <- NA

# Setup
lats <- seq(-89.75, 89.75, by = 0.5)
lons <- seq(-179.75, 179.75, by = 0.5)

# Calculate
n.index <- c(-1, 0, 1)

dams$MODEL_LONG_DD <- NA
dams$MODEL_LAT_DD <- NA
dams$MODEL_AREA_FRAC <- NA
dams$MODEL_CATCH_FRAC <- NA
for (i in 1:nrow(dams)) {
  x <- which.min(abs(lons - dams$LONG_DD[i]))
  y <- which.min(abs(lats - dams$LAT_DD[i]))

  frac.a <- dams$CATCH_SKM[i] * 1e6 / area[x, y]
  frac.a <- min(frac.a, 1)

  dams$MODEL_AREA_FRAC[i] <- frac.a

  if (frac.a < 1) {
    dams$MODEL_LONG_DD[i] <- lons[x]
    dams$MODEL_LAT_DD[i] <- lats[y]
    dams$MODEL_CATCH_FRAC[i] <- frac.a
    next
  }

  bx <- x
  by <- y
  ba <- Inf
  for (xni in n.index) {
    for (yni in n.index) {
      xn <- x + xni
      yn <- y + yni

      if (xn <= 0 || xn > dim(accumulation)[1] ||
        yn <= 0 || yn > dim(accumulation)[2]) {
        next
      }

      if (is.na(accumulation[xn, yn])) {
        next
      }

      # if neighbouring accumulation is better than the current accumulation,
      # save neighbouring location
      if (abs(dams$CATCH_SKM[i] * 1e6 - accumulation[xn, yn]) < ba) {
        bx <- xn
        by <- yn
        ba <- abs(dams$CATCH_SKM[i] * 1e6 - accumulation[xn, yn])
      }
    }
  }

  frac.c <- dams$CATCH_SKM[i] * 1e6 / accumulation[bx, by]
  frac.c <- max(frac.c, 1 / frac.c)

  # Only correct if the correction is a good approximation
  if (frac.c > 1.5) {
    bx <- x
    by <- y
  }

  dams$MODEL_CATCH_FRAC[i] <- frac.c
  dams$MODEL_LONG_DD[i] <- lons[bx]
  dams$MODEL_LAT_DD[i] <- lats[by]
}

# Save
dir.create(dirname(dam.out))
write.csv(x = dams, file = dam.out, row.names = F)
