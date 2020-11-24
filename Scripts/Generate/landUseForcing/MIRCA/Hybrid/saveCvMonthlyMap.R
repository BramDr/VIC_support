library(fields)
library(ncdf4)

rm(list = ls())

# Input
map.script <- "../../../../Support/mapFunctions.R"
param.file <- "Saves/parameters_30min_global.RDS"
mask.file <- "../../../../../Data/Primary/VIC/domain_global.nc"
vegetation.file <- "../../../../../Data/VIC/Parameters/global/vegetation_params_Vliet_global.nc"
cv.monthly.out <- "Saves/Cv_monthly_30min_global.RDS"
split <- data.frame(
  name = c(
    "wheatRainfed", "wheatIrrigated",
    "maizeRainfed", "maizeIrrigated",
    "riceRainfed", "riceIrrigated",
    "soybeanRainfed", "soybeanIrrigated"
  ),
  id = c(
    27, 1,
    28, 2,
    29, 3,
    34, 8
  ),
  stringsAsFactors = F
)

# Load
source(map.script)

param.file.tmp <- gsub(x = param.file, pattern = "parameters_", replacement = paste0("parameters", "Irrigated", "_"))
irr <- readRDS(param.file.tmp)
param.file.tmp <- gsub(x = param.file, pattern = "parameters_", replacement = paste0("parameters", "Rainfed", "_"))
rain <- readRDS(param.file.tmp)

for (i in 1:nrow(split)) {
  param.file.tmp <- gsub(x = param.file, pattern = "parameters_", replacement = paste0("parameters", split$name[i], "_"))
  param.split <- readRDS(param.file.tmp)
  assign(x = split$name[i], value = param.split)
}

nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

nc <- nc_open(vegetation.file)
Cv.veg <- ncvar_get(nc, "Cv")
nc_close(nc)

# Setup
na.map <- is.na(mask) | mask == 0
nveg <- dim(Cv.veg)[3] + 1 + nrow(split)
nmonths <- 12

# Caclulate
## Fill variables
irr.filled <- list()
for (i in 1:length(irr)) {
  irr.filled[[i]] <- fillMap(irr[[i]], na.map, getNearestZero)
}
names(irr.filled) <- names(irr)
rain.filled <- list()
for (i in 1:length(rain)) {
  rain.filled[[i]] <- fillMap(rain[[i]], na.map, getNearestZero)
}
names(rain.filled) <- names(rain)

for (i in 1:nrow(split)) {
  data <- get(x = paste0(split$name[i]))
  data.filled <- list()
  for (j in 1:length(data)) {
    data.filled[[j]] <- fillMap(data[[j]], na.map, getNearestZero)
  }
  names(data.filled) <- names(data)
  assign(x = paste0(split$name[i], ".filled"), value = data.filled)
}

image.plot(wheatRainfed.filled[["Cv"]])
image.plot(wheatRainfed.filled[["fcanopy"]][, , 3] == 0)

## Calculate adjusted Cv fractions
Cv.monthly <- array(0, dim = c(dim(mask)[1], dim(mask)[2], nveg, nmonths))
for (z in 1:nmonths) {
  Cv.monthly[, , 11, z] <- rain.filled[["Cv"]]
  Cv.monthly[, , 12, z] <- irr.filled[["Cv"]]
  for (i in 1:nrow(split)) {
    split.filled <- get(x = paste0(split$name[i], ".filled"))
    Cv.monthly[, , 12 + i, z] <- split.filled[["Cv"]]
  }
}
for (x in 1:dim(mask)[1]) {
  for (y in 1:dim(mask)[2]) {
    if (is.na(mask[x, y])) {
      Cv.monthly[x, y, , ] <- NA
      next
    }

    crop.f <- irr.filled[["Cv"]][x, y] + rain.filled[["Cv"]][x, y]
    for (i in 1:nrow(split)) {
      data.filled <- get(x = paste0(split$name[i], ".filled"))
      crop.f <- crop.f + data.filled[["Cv"]][x, y]
    }

    veg.f <- sum(Cv.veg[x, y, 1:10])
    nat.f <- sum(Cv.veg[x, y, c(1:10, 12)])

    for (z in 1:nmonths) {
      if (crop.f > 1) {
        Cv.monthly[x, y, , z] <- Cv.monthly[x, y, , z] / crop.f
        crop.f <- 1
      }

      if (veg.f <= 0) {
        if (crop.f == 1) {
          next
        }
        Cv.monthly[x, y, dim(Cv.monthly)[3], z] <- (1 - crop.f)
      } else {
        if (crop.f + veg.f > 1) {
          rescale.f <- (1 - crop.f) / veg.f
          Cv.monthly[x, y, c(1:10), z] <- Cv.veg[x, y, c(1:10)] * rescale.f
        } else if (crop.f + nat.f > 1) {
          Cv.monthly[x, y, c(1:10), z] <- Cv.veg[x, y, c(1:10)]
          Cv.monthly[x, y, dim(Cv.monthly)[3], z] <- 1.0 - crop.f - veg.f
        } else {
          rescale.f <- (1 - crop.f) / nat.f
          Cv.monthly[x, y, c(1:10, dim(Cv.monthly)[3]), z] <- Cv.veg[x, y, c(1:10, 12)] * rescale.f
        }
      }

      Cv.monthly[x, y, , z] <- Cv.monthly[x, y, , z] / sum(Cv.monthly[x, y, , z])
    }
  }
}
Cv.sum <- apply(X = Cv.monthly, MARGIN = c(1, 2, 4), FUN = sum)
for (z in 1:nmonths) {
  image.plot(Cv.sum[, , z], main = z)
}

## Calculate bare soil fraction
Cv.monthly.adj <- Cv.monthly
for (z in 1:nmonths) {
  diff <- Cv.monthly[, , 11, z] * (1 - rain.filled[["fcanopy"]][, , z])
  Cv.monthly.adj[, , 11, z] <- Cv.monthly.adj[, , 11, z] - diff
  Cv.monthly.adj[, , dim(Cv.monthly.adj)[3], z] <- Cv.monthly.adj[, , dim(Cv.monthly.adj)[3], z] + diff

  diff <- Cv.monthly[, , 12, z] * (1 - irr.filled[["fcanopy"]][, , z])
  Cv.monthly.adj[, , 12, z] <- Cv.monthly.adj[, , 12, z] - diff
  Cv.monthly.adj[, , dim(Cv.monthly.adj)[3], z] <- Cv.monthly.adj[, , dim(Cv.monthly.adj)[3], z] + diff

  for (i in 1:nrow(split)) {
    split.filled <- get(x = paste0(split$name[i], ".filled"))
    diff <- Cv.monthly[, , 12 + i, z] * (1 - split.filled[["fcanopy"]][, , z])
    Cv.monthly.adj[, , 12 + i, z] <- Cv.monthly.adj[, , 12 + i, z] - diff
    Cv.monthly.adj[, , dim(Cv.monthly.adj)[3], z] <- Cv.monthly.adj[, , dim(Cv.monthly.adj)[3], z] + diff
  }
}
Cv.sum <- apply(X = Cv.monthly, MARGIN = c(1, 2, 4), FUN = sum)
for (z in 1:nmonths) {
  image.plot(Cv.sum[, , z], main = z)
}

Cv.monthly.adj[225, 72, 21, ]

# Save
dir.create(dirname(cv.monthly.out))
saveRDS(Cv.monthly.adj, cv.monthly.out)
