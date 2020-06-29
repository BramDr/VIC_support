library(fields)
library(ncdf4)
rm(list = ls())

# Input
function.script <- "../../../Support/generateFunctions.R"
map.script <- "../../../Support/mapFunctions.R"
mask.file <- "../../../../Data/Primary/VIC/domain_global.nc"
paddy.file <- "Aggregated/Saves/parametersUniformAltPaddy_30min_global.RDS"
irr.file <- "Aggregated/Saves/parametersUniformAltIrrigated_30min_global.RDS"
rain.file <- "Aggregated/Saves/parametersUniformAltRainfed_30min_global.RDS"
vegetation.file <- "../../../../Data/VIC/Parameters/global/vegetation_params_VlietAlt_global.nc"
vegetation.out <- "../../../../Data/VIC/Parameters/global/vegetation_params_MIRCAaggregatedUniformAlt_global.nc"

# Load
source(function.script)
source(map.script)

irr <- readRDS(irr.file)
paddy <- readRDS(paddy.file)
rain <- readRDS(rain.file)

nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

nc <- nc_open(vegetation.file)
Cv.veg <- ncvar_get(nc, "Cv")
nc_close(nc)

# Setup
na.map <- is.na(mask) | mask == 0

put.maps <- function(nc.file, maps, veg.idx) {
  nc <- nc_open(nc.file, write = T)
  ncvar_put(nc, "wind_atten", maps[["wind_atten"]], start = c(1, 1, veg.idx), count = c(-1, -1, 1))
  ncvar_put(nc, "wind_h", maps[["wind_h"]], start = c(1, 1, veg.idx), count = c(-1, -1, 1))
  ncvar_put(nc, "rmin", maps[["rmin"]], start = c(1, 1, veg.idx), count = c(-1, -1, 1))
  ncvar_put(nc, "rarc", maps[["rarc"]], start = c(1, 1, veg.idx), count = c(-1, -1, 1))
  ncvar_put(nc, "rad_atten", maps[["rad_atten"]], start = c(1, 1, veg.idx), count = c(-1, -1, 1))
  ncvar_put(nc, "RGL", maps[["RGL"]], start = c(1, 1, veg.idx), count = c(-1, -1, 1))
  ncvar_put(nc, "trunk_ratio", maps[["trunk_ratio"]], start = c(1, 1, veg.idx), count = c(-1, -1, 1))
  ncvar_put(nc, "overstory", maps[["overstory"]], start = c(1, 1, veg.idx), count = c(-1, -1, 1))

  ncvar_put(nc, "root_depth", maps[["root_depth.1"]], start = c(1, 1, 1, veg.idx), count = c(-1, -1, 1, 1))
  ncvar_put(nc, "root_depth", maps[["root_depth.2"]], start = c(1, 1, 2, veg.idx), count = c(-1, -1, 1, 1))
  ncvar_put(nc, "root_depth", maps[["root_depth.3"]], start = c(1, 1, 3, veg.idx), count = c(-1, -1, 1, 1))
  ncvar_put(nc, "root_fract", maps[["root_frac.1"]], start = c(1, 1, 1, veg.idx), count = c(-1, -1, 1, 1))
  ncvar_put(nc, "root_fract", maps[["root_frac.2"]], start = c(1, 1, 2, veg.idx), count = c(-1, -1, 1, 1))
  ncvar_put(nc, "root_fract", maps[["root_frac.3"]], start = c(1, 1, 3, veg.idx), count = c(-1, -1, 1, 1))

  ncvar_put(nc, "albedo", maps[["albedo"]], start = c(1, 1, 1, veg.idx), count = c(-1, -1, -1, 1))
  ncvar_put(nc, "LAI", maps[["LAI"]], start = c(1, 1, 1, veg.idx), count = c(-1, -1, -1, 1))
  ncvar_put(nc, "displacement", maps[["displacement"]], start = c(1, 1, 1, veg.idx), count = c(-1, -1, -1, 1))
  ncvar_put(nc, "veg_rough", maps[["veg_rough"]], start = c(1, 1, 1, veg.idx), count = c(-1, -1, -1, 1))
  ncvar_put(nc, "fcanopy", maps[["fcanopy"]], start = c(1, 1, 1, veg.idx), count = c(-1, -1, -1, 1))
  nc_close(nc)
}
put.old <- function(nc.file, old.file) {
  nc <- nc_open(old.file)
  wind_atten <- ncvar_get(nc, "wind_atten")
  wind_h <- ncvar_get(nc, "wind_h")
  rmin <- ncvar_get(nc, "rmin")
  rarc <- ncvar_get(nc, "rarc")
  rad_atten <- ncvar_get(nc, "rad_atten")
  RGL <- ncvar_get(nc, "RGL")
  trunk_ratio <- ncvar_get(nc, "trunk_ratio")
  overstory <- ncvar_get(nc, "overstory")

  root_fract <- ncvar_get(nc, "root_fract")
  root_depth <- ncvar_get(nc, "root_depth")

  albedo <- ncvar_get(nc, "albedo")
  LAI <- ncvar_get(nc, "LAI")
  displacement <- ncvar_get(nc, "displacement")
  veg_rough <- ncvar_get(nc, "veg_rough")
  nc_close(nc)

  nc <- nc_open(nc.file, write = T)
  ncvar_put(nc, "wind_atten", wind_atten[, , 1:10], start = c(1, 1, 1), count = c(-1, -1, 10))
  ncvar_put(nc, "wind_h", wind_h[, , 1:10], start = c(1, 1, 1), count = c(-1, -1, 10))
  ncvar_put(nc, "rmin", rmin[, , 1:10], start = c(1, 1, 1), count = c(-1, -1, 10))
  ncvar_put(nc, "rarc", rarc[, , 1:10], start = c(1, 1, 1), count = c(-1, -1, 10))
  ncvar_put(nc, "rad_atten", rad_atten[, , 1:10], start = c(1, 1, 1), count = c(-1, -1, 10))
  ncvar_put(nc, "RGL", RGL[, , 1:10], start = c(1, 1, 1), count = c(-1, -1, 10))
  ncvar_put(nc, "trunk_ratio", trunk_ratio[, , 1:10], start = c(1, 1, 1), count = c(-1, -1, 10))
  ncvar_put(nc, "overstory", overstory[, , 1:10], start = c(1, 1, 1), count = c(-1, -1, 10))

  ncvar_put(nc, "wind_atten", wind_atten[, , 12], start = c(1, 1, 14), count = c(-1, -1, 1))
  ncvar_put(nc, "wind_h", wind_h[, , 12], start = c(1, 1, 14), count = c(-1, -1, 1))
  ncvar_put(nc, "rmin", rmin[, , 12], start = c(1, 1, 14), count = c(-1, -1, 1))
  ncvar_put(nc, "rarc", rarc[, , 12], start = c(1, 1, 14), count = c(-1, -1, 1))
  ncvar_put(nc, "rad_atten", rad_atten[, , 12], start = c(1, 1, 14), count = c(-1, -1, 1))
  ncvar_put(nc, "RGL", RGL[, , 12], start = c(1, 1, 14), count = c(-1, -1, 1))
  ncvar_put(nc, "trunk_ratio", trunk_ratio[, , 12], start = c(1, 1, 14), count = c(-1, -1, 1))
  ncvar_put(nc, "overstory", overstory[, , 12], start = c(1, 1, 14), count = c(-1, -1, 1))

  ncvar_put(nc, "root_fract", root_fract[, , , 1:10], start = c(1, 1, 1, 1), count = c(-1, -1, -1, 10))
  ncvar_put(nc, "root_depth", root_depth[, , , 1:10], start = c(1, 1, 1, 1), count = c(-1, -1, -1, 10))

  ncvar_put(nc, "root_fract", root_fract[, , , 12], start = c(1, 1, 1, 14), count = c(-1, -1, -1, 1))
  ncvar_put(nc, "root_depth", root_depth[, , , 12], start = c(1, 1, 1, 14), count = c(-1, -1, -1, 1))

  ncvar_put(nc, "albedo", albedo[, , , 1:10], start = c(1, 1, 1, 1), count = c(-1, -1, -1, 10))
  ncvar_put(nc, "LAI", LAI[, , , 1:10], start = c(1, 1, 1, 1), count = c(-1, -1, -1, 10))
  ncvar_put(nc, "displacement", displacement[, , , 1:10], start = c(1, 1, 1, 1), count = c(-1, -1, -1, 10))
  ncvar_put(nc, "veg_rough", veg_rough[, , , 1:10], start = c(1, 1, 1, 1), count = c(-1, -1, -1, 10))
  ncvar_put(nc, "fcanopy", albedo[, , , 1:10] * 0 + 1, start = c(1, 1, 1, 1), count = c(-1, -1, -1, 10))

  ncvar_put(nc, "albedo", albedo[, , , 12], start = c(1, 1, 1, 14), count = c(-1, -1, -1, 1))
  ncvar_put(nc, "LAI", LAI[, , , 12], start = c(1, 1, 1, 14), count = c(-1, -1, -1, 1))
  ncvar_put(nc, "displacement", displacement[, , , 12], start = c(1, 1, 1, 14), count = c(-1, -1, -1, 1))
  ncvar_put(nc, "veg_rough", veg_rough[, , , 12], start = c(1, 1, 1, 14), count = c(-1, -1, -1, 1))
  ncvar_put(nc, "fcanopy", albedo[, , , 12] * 0 + 1, start = c(1, 1, 1, 14), count = c(-1, -1, -1, 1))
  nc_close(nc)
}

# Calculate
## Fill variables
irr.filled <- list()
for (i in 1:length(irr)) {
  irr.filled[[i]] <- fillMap(irr[[i]], na.map, getNearestZero)
}
names(irr.filled) <- names(irr)
paddy.filled <- list()
for (i in 1:length(paddy)) {
  paddy.filled[[i]] <- fillMap(paddy[[i]], na.map, getNearestZero)
}
names(paddy.filled) <- names(paddy)
rain.filled <- list()
for (i in 1:length(rain)) {
  rain.filled[[i]] <- fillMap(rain[[i]], na.map, getNearestZero)
}
names(rain.filled) <- names(rain)

## Calculate adjusted Cv fractions
Cv.new <- array(0, dim = c(dim(mask)[1], dim(mask)[2], dim(Cv.veg)[3] + 2))
Cv.new[, , 11] <- rain.filled[["Cv"]]
Cv.new[, , 12] <- irr.filled[["Cv"]]
Cv.new[, , 13] <- paddy.filled[["Cv"]]
for (x in 1:dim(mask)[1]) {
  for (y in 1:dim(mask)[2]) {
    if (is.na(mask[x, y])) {
      Cv.new[x, y, ] <- NA
      next
    }

    crop.f <- irr.filled[["Cv"]][x, y] + paddy.filled[["Cv"]][x, y] + rain.filled[["Cv"]][x, y]
    veg.f <- sum(Cv.veg[x, y, c(1:10)])
    nat.f <- sum(Cv.veg[x, y, c(1:10, 12)])

    if (crop.f > 1) {
      Cv.new[x, y, ] <- Cv.new[x, y, ] / crop.f
      crop.f <- 1
    }

    if (veg.f <= 0) {
      if (crop.f == 1) {
        next
      }
      Cv.new[x, y, dim(Cv.new)[3]] <- (1 - crop.f)
    } else {
      if (crop.f + veg.f > 1) {
        rescale.f <- (1 - crop.f) / veg.f
        Cv.new[x, y, c(1:10)] <- Cv.veg[x, y, c(1:10)] * rescale.f
      } else if (crop.f + nat.f > 1) {
        Cv.new[x, y, c(1:10)] <- Cv.veg[x, y, c(1:10)]
        Cv.new[x, y, dim(Cv.new)[3]] <- 1.0 - crop.f - veg.f
      } else {
        rescale.f <- (1 - crop.f) / nat.f
        Cv.new[x, y, c(1:10, dim(Cv.new)[3])] <- Cv.veg[x, y, c(1:10, 12)] * rescale.f
      }
    }

    ## Add bare soil to crop tiles for the coverage forcing
    if (crop.f > 0 && Cv.new[x, y, dim(Cv.new)[3]] == 0) {
      if (crop.f > 0.01) {
        rescale.f <- (sum(Cv.new[x, y, 11:13]) - 0.01) / sum(Cv.new[x, y, 11:13])
        Cv.new[x, y, dim(Cv.new)[3]] <- Cv.new[x, y, dim(Cv.new)[3]] + 0.01
        Cv.new[x, y, 11:13] <- Cv.new[x, y, 11:13] * rescale.f
      } else {
        rescale.f <- 0.5
        Cv.new[x, y, dim(Cv.new)[3]] <- Cv.new[x, y, dim(Cv.new)[3]] + sum(Cv.new[x, y, 11:13]) * (1 - rescale.f)
        Cv.new[x, y, 11:13] <- Cv.new[x, y, 11:13] * rescale.f
      }
    }

    Cv.new[x, y, ] <- Cv.new[x, y, ] / sum(Cv.new[x, y, ])
  }
}
Cv.sum <- apply(X = Cv.new, MARGIN = c(1, 2), FUN = sum)
image.plot(Cv.sum)

## Calculate Nveg
Nveg <- apply(X = Cv.new[, , 1:(dim(Cv.new)[3] - 1)], MARGIN = c(1, 2), FUN = function(x) {
  sum(x > 0)
})
image.plot(Nveg)

# Save
system(command = paste0("ncks -h -x -v ", paste0(c(veg.vars, "veg_class"), collapse = ","), " ", vegetation.file, " -O ", vegetation.out))
addVegVars(nc.file = vegetation.out, nveg_class = 14)

put.old(vegetation.out, vegetation.file)
put.maps(vegetation.out, rain.filled, 11)
put.maps(vegetation.out, irr.filled, 12)
put.maps(vegetation.out, paddy.filled, 13)

nc <- nc_open(vegetation.out, write = T)
ncvar_put(nc, "Cv", Cv.new)
ncvar_put(nc, "Nveg", Nveg)
nc_close(nc)
