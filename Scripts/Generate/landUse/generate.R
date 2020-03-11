library(fields)
library(ncdf4)
rm(list = ls())

# Input
function.script <- "../../Support/generateFunctions.R"
map.script <- "../../Support/mapFunctions.R"
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
paddy.file <- "Saves/parametersPaddy_30min_global.RDS"
irr.file <- "Saves/parametersIrrigated_30min_global.RDS"
rain.file <- "Saves/parametersRainfed_30min_global.RDS"
vegetation.file <- "../../../Data/Transformed/Parameters/global/VIC_params_VlietAlt30min_global.nc"
vegetation.out <- "../../../Data/VIC/Parameters/global/VIC_params_MIRCA2000_global.nc"

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
fill.maps <- function(maps) {
  maps.new <- list()
  for (i in 1:length(maps)) {
    map <- maps[[i]]

    for (x in 1:dim(map)[1]) {
      for (y in 1:dim(map)[2]) {
        if (is.na(mask[x, y])) {
          next
        }

        if (length(dim(map)) == 3) {
          if (is.na(map[x, y, 1])) {
            for (z in 1:dim(map)[3]) {
              map[x, y, z] <- getNearestZero(map[, , z], x, y)
            }
          }
        } else {
          if (is.na(map[x, y])) {
            map[x, y] <- getNearestZero(map, x, y)
          }
        }
      }
    }

    maps.new[[names(maps)[i]]] <- map
  }

  return(maps.new)
}
put.maps <- function(nc.file, maps, veg.idx, mask) {
  empty.map <- mask * 0

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
  ncvar_put(nc, "root_depth", empty.map, start = c(1, 1, 3, veg.idx), count = c(-1, -1, 1, 1))
  ncvar_put(nc, "root_fract", maps[["root_frac.1"]], start = c(1, 1, 1, veg.idx), count = c(-1, -1, 1, 1))
  ncvar_put(nc, "root_fract", maps[["root_frac.2"]], start = c(1, 1, 2, veg.idx), count = c(-1, -1, 1, 1))
  ncvar_put(nc, "root_fract", empty.map, start = c(1, 1, 3, veg.idx), count = c(-1, -1, 1, 1))

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
irr.filled <- fill.maps(irr)
paddy.filled <- fill.maps(paddy)
rain.filled <- fill.maps(rain)

## Calculate adjusted Cv fractions and Nveg
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
    veg.f <- sum(Cv.veg[x, y, c(1:10, 12)])

    if (crop.f > 1) {
      Cv.new[x, y, 11:13] <- Cv.new[x, y, 11:13] / crop.f
      crop.f <- 1
    }

    if (veg.f <= 0) {
      if (crop.f == 1) {
        next
      }
      Cv.new[x, y, dim(Cv.new)[3]] <- (1 - crop.f)
    } else {
      rescale.f <- (1 - crop.f) / veg.f
      Cv.new[x, y, c(1:10, 14)] <- Cv.veg[x, y, c(1:10, 12)] * rescale.f
    }
  }
}
Cv.sum <- apply(X = Cv.new, MARGIN = c(1, 2), FUN = sum)
image.plot(Cv.sum)

Nveg <- apply(X = Cv.new[, , 1:(dim(Cv.new)[3] - 1)], MARGIN = c(1, 2), FUN = function(x) {
  sum(x > 0)
})
image.plot(Nveg)

removeVegVars(nc.old.file = vegetation.file, nc.new.file = vegetation.out)
addVegVars(nc.file = vegetation.out, nveg_class = 14)

put.old(vegetation.out, vegetation.file)
put.maps(vegetation.out, rain.filled, 11, mask)
put.maps(vegetation.out, irr.filled, 12, mask)
put.maps(vegetation.out, paddy.filled, 13, mask)

nc <- nc_open(vegetation.out, write = T)
ncvar_put(nc, "Cv", Cv.new)
ncvar_put(nc, "Nveg", Nveg)
nc_close(nc)
