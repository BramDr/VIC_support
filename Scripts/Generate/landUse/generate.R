library(fields)
library(ncdf4)
rm(list = ls())

# Input
function.script <- "generateFunctions.R"
param.irr.file <- "Saves/parameters_irrigated.RDS"
param.paddy.file <- "Saves/parameters_paddy.RDS"
param.rain.file <- "Saves/parameters_rainfed.RDS"
vegetation.file <- "Input/VIC_params_global.nc"
vegetation.out <- "Output/VIC_params_MIRCA2000_global.nc"

# Load
source(function.script)

nc <- nc_open(vegetation.file)
old.Cv <- ncvar_get(nc, nc$var$Cv)
nc_close(nc)

param.irr <- readRDS(param.irr.file)
param.paddy <- readRDS(param.paddy.file)
param.rain <- readRDS(param.rain.file)

# Setup
remove.vars <- c(
  "veg_descr", "veg_class", "Nveg", "Cv", "wind_atten",
  "wind_h", "rmin", "rarc", "rad_atten", "RGL",
  "trunk_ratio", "overstory", "root_fract", "root_depth",
  "LAI", "displacement", "veg_rough", "albedo"
)
system(command = paste0("ncks -x -v ", paste0(remove.vars, collapse = ","), " ", vegetation.file, " -O ", vegetation.out))

add.data <- function(name, nc.orig, nc.new, data.paddy, data.irr, data.rain) {
  data.old <- ncvar_get(nc.old, name)

  ncvar_put(nc.new, name, data.old[, , 1:10], start = c(1, 1, 1), count = c(-1, -1, 10))
  ncvar_put(nc.new, name, data.rain[[name]], start = c(1, 1, 11), count = c(-1, -1, 1))
  ncvar_put(nc.new, name, data.irr[[name]], start = c(1, 1, 12), count = c(-1, -1, 1))
  ncvar_put(nc.new, name, data.paddy[[name]], start = c(1, 1, 13), count = c(-1, -1, 1))
  ncvar_put(nc.new, name, data.old[, , 12], start = c(1, 1, 14), count = c(-1, -1, 1))
}

add.data.monthly <- function(name, nc.orig, nc.new, data.paddy, data.irr, data.rain) {
  if (name == "Fcanopy") {
    name2 <- "fcanopy"
    data.old <- ncvar_get(nc.old, "albedo")
    for (x in 1:dim(data.old)[1]) {
      for (y in 1:dim(data.old)[2]) {
        if (is.na(data.old[x, y, 1, 1])) {
          next
        }

        for (z in 1:dim(data.old)[3]) {
          data.old[x, y, z, ] <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        }
      }
    }

    ncvar_put(nc.new, name2, data.old[, , , 1:10], start = c(1, 1, 1, 1), count = c(-1, -1, -1, 10))
    ncvar_put(nc.new, name2, data.rain[[name]], start = c(1, 1, 1, 11), count = c(-1, -1, -1, 1))
    ncvar_put(nc.new, name2, data.irr[[name]], start = c(1, 1, 1, 12), count = c(-1, -1, -1, 1))
    ncvar_put(nc.new, name2, data.paddy[[name]], start = c(1, 1, 1, 13), count = c(-1, -1, -1, 1))
    ncvar_put(nc.new, name2, data.old[, , , 12], start = c(1, 1, 1, 14), count = c(-1, -1, -1, 1))
  } else {
    data.old <- ncvar_get(nc.old, name)

    ncvar_put(nc.new, name, data.old[, , , 1:10], start = c(1, 1, 1, 1), count = c(-1, -1, -1, 10))
    ncvar_put(nc.new, name, data.rain[[name]], start = c(1, 1, 1, 11), count = c(-1, -1, -1, 1))
    ncvar_put(nc.new, name, data.irr[[name]], start = c(1, 1, 1, 12), count = c(-1, -1, -1, 1))
    ncvar_put(nc.new, name, data.paddy[[name]], start = c(1, 1, 1, 13), count = c(-1, -1, -1, 1))
    ncvar_put(nc.new, name, data.old[, , , 12], start = c(1, 1, 1, 14), count = c(-1, -1, -1, 1))
  }
}

add.data.root <- function(name, nc.orig, nc.new, data.paddy, data.irr, data.rain) {
  name2 <- name
  if (name == "root_frac.1") {
    name2 <- "root_fract"
    idx <- 1
  }
  if (name == "root_frac.2") {
    name2 <- "root_fract"
    idx <- 2
  }
  if (name == "root_frac.3") {
    name2 <- "root_fract"
    idx <- 3
  }
  if (name == "root_depth.1") {
    name2 <- "root_depth"
    idx <- 1
  }
  if (name == "root_depth.2") {
    name2 <- "root_depth"
    idx <- 2
  }
  if (name == "root_depth.3") {
    name2 <- "root_depth"
    idx <- 3
  }

  data.old <- ncvar_get(nc.old, name2)

  ncvar_put(nc.new, name2, data.old[, , idx, 1:10], start = c(1, 1, idx, 1), count = c(-1, -1, 1, 10))
  ncvar_put(nc.new, name2, data.rain[[name]], start = c(1, 1, idx, 11), count = c(-1, -1, 1, 1))
  ncvar_put(nc.new, name2, data.irr[[name]], start = c(1, 1, idx, 12), count = c(-1, -1, 1, 1))
  ncvar_put(nc.new, name2, data.paddy[[name]], start = c(1, 1, idx, 13), count = c(-1, -1, 1, 1))
  ncvar_put(nc.new, name2, data.old[, , idx, 12], start = c(1, 1, idx, 14), count = c(-1, -1, 1, 1))
}


# Calculate
new.Cv <- array(NA, dim = c(dim(old.Cv)[1:2], dim(old.Cv)[3] + 2))
new.Cv[, , 1:10] <- old.Cv[, , 1:10]
new.Cv[, , 11] <- param.rain$Cv
new.Cv[, , 12] <- param.irr$Cv
new.Cv[, , 13] <- param.paddy$Cv
new.Cv[, , 14] <- old.Cv[, , 12]

## adjust Cv for added or removed crop areas
sum.all <- apply(new.Cv, MARGIN = c(1, 2), FUN = sum)
sum.nCrop <- apply(new.Cv[, , c(1:10, 14)], MARGIN = c(1, 2), FUN = sum)
sum.crop <- apply(new.Cv[, , c(11:13)], MARGIN = c(1, 2), FUN = sum)
add <- 1 - sum.all

adj.Cv <- new.Cv
for (x in 1:dim(adj.Cv)[1]) {
  for (y in 1:dim(adj.Cv)[2]) {
    if (is.na(add[x, y]) || add[x, y] == 0) {
      next
    }

    if (sum.nCrop[x, y] != 0) {
      adj.fac <- 1 + (add[x, y] / sum.nCrop[x, y])
      adj.Cv[x, y, c(1:10, 14)] <- new.Cv[x, y, c(1:10, 14)] * adj.fac
    } else {
      new.veg <- rep(0, 14)
      for (z in c(1:10, 14)) {
        new.veg[z] <- mean(new.Cv[(x - 2):(x + 2), (y - 2):(y + 2), z], na.rm = T)
      }
      adj.Cv[x, y, c(1:10, 14)] <- (new.veg[c(1:10, 14)] / sum(new.veg[c(1:10, 14)])) * add[x, y]
    }
  }
}
sum.check <- apply(adj.Cv, MARGIN = c(1, 2), FUN = sum)
image.plot(sum.check)

Nveg <- apply(X = adj.Cv[, , 1:13], MARGIN = c(1, 2), FUN = function(x) {
  sum(x > 0)
})
image.plot(Nveg)

# Save
nc <- nc_open(filename = vegetation.out)
nc_close(nc = nc)

lon.dim <- nc$dim$lon
lat.dim <- nc$dim$lat
root.dim <- nc$dim$root_zone
month.dim <- nc$dim$month
veg.dim <- ncdim_def(
  name = "veg_class", units = "class", vals = 1:14,
  longname = "Vegetation class: 1 - Evergreen Needleleaf, 2 - Evergreen Broadleaf, 3 - Deciduous Needleaf, 
                    4 - Diciduous Broadleaf, 5 - Mixed Cover, 6 - Woodland, 7 - Wooded Grasslands, 8 - Closed Shrublands, 
                    9 - Open Shrublands, 10 - Grasslands, 11 - Rainfed Cropland, 12 - Irrigated Cropland, 13 - Paddy Cropland, 14 - Bare Soil"
)
addVegVars(vegetation.out, lon.dim, lat.dim, veg.dim, root.dim, month.dim)

nc <- nc_open(filename = vegetation.out, write = T)
nc.old <- nc_open(filename = vegetation.file)

ncatt_put(
  nc = nc,
  varid = 0,
  attname = "Description",
  attval = "VIC parameters using MIRCA2000. Created by Bram Droppers"
)

ncvar_put(nc, "Cv", adj.Cv)
ncvar_put(nc, "Nveg", Nveg)

add.data("wind_atten", nc.old, nc, param.paddy, param.irr, param.rain)
add.data("rmin", nc.old, nc, param.paddy, param.irr, param.rain)
add.data("rarc", nc.old, nc, param.paddy, param.irr, param.rain)
add.data("rad_atten", nc.old, nc, param.paddy, param.irr, param.rain)
add.data("RGL", nc.old, nc, param.paddy, param.irr, param.rain)
add.data("overstory", nc.old, nc, param.paddy, param.irr, param.rain)
add.data("trunk_ratio", nc.old, nc, param.paddy, param.irr, param.rain)
add.data.root("root_frac.1", nc.old, nc, param.paddy, param.irr, param.rain)
add.data.root("root_frac.2", nc.old, nc, param.paddy, param.irr, param.rain)
add.data.root("root_depth.1", nc.old, nc, param.paddy, param.irr, param.rain)
add.data.root("root_depth.2", nc.old, nc, param.paddy, param.irr, param.rain)
add.data.monthly("LAI", nc.old, nc, param.paddy, param.irr, param.rain)
add.data.monthly("displacement", nc.old, nc, param.paddy, param.irr, param.rain)
add.data.monthly("veg_rough", nc.old, nc, param.paddy, param.irr, param.rain)
add.data.monthly("albedo", nc.old, nc, param.paddy, param.irr, param.rain)
add.data.monthly("Fcanopy", nc.old, nc, param.paddy, param.irr, param.rain)

ncvar_put(nc, "root_fract", array(0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len)), start = c(1, 1, 3, 1), count = c(-1, -1, 1, -1))
ncvar_put(nc, "root_depth", array(0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len)), start = c(1, 1, 3, 1), count = c(-1, -1, 1, -1))

nc_close(nc.old)
nc_close(nc)


## check and adjust root fractions
nc <- nc_open(filename = vegetation.out)
Cv.check <- ncvar_get(nc, nc$var$Cv)
root.frac.check <- ncvar_get(nc, nc$var$root_fract)
root.depth.check <- ncvar_get(nc, nc$var$root_depth)
nc_close(nc)

root.frac.new <- root.frac.check
root.depth.new <- root.depth.check
for (x in 1:dim(Cv.check)[1]) {
  for (y in 1:dim(Cv.check)[2]) {
    for (v in 1:(dim(Cv.check)[3] - 1)) {
      if (is.na(Cv.check[x, y, v])) {
        next
      }

      if (Cv.check[x, y, v] > 0) {
        root.frac.sum <- sum(root.frac.new[x, y, , v])
        root.depth.sum <- sum(root.depth.new[x, y, , v])
        if (root.frac.sum <= 0 || root.depth.sum <= 0) {
          root.frac.new[x, y, 1, v] <- max(root.frac.check[, , 1, v], na.rm = T)
          root.frac.new[x, y, 2, v] <- max(root.frac.check[, , 2, v], na.rm = T)
          root.frac.new[x, y, 3, v] <- max(root.frac.check[, , 3, v], na.rm = T)
          root.depth.new[x, y, 1, v] <- max(root.depth.check[, , 1, v], na.rm = T)
          root.depth.new[x, y, 2, v] <- max(root.depth.check[, , 2, v], na.rm = T)
          root.depth.new[x, y, 3, v] <- max(root.depth.check[, , 3, v], na.rm = T)
        }

        root.frac.sum <- sum(root.frac.new[x, y, , v])
        root.depth.sum <- sum(root.depth.new[x, y, , v])
        if (root.frac.sum != 1 || root.depth.sum != 1) {
          root.frac.new[x, y, , v] <- root.frac.check[x, y, , v] / root.frac.sum
          root.depth.new[x, y, , v] <- root.depth.check[x, y, , v] / root.depth.sum
        }
      }
    }
  }
}

nc <- nc_open(filename = vegetation.out, write = T)
ncvar_put(nc, nc$var$root_fract, root.frac.new)
ncvar_put(nc, nc$var$root_depth, root.depth.new)
nc_close(nc)

## check and adjust canopy coverage
nc <- nc_open(filename = vegetation.out)
Cv.check <- ncvar_get(nc, nc$var$Cv)
fcanopy.check <- ncvar_get(nc, nc$var$fcanopy)
nc_close(nc)

fcanopy.new <- fcanopy.check
for (x in 1:dim(Cv.check)[1]) {
  for (y in 1:dim(Cv.check)[2]) {
    for (v in 1:(dim(Cv.check)[3] - 1)) {
      if (is.na(Cv.check[x, y, v])) {
        next
      }

      if (Cv.check[x, y, v] > 0) {
        sel <- fcanopy.check[x, y, , v] < 0.00011
        if (sum(sel) > 0) {
          fcanopy.new[x, y, sel, v] <- 0.00011
        }
      }
    }
  }
}

nc <- nc_open(filename = vegetation.out, write = T)
ncvar_put(nc, nc$var$fcanopy, fcanopy.new)
nc_close(nc)
