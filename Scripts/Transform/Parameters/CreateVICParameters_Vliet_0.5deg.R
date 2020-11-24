library(ncdf4)
library(fields)
rm(list = ls())

# Input
support.script <- "../../Support/mapFunctions.R"
dir <- "../../../Data/Transformed/Parameters"
pattern <- "Vliet30min"
vic.file <- "../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out <- "../../../Data/VIC/Parameters/global/VIC_params_Vliet30min_global.nc"

# Load
source(support.script)
files <- list.files(dir, pattern = pattern, full.names = T)

lai <- readRDS(grep(files, pattern = "LAI", value = T))
d1 <- readRDS(grep(files, pattern = "D1", value = T))
d2 <- readRDS(grep(files, pattern = "D2", value = T))
d3 <- readRDS(grep(files, pattern = "D3", value = T))
d4 <- readRDS(grep(files, pattern = "D4", value = T))
infilt <- readRDS(grep(files, pattern = "infilt", value = T))
depth <- readRDS(grep(files, pattern = "depth", value = T))
cv <- readRDS(grep(files, pattern = "cv", value = T))
overstory <- readRDS(grep(files, pattern = "Overstory", value = T))
rarc <- readRDS(grep(files, pattern = "Rarc", value = T))
rmin <- readRDS(grep(files, pattern = "Rmin", value = T))
windh <- readRDS(grep(files, pattern = "Windh", value = T))
rgl <- readRDS(grep(files, pattern = "Rgl", value = T))
solatn <- readRDS(grep(files, pattern = "SolarAttenuation", value = T))
windatn <- readRDS(grep(files, pattern = "WindAttenuation", value = T))
trunk <- readRDS(grep(files, pattern = "TrunkRatio", value = T))
expt <- readRDS(grep(files, pattern = "soilExpt", value = T))
ksat <- readRDS(grep(files, pattern = "soilKsat", value = T))
phi_s <- readRDS(grep(files, pattern = "Phis", value = T))
bubble <- readRDS(grep(files, pattern = "Bubble", value = T))
quartz <- readRDS(grep(files, pattern = "Quartz", value = T))
bulk_density <- readRDS(grep(files, pattern = "BulkDensity", value = T))
soil_density <- readRDS(grep(files, pattern = "SoilDensity", value = T))
wcr <- readRDS(grep(files, pattern = "Wcr", value = T))
wp <- readRDS(grep(files, pattern = "Wp", value = T))
residual <- readRDS(grep(files, pattern = "Residual", value = T))
init_moist <- readRDS(grep(files, pattern = "InitMoist", value = T))
nveg <- apply(cv, c(1, 2), function(x) {
  return(sum(x[1:(length(x) - 1)] > 0, na.rm = T))
})

nc <- nc_open(vic.file)
Cv <- ncvar_get(nc, "Cv")
root_fract <- ncvar_get(nc, "root_fract")
root_depth <- ncvar_get(nc, "root_depth")
nc_close(nc)

for (l in 1:dim(root_fract)[3]) {
  for (v in 1:dim(root_fract)[4]) {
    root_fract[, , l, v] <- max(root_fract[, , l, v], na.rm = T)
    root_depth[, , l, v] <- max(root_depth[, , l, v], na.rm = T)
  }
}

# Setup
na.map <- is.na(Cv[, , 1])

# Calculate
missing.map <- array(NA, dim = dim(cv))
zero.map <- array(NA, dim = dim(cv))
lai.missing <- lai
for (i in 1:(dim(cv)[3] - 1)) {
  print(i)
  for (x in 1:dim(cv)[1]) {
    for (y in 1:dim(cv)[2]) {
      if (is.na(cv[x, y, i])) {
        next
      }

      missing.map[x, y, i] <- 0
      zero.map[x, y, i] <- 0

      if (cv[x, y, i] <= 0) {
        next
      }

      if (is.na(lai[x, y, i, 1])) {
        missing.map[x, y, i] <- 1

        for (j in 1:dim(lai)[4]) {
          lai.missing[x, y, i, j] <- getNearestMean(lai[, , i, j], x, y)
        }
      }
    }
  }
}
for (i in 1:(dim(missing.map)[3] - 1)) {
  image.plot(missing.map[, , i], main = i)
}

d1.fill <- fillMap(
  map = d1,
  na.map = na.map,
  nearest.function = getNearestMean
)
d2.fill <- fillMap(
  map = d2,
  na.map = na.map,
  nearest.function = getNearestMean
)
d3.fill <- fillMap(
  map = d3,
  na.map = na.map,
  nearest.function = getNearestMean
)
d4.fill <- fillMap(
  map = d4,
  na.map = na.map,
  nearest.function = getNearestMean
)
infilt.fill <- fillMap(
  map = infilt,
  na.map = na.map,
  nearest.function = getNearestMean
)
nveg.fill <- fillMap(
  map = nveg,
  na.map = na.map,
  nearest.function = getNearestZero
)

lai.fill <- lai.missing
cv.fill <- cv
overstory.fill <- overstory
rarc.fill <- rarc
rmin.fill <- rmin
windh.fill <- windh
rgl.fill <- rgl
solatn.fill <- solatn
windatn.fill <- windatn
trunk.fill <- trunk
root_fract.fill <- root_fract
root_depth.fill <- root_depth
depth.fill <- depth
expt.fill <- expt
ksat.fill <- ksat
phi_s.fill <- phi_s
bubble.fill <- bubble
quartz.fill <- quartz
bulk_density.fill <- bulk_density
soil_density.fill <- soil_density
wcr.fill <- wcr
wp.fill <- wp
residual.fill <- residual
init_moist.fill <- init_moist
for (i in 1:dim(lai.missing)[3]) {
  print(i)

  cv.fill[, , i] <- fillMap(
    map = cv[, , i],
    na.map = na.map,
    nearest.function = getNearestZero
  )
  overstory.fill[, , i] <- fillMap(
    map = overstory[, , i],
    na.map = na.map,
    nearest.function = getNearestZero
  )
  rarc.fill[, , i] <- fillMap(
    map = rarc[, , i],
    na.map = na.map,
    nearest.function = getNearestZero
  )
  rmin.fill[, , i] <- fillMap(
    map = rmin[, , i],
    na.map = na.map,
    nearest.function = getNearestZero
  )
  windh.fill[, , i] <- fillMap(
    map = windh[, , i],
    na.map = na.map,
    nearest.function = getNearestZero
  )
  rgl.fill[, , i] <- fillMap(
    map = rgl[, , i],
    na.map = na.map,
    nearest.function = getNearestZero
  )
  solatn.fill[, , i] <- fillMap(
    map = solatn[, , i],
    na.map = na.map,
    nearest.function = getNearestZero
  )
  windatn.fill[, , i] <- fillMap(
    map = windatn[, , i],
    na.map = na.map,
    nearest.function = getNearestZero
  )
  trunk.fill[, , i] <- fillMap(
    map = trunk[, , i],
    na.map = na.map,
    nearest.function = getNearestZero
  )
  if (i == dim(lai.missing)[3]) {
    rarc.fill[, , i] <- fillMap(
      map = rarc[, , i],
      na.map = na.map,
      nearest.function = getNearestValue, value = 100
    )
    windh.fill[, , i] <- fillMap(
      map = windh[, , i],
      na.map = na.map,
      nearest.function = getNearestValue, value = 2
    )
  }

  for (j in 1:dim(lai.missing)[4]) {
    lai.fill[, , i, j] <- fillMap(
      map = lai.missing[, , i, j],
      na.map = na.map,
      nearest.function = getNearestZero
    )
  }
  for (j in 1:dim(root_fract)[3]) {
    root_fract.fill[, , j, i] <- fillMap(
      map = root_fract[, , j, i],
      na.map = na.map,
      nearest.function = getNearestMean
    )
    root_depth.fill[, , j, i] <- fillMap(
      map = root_depth[, , j, i],
      na.map = na.map,
      nearest.function = getNearestMean
    )
  }
}
for (i in 1:dim(expt)[3]) {
  print(i)

  depth.fill[, , i] <- fillMap(
    map = depth[, , i],
    na.map = na.map,
    nearest.function = getNearestMean
  )
  expt.fill[, , i] <- fillMap(
    map = expt[, , i],
    na.map = na.map,
    nearest.function = getNearestMean
  )
  ksat.fill[, , i] <- fillMap(
    map = ksat[, , i],
    na.map = na.map,
    nearest.function = getNearestMean
  )
  phi_s.fill[, , i] <- fillMap(
    map = phi_s[, , i],
    na.map = na.map,
    nearest.function = getNearestMean
  )
  bubble.fill[, , i] <- fillMap(
    map = bubble[, , i],
    na.map = na.map,
    nearest.function = getNearestMean
  )
  quartz.fill[, , i] <- fillMap(
    map = quartz[, , i],
    na.map = na.map,
    nearest.function = getNearestMean
  )
  bulk_density.fill[, , i] <- fillMap(
    map = bulk_density[, , i],
    na.map = na.map,
    nearest.function = getNearestMean
  )
  soil_density.fill[, , i] <- fillMap(
    map = soil_density[, , i],
    na.map = na.map,
    nearest.function = getNearestMean
  )
  wcr.fill[, , i] <- fillMap(
    map = wcr[, , i],
    na.map = na.map,
    nearest.function = getNearestMean
  )
  wp.fill[, , i] <- fillMap(
    map = wp[, , i],
    na.map = na.map,
    nearest.function = getNearestMean
  )
  residual.fill[, , i] <- fillMap(
    map = residual[, , i],
    na.map = na.map,
    nearest.function = getNearestMean
  )
  init_moist.fill[, , i] <- fillMap(
    map = init_moist[, , i],
    na.map = na.map,
    nearest.function = getNearestMean
  )
}
image.plot(cv.fill[, , 1])
image.plot(expt[, , 1])
image.plot(d1.fill)
image.plot(d2.fill)
image.plot(d3.fill)
image.plot(d4.fill)
image.plot(infilt.fill)

# Save
dir.create(dirname(vic.out))
file.copy(from = vic.file, to = vic.out, overwrite = T)

nc <- nc_open(vic.out, write = T)
ncvar_put(nc, "LAI", aperm(lai.fill, c(1, 2, 4, 3)))
ncvar_put(nc, "Nveg", nveg.fill)
ncvar_put(nc, "Ds", d1.fill)
ncvar_put(nc, "Dsmax", d2.fill)
ncvar_put(nc, "Ws", d3.fill)
ncvar_put(nc, "c", d4.fill)
ncvar_put(nc, "infilt", infilt.fill)
ncvar_put(nc, "depth", depth.fill)
ncvar_put(nc, "Cv", cv.fill)
ncvar_put(nc, "overstory", overstory.fill)
ncvar_put(nc, "rarc", rarc.fill)
ncvar_put(nc, "rmin", rmin.fill)
ncvar_put(nc, "wind_h", windh.fill)
ncvar_put(nc, "RGL", rgl.fill)
ncvar_put(nc, "rad_atten", solatn.fill)
ncvar_put(nc, "wind_atten", windatn.fill)
ncvar_put(nc, "trunk_ratio", trunk.fill)
ncvar_put(nc, "root_fract", root_fract.fill)
ncvar_put(nc, "root_depth", root_depth.fill)
ncvar_put(nc, "expt", expt.fill)
ncvar_put(nc, "Ksat", ksat.fill)
ncvar_put(nc, "phi_s", phi_s.fill)
ncvar_put(nc, "bubble", bubble.fill)
ncvar_put(nc, "quartz", quartz.fill)
ncvar_put(nc, "bulk_density", bulk_density.fill)
ncvar_put(nc, "soil_density", soil_density.fill)
ncvar_put(nc, "Wcr_FRACT", wcr.fill)
ncvar_put(nc, "Wpwp_FRACT", wp.fill)
ncvar_put(nc, "resid_moist", residual.fill)
ncvar_put(nc, "init_moist", init_moist.fill)
nc_close(nc)
