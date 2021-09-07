library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
map.support.file <- "../../../../Scripts/Support/mapFunctions.R"
generate.support.file <- "../../../../Scripts/Support/generateFunctions.R"
mask.file <- "../../../../Data/Transformed/Routing/mask_5min_Indus.RDS"

cv.file <- "./Saves/Cv_5min_Indus.RDS"

root.fract.file <- "./Saves/root_frac_5min_Indus.RDS"
root.depth.file <- "./Saves/root_depth_5min_Indus.RDS"

lai.file <- "./Saves/LAI_5min_Indus.RDS"
displacement.file <- "./Saves/displacement_5min_Indus.RDS"
veg.rough.file <- "./Saves/veg_rough_5min_Indus.RDS"
albedo.file <- "./Saves/albedo_5min_Indus.RDS"
fcanopy.file <- "./Saves/fcanopy_5min_Indus.RDS"

overstory.file <- "./Saves/overstory_5min_Indus.RDS"
rarc.file <- "./Saves/rarc_5min_Indus.RDS"
rmin.file <- "./Saves/rmin_5min_Indus.RDS"
rgl.file <- "./Saves/rgl_5min_Indus.RDS"
sol.atten.file <- "./Saves/sol_atten_5min_Indus.RDS"
wind.atten.file <- "./Saves/wind_atten_5min_Indus.RDS"
trunk.ratio.file <- "./Saves/trunk_ratio_5min_Indus.RDS"
wind.h.file <- "./Saves/wind_h_5min_Indus.RDS"

vic.orig <- "../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out <- "../../../../Data/VIC/Parameters/Indus_5min/vegetation_params_Modis_Indus.nc"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
source(map.support.file)
source(generate.support.file)
mask <- readRDS(mask.file)

cv <- readRDS(cv.file)
root.fract <- readRDS(root.fract.file)
root.depth <- readRDS(root.depth.file)
lai <- readRDS(lai.file)
displacement <- readRDS(displacement.file)
veg.rough <- readRDS(veg.rough.file)
albedo <- readRDS(albedo.file)
fcanopy <- readRDS(fcanopy.file)
overstory <- readRDS(overstory.file)
rarc <- readRDS(rarc.file)
rmin <- readRDS(rmin.file)
rgl <- readRDS(rgl.file)
sol.atten <- readRDS(sol.atten.file)
wind.atten <- readRDS(wind.atten.file)
trunk.ratio <- readRDS(trunk.ratio.file)
wind.h <- readRDS(wind.h.file)

# Setup
na.map <- is.na(mask) | mask == 0

# Set vegetation without LAI, fcanopy, or albedo to NA
v = 13
for(v in 1:(dim(cv)[3] - 1)){
  lai.tmp = lai[,,v,]
  fcanopy.tmp = fcanopy[,,v,]
  albedo.tmp = albedo[,,v,]
  
  lai.tmp[lai.tmp == 0] = NA
  fcanopy.tmp[fcanopy.tmp == 0] = NA
  albedo.tmp[albedo.tmp == 0] = NA
  
  lai[,,v,] = lai.tmp
  fcanopy[,,v,] = fcanopy.tmp
  albedo[,,v,] = albedo.tmp
}

# Set bare soil lai to 0
lai[,,14,] = 0
# Set bare soil fcanopy to 0.0001
fcanopy[,,14,] = 0.0001
# Set bare soil albedo to 0.2
albedo[,,14,] = 0.2

# Set minimum fcanopy
fcanopy[fcanopy < 0.0001] = 0.0001

# Calculate
Nveg <- apply(X = cv, MARGIN = c(1, 2), FUN = function(x) {
  sum(na.omit(x[1:(length(x) - 1)]) > 0)
})

Nveg.fill <- fillMap(Nveg, na.map, getNearestZero)
cv.fill <- fillMap(cv, na.map, getNearestZero)

root.fract.fill <- root.fract
root.depth.fill <- root.depth
lai.fill <- lai
displacement.fill <- displacement
veg.rough.fill <- veg.rough
albedo.fill <- albedo
fcanopy.fill <- fcanopy
overstory.fill <- overstory
rarc.fill <- rarc
rmin.fill <- rmin
rgl.fill <- rgl
sol.atten.fill <- sol.atten
wind.atten.fill <- wind.atten
trunk.ratio.fill <- trunk.ratio
wind.h.fill <- wind.h
for(v in 1:(dim(cv.fill)[3] - 1)){
  na.map = is.na(cv.fill[,,v]) | cv.fill[,,v] <= 0
  root.fract.fill[,,v,] <- fillMap(root.fract[,,v,], na.map, getNearestMean)
  root.depth.fill[,,v,] <- fillMap(root.depth[,,v,], na.map, getNearestMean)
  lai.fill[,,v,] <- fillMap(lai[,,v,], na.map, getNearestMean)
  displacement.fill[,,v,] <- fillMap(displacement[,,v,], na.map, getNearestMean)
  veg.rough.fill[,,v,] <- fillMap(veg.rough[,,v,], na.map, getNearestMean)
  albedo.fill[,,v,] <- fillMap(albedo[,,v,], na.map, getNearestMean)
  fcanopy.fill[,,v,] <- fillMap(fcanopy[,,v,], na.map, getNearestMean)
  overstory.fill[,,v] <- fillMap(overstory[,,v], na.map, getNearestMean)
  rarc.fill[,,v] <- fillMap(rarc[,,v], na.map, getNearestMean)
  rmin.fill[,,v] <- fillMap(rmin[,,v], na.map, getNearestMean)
  rgl.fill[,,v] <- fillMap(rgl[,,v], na.map, getNearestMean)
  sol.atten.fill[,,v] <- fillMap(sol.atten[,,v], na.map, getNearestMean)
  wind.atten.fill[,,v] <- fillMap(wind.atten[,,v], na.map, getNearestMean)
  trunk.ratio.fill[,,v] <- fillMap(trunk.ratio[,,v], na.map, getNearestMean)
  wind.h.fill[,,v] <- fillMap(wind.h[,,v], na.map, getNearestMean)
}

# Adjust vegetation with fractional LAI < 1
fcanopy.fill.tmp = fcanopy.fill[,,1:13,]
lai.fill.tmp = lai.fill[,,1:13,]
sel = !is.na(lai.fill.tmp) & lai.fill.tmp / fcanopy.fill.tmp < 1  & fcanopy.fill.tmp > 0.0001
fcanopy.fill.tmp[sel] = lai.fill.tmp[sel]
fcanopy.fill[,,1:13,] = fcanopy.fill.tmp

# Set minimum fcanopy
fcanopy.fill[fcanopy.fill < 0.0001] = 0.0001

# Create
dim.lon <- ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = out.lons,
  longname = "longitude of grid cell center"
)
dim.lat <- ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = out.lats,
  longname = "latitude of grid cell center"
)
dim.veg_class <- ncdim_def(
  name = "veg_class",
  units = "class",
  vals = 1:dim(cv)[3],
  longname = paste0("vegetation class [", 
                    " 1 = Evergreen needleleaf forests",
                    " 2 = Evergreen broadleaf forests",
                    " 3 = Deciduous needleleaf forests",
                    " 4 = Deciduous broadleaf forests",
                    " 5 = Mixed forests",
                    " 6 = Closed shrublands",
                    " 7 = Open shrublands",
                    " 8 = Woody savannas",
                    " 9 = Savannas",
                    " 10 = Grasslands",
                    " 11 = Permanent wetlands",
                    " 12 = Croplands",
                    " 13 = Urband and build-up lands",
                    " 14 = Barren",
                    "]")
)

nc = nc_open(vic.orig)
dim.month = nc$dim$month
dim.root_zone = nc$dim$root_zone
nc_close(nc)

var.list = list()
for(var in c(veg.vars)) {
  print(var)
  if(var %in% c("Nveg")){
    var.var = cropyCreateVariable(base.file = vic.orig, base.name = var,
                                   dim = list(dim.lon, dim.lat), 
                                   chunksizes = c(length(out.lons), length(out.lats)))
  } else if(var %in% c("Cv", "overstory", "rarc", "rmin", "RGL", "rad_atten", "wind_atten", "trunk_ratio", "wind_h")){
    var.var = cropyCreateVariable(base.file = vic.orig, base.name = var,
                                   dim = list(dim.lon, dim.lat, dim.veg_class), 
                                   chunksizes = c(length(out.lons), length(out.lats), 1))
  } else if (var %in% c("veg_rough", "displacement", "albedo", "fcanopy", "LAI")) {
    var.var = cropyCreateVariable(base.file = vic.orig, base.name = var,
                                  dim = list(dim.lon, dim.lat, dim.month, dim.veg_class), 
                                  chunksizes = c(length(out.lons), length(out.lats), 1, 1))
  } else if (var %in% c("root_depth", "root_fract")) {
    var.var = cropyCreateVariable(base.file = vic.orig, base.name = var,
                                  dim = list(dim.lon, dim.lat, dim.root_zone, dim.veg_class), 
                                  chunksizes = c(length(out.lons), length(out.lats), 1, 1))
  }
  
  var.list[[var]] = var.var
}
var.list[["fcanopy"]] = fcanopy.var <- ncvar_def(name = "fcanopy", 
                                                 units = "#", 
                                                 dim = list(dim.lon, dim.lat, dim.month, dim.veg_class), 
                                                 longname = "Canopy cover fraction", 
                                                 prec = "double", missval = -1)

nc = nc_create(vic.out, vars = var.list)
nc_close(nc)

# Save
nc <- nc_open(vic.out, write = T)
ncvar_put(nc, "Nveg", Nveg.fill)
ncvar_put(nc, "Cv", cv.fill)
ncvar_put(nc, "root_fract", aperm(root.fract.fill, perm = c(1, 2, 4, 3)))
ncvar_put(nc, "root_depth", aperm(root.depth.fill, perm = c(1, 2, 4, 3)))
ncvar_put(nc, "LAI", aperm(lai.fill, perm = c(1, 2, 4, 3)))
ncvar_put(nc, "displacement", aperm(displacement.fill, perm = c(1, 2, 4, 3)))
ncvar_put(nc, "veg_rough", aperm(veg.rough.fill, perm = c(1, 2, 4, 3)))
ncvar_put(nc, "albedo", aperm(albedo.fill, perm = c(1, 2, 4, 3)))
ncvar_put(nc, "fcanopy", aperm(fcanopy.fill, perm = c(1, 2, 4, 3)))
ncvar_put(nc, "overstory", overstory.fill)
ncvar_put(nc, "rarc", rarc.fill)
ncvar_put(nc, "rmin", rmin.fill)
ncvar_put(nc, "RGL", rgl.fill)
ncvar_put(nc, "rad_atten", sol.atten.fill)
ncvar_put(nc, "wind_atten", wind.atten.fill)
ncvar_put(nc, "trunk_ratio", trunk.ratio.fill)
ncvar_put(nc, "wind_h", wind.h.fill)
nc_close(nc)
