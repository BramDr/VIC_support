library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
map.support.file <- "../../../../Scripts/Support/mapFunctions.R"
generate.support.file <- "../../../../Scripts/Support/generateFunctions.R"
mask.file <- "../../../../Data/Transformed/Parameters/mask_Dahri_5min_Indus.RDS"

cv.file <- "../../../../Data/Transformed/Parameters/Cv_Dahri_5min_Indus.RDS"

root.fract.file <- "../../../../Data/Transformed/Parameters/rootFract_Dahri_5min_Indus.RDS"
root.depth.file <- "../../../../Data/Transformed/Parameters/rootDepth_Dahri_5min_Indus.RDS"

lai.file <- "../../../../Data/Transformed/Parameters/LAI_Dahri_5min_Indus.RDS"
displacement.file <- "../../../../Data/Transformed/Parameters/displacement_Dahri_5min_Indus.RDS"
veg.rough.file <- "../../../../Data/Transformed/Parameters/vegRough_Dahri_5min_Indus.RDS"
albedo.file <- "../../../../Data/Transformed/Parameters/albedo_Dahri_5min_Indus.RDS"

overstory.file <- "../../../../Data/Transformed/Parameters/vegetationOverstory_Dahri_5min_Indus.RDS"
rarc.file <- "../../../../Data/Transformed/Parameters/vegetationRarc_Dahri_5min_Indus.RDS"
rmin.file <- "../../../../Data/Transformed/Parameters/vegetationRmin_Dahri_5min_Indus.RDS"
rgl.file <- "../../../../Data/Transformed/Parameters/vegetationRgl_Dahri_5min_Indus.RDS"
sol.atten.file <- "../../../../Data/Transformed/Parameters/vegetationSolarAttenuation_Dahri_5min_Indus.RDS"
wind.atten.file <- "../../../../Data/Transformed/Parameters/vegetationWindAttenuation_Dahri_5min_Indus.RDS"
trunk.ratio.file <- "../../../../Data/Transformed/Parameters/vegetationTrunkRatio_Dahri_5min_Indus.RDS"
wind.h.file <- "../../../../Data/Transformed/Parameters/vegetationWindh_Dahri_5min_Indus.RDS"

vic.orig <- "../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out <- "../../../../Data/VIC/Parameters/Indus_5min/vegetation_params_Dahri_Indus.nc"

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

# Calculate
Nveg <- apply(X = cv, MARGIN = c(1, 2), FUN = function(x) {
  sum(na.omit(x[1:(length(x) - 1)]) > 0)
})

Nveg.fill <- fillMap(Nveg, na.map, getNearestZero)
cv.fill <- fillMap(cv, na.map, getNearestZero)
displacement.fill <- fillMap(displacement, na.map, getNearestMean)
veg.rough.fill <- fillMap(veg.rough, na.map, getNearestMean)
overstory.fill <- fillMap(overstory, na.map, getNearestMean)
rarc.fill <- fillMap(rarc, na.map, getNearestMean)
rmin.fill <- fillMap(rmin, na.map, getNearestMean)
rgl.fill <- fillMap(rgl, na.map, getNearestMean)
sol.atten.fill <- fillMap(sol.atten, na.map, getNearestMean)
wind.atten.fill <- fillMap(wind.atten, na.map, getNearestMean)
trunk.ratio.fill <- fillMap(trunk.ratio, na.map, getNearestMean)
wind.h.fill <- fillMap(wind.h, na.map, getNearestMean)

root.fract.fill = root.fract
root.depth.fill = root.depth
lai.fill = lai
albedo.fill = albedo
for(z in 1:dim(lai)[3]) {
  print(z)
  na.map.tmp <- is.na(mask) | mask == 0 | is.na(cv[,,z]) | cv[,,z] <= 0
  root.fract.fill[,,z,] <- fillMap(root.fract[,,z,], na.map.tmp, getNearestZero)
  root.depth.fill[,,z,] <- fillMap(root.depth[,,z,], na.map.tmp, getNearestZero)
  lai.fill[,,z,] <- fillMap(lai[,,z,], na.map.tmp, getNearestMean)
  albedo.fill[,,z,] <- fillMap(albedo[,,z,], na.map.tmp, getNearestMean)
}

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
                    " 6 = Woodland",
                    " 7 = Woody savannas",
                    " 8 = Closed shrublands",
                    " 9 = Open shrublands",
                    " 10 = Grasslands",
                    " 11 = Croplands",
                    " 12 = Sparse natural vegetation",
                    " 13 = Urband and build-up lands",
                    " 14 = Barren land",
                    " 15 = Snow and ice",
                    " 16 = Water",
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
ncvar_put(nc, "overstory", overstory.fill)
ncvar_put(nc, "rarc", rarc.fill)
ncvar_put(nc, "rmin", rmin.fill)
ncvar_put(nc, "RGL", rgl.fill)
ncvar_put(nc, "rad_atten", sol.atten.fill)
ncvar_put(nc, "wind_atten", wind.atten.fill)
ncvar_put(nc, "trunk_ratio", trunk.ratio.fill)
ncvar_put(nc, "wind_h", wind.h.fill)
nc_close(nc)
