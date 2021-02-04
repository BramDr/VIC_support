library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
map.support.file <- "../../../../Scripts/Support/mapFunctions.R"
generate.support.file <- "../../../../Scripts/Support/generateFunctions.R"
mask.file <- "../../../../Data/Transformed/Routing/distance_5min_Indus.RDS"

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

# Set bare soil lai to 0
m = 1
for(m in 1:dim(lai)[4]){
  z = dim(lai)[3]
  
  lai.tmp = lai[,,z,m]
  
  sel = cv[,,z] > 0
  lai.tmp[sel] = 0
  
  lai[,,z,m] = lai.tmp
}
image.plot(lai[,,13,1])

# Set bare soil fcanopy to 0.1
m = 1
for(m in 1:dim(fcanopy)[4]){
  z = dim(fcanopy)[3]
  
  fcanopy.tmp = fcanopy[,,z,m]
  
  sel = cv[,,z] > 0
  fcanopy.tmp[sel] = 0.01
  
  fcanopy[,,z,m] = fcanopy.tmp
}
image.plot(fcanopy[,,13,1])

# Set bare soil albedo to 0.2
m = 1
for(m in 1:dim(albedo)[4]){
  z = dim(albedo)[3]
  
  albedo.tmp = albedo[,,z,m]
  
  sel = cv[,,z] > 0
  albedo.tmp[sel] = 0.2
  
  albedo[,,z,m] = albedo.tmp
}
image.plot(albedo[,,dim(albedo)[3],1])

# Calculate
Nveg <- apply(X = cv, MARGIN = c(1, 2), FUN = function(x) {
  sum(na.omit(x[1:(length(x) - 1)]) > 0)
})

Nveg.fill <- fillMap(Nveg, na.map, getNearestZero)
cv.fill <- fillMap(cv, na.map, getNearestZero)
root.fract.fill <- fillMap(root.fract, na.map, getNearestMean)
root.depth.fill <- fillMap(root.depth, na.map, getNearestMean)
lai.fill <- fillMap(lai, na.map, getNearestMean)
displacement.fill <- fillMap(displacement, na.map, getNearestMean)
veg.rough.fill <- fillMap(veg.rough, na.map, getNearestMean)
albedo.fill <- fillMap(albedo, na.map, getNearestMean)
fcanopy.fill <- fillMap(fcanopy, na.map, getNearestMean)
overstory.fill <- fillMap(overstory, na.map, getNearestMean)
rarc.fill <- fillMap(rarc, na.map, getNearestMean)
rmin.fill <- fillMap(rmin, na.map, getNearestMean)
rgl.fill <- fillMap(rgl, na.map, getNearestMean)
sol.atten.fill <- fillMap(sol.atten, na.map, getNearestMean)
wind.atten.fill <- fillMap(wind.atten, na.map, getNearestMean)
trunk.ratio.fill <- fillMap(trunk.ratio, na.map, getNearestMean)
wind.h.fill <- fillMap(wind.h, na.map, getNearestMean)

# Set filled Cv to bare soil and normalize
cv.sum = apply(X = cv.fill, MARGIN = c(1,2), FUN = sum)
cv.bare = cv.fill[,,dim(cv.fill)[3]]
cv.bare[cv.sum == 0] = 1
cv.fill[,,dim(cv.fill)[3]] = cv.bare

cv.sum = apply(X = cv.fill, MARGIN = c(1,2), FUN = sum)
for(z in 1:dim(cv.fill)[3]){
  cv.fill[,,z] = cv.fill[,,z] / cv.sum
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
