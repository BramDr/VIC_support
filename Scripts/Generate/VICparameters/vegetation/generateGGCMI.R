library(fields)
library(ncdf4)
rm(list = ls())

# Input
map.script = "../../../../Scripts/Support/mapFunctions.R"
generate.script <- "../../../../Scripts/Support/generateFunctions.R"
mask.file <- "../../../../Data/Primary/VIC/domain_global.nc"
vegetation.file <- "../../../../Data/VIC/Parameters/global/vegetation_params_Vliet_global.nc"
vegetation.out <- "../../../../Data/VIC/Parameters/global/vegetation_params_GGCMI_global.nc"

# Load
source(map.script)
source(generate.script)

nc = nc_open(mask.file)
mask = ncvar_get(nc, "mask")
nc_close(nc)

# Setup
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
veg_class = 2
nmonth = 12
nroot = 3

na.map = is.na(mask) | mask == 0

Nveg = array(1, dim = c(length(lons), length(lats)))
cv = array(1, dim = c(length(lons), length(lats), veg_class))
root.fract = array(0, dim = c(length(lons), length(lats), veg_class, nroot))
root.depth = array(0, dim = c(length(lons), length(lats), veg_class, nroot))
lai = array(2, dim = c(length(lons), length(lats), veg_class, nmonth))
displacement = array(1 * 0.67, dim = c(length(lons), length(lats), veg_class, nmonth))
veg.rough = array(1 * 0.123, dim = c(length(lons), length(lats), veg_class, nmonth))
albedo = array(0.1, dim = c(length(lons), length(lats), veg_class, nmonth))
fcanopy = array(1, dim = c(length(lons), length(lats), veg_class, nmonth))
overstory = array(0, dim = c(length(lons), length(lats), veg_class))
rarc = array(2, dim = c(length(lons), length(lats), veg_class))
rmin = array(80, dim = c(length(lons), length(lats), veg_class))
rgl = array(100, dim = c(length(lons), length(lats), veg_class))
sol.atten = array(0.5, dim = c(length(lons), length(lats), veg_class))
wind.atten = array(0.5, dim = c(length(lons), length(lats), veg_class))
trunk.ratio = array(0.2, dim = c(length(lons), length(lats), veg_class))
wind.h = array(2, dim = c(length(lons), length(lats), veg_class))

cv[,,dim(cv)[3]] = 0
root.fract[,,1:(dim(root.fract)[3] - 1),1] = 0.5
root.depth[,,1:(dim(root.fract)[3] - 1),1] = 0.3
root.fract[,,1:(dim(root.fract)[3] - 1),2] = 0.5
root.depth[,,1:(dim(root.fract)[3] - 1),2] = 0.7
lai[,,dim(lai)[3],] = 0
displacement[,,dim(displacement)[3],] = 0
veg.rough[,,dim(veg.rough)[3],] = 0
albedo[,,dim(albedo)[3],] = 0.2
fcanopy[,,dim(fcanopy)[3],] = 0.0001
overstory[,,dim(overstory)[3]] = 0
rarc[,,dim(rarc)[3]] = 100
rmin[,,dim(rmin)[3]] = 0
rgl[,,dim(rgl)[3]] = 0
sol.atten[,,dim(sol.atten)[3]] = 0
wind.atten[,,dim(wind.atten)[3]] = 0
trunk.ratio[,,dim(trunk.ratio)[3]] = 0
wind.h[,,dim(wind.h)[3]] = 2

# Calculate
Nveg.fill = fillMap(Nveg, na.map, getNearestZero)
cv.fill = fillMap(cv, na.map, getNearestZero)
root.fract.fill = fillMap(root.fract, na.map, getNearestZero)
root.depth.fill = fillMap(root.depth, na.map, getNearestZero)
lai.fill = fillMap(lai, na.map, getNearestZero)
displacement.fill = fillMap(displacement, na.map, getNearestMean)
veg.rough.fill = fillMap(veg.rough, na.map, getNearestMean)
albedo.fill = fillMap(albedo, na.map, getNearestMean)
fcanopy.fill = fillMap(fcanopy, na.map, getNearestMean)
overstory.fill = fillMap(overstory, na.map, getNearestMean)
rarc.fill = fillMap(rarc, na.map, getNearestMean)
rmin.fill = fillMap(rmin, na.map, getNearestMean)
rgl.fill = fillMap(rgl, na.map, getNearestMean)
sol.atten.fill = fillMap(sol.atten, na.map, getNearestMean)
wind.atten.fill = fillMap(wind.atten, na.map, getNearestMean)
trunk.ratio.fill = fillMap(trunk.ratio, na.map, getNearestMean)
wind.h.fill = fillMap(wind.h, na.map, getNearestMean)

# Save
dir.create(dirname(vegetation.out))
system(command = paste0("ncks -h -x -v ", paste0(c(veg.vars, "veg_class"), collapse = ","), " ", vegetation.file, " -O ", vegetation.out))
addVegVars(nc.file = vegetation.out, nveg_class = veg_class)

nc = nc_open(vegetation.out, write = T)
ncvar_put(nc, "Nveg", Nveg.fill)
ncvar_put(nc, "Cv", cv.fill)
ncvar_put(nc, "root_fract", aperm(root.fract.fill, perm = c(1,2,4,3)))
ncvar_put(nc, "root_depth", aperm(root.depth.fill, perm = c(1,2,4,3)))
ncvar_put(nc, "LAI", aperm(lai.fill, perm = c(1,2,4,3)))
ncvar_put(nc, "displacement", aperm(displacement.fill, perm = c(1,2,4,3)))
ncvar_put(nc, "veg_rough", aperm(veg.rough.fill, perm = c(1,2,4,3)))
ncvar_put(nc, "albedo", aperm(albedo.fill, perm = c(1,2,4,3)))
ncvar_put(nc, "fcanopy", aperm(fcanopy.fill, perm = c(1,2,4,3)))
ncvar_put(nc, "overstory", overstory.fill)
ncvar_put(nc, "rarc", rarc.fill)
ncvar_put(nc, "rmin", rmin.fill)
ncvar_put(nc, "RGL", rgl.fill)
ncvar_put(nc, "rad_atten", sol.atten.fill)
ncvar_put(nc, "wind_atten", wind.atten.fill)
ncvar_put(nc, "trunk_ratio", trunk.ratio.fill)
ncvar_put(nc, "wind_h", wind.h.fill)
nc_close(nc)
