library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
map.support.file = "../../../../Scripts/Support/mapFunctions.R"
generate.support.file = "../../../../Scripts/Support/generateFunctions.R"

cv.file = "../../../../Data/Transformed/Parameters/cv_Vliet30min_30min_global.RDS"

root.fract.file = "../../../../Data/Transformed/Parameters/rootFract_Vliet30min_30min_global.RDS"
root.depth.file = "../../../../Data/Transformed/Parameters/rootDepth_Vliet30min_30min_global.RDS"

lai.file = "../../../../Data/Transformed/Parameters/LAI_Vliet30min_30min_global.RDS"
displacement.file = "../../../../Data/Transformed/Parameters/displacement_Vliet30min_30min_global.RDS"
veg.rough.file = "../../../../Data/Transformed/Parameters/vegRough_Vliet30min_30min_global.RDS"
albedo.file = "../../../../Data/Transformed/Parameters/albedo_Vliet30min_30min_global.RDS"

overstory.file = "../../../../Data/Transformed/Parameters/vegetationOverstory_Vliet30min_30min_global.RDS"
rarc.file = "../../../../Data/Transformed/Parameters/vegetationRarc_NijssenAlt120min_30min_global.RDS"
rmin.file = "../../../../Data/Transformed/Parameters/vegetationRmin_NijssenAlt120min_30min_global.RDS"
rgl.file = "../../../../Data/Transformed/Parameters/vegetationRgl_Vliet30min_30min_global.RDS"
sol.atten.file = "../../../../Data/Transformed/Parameters/vegetationSolarAttenuation_Vliet30min_30min_global.RDS"
wind.atten.file = "../../../../Data/Transformed/Parameters/vegetationWindAttenuation_Vliet30min_30min_global.RDS"
trunk.ratio.file = "../../../../Data/Transformed/Parameters/vegetationTrunkRatio_Vliet30min_30min_global.RDS"
wind.h.file = "../../../../Data/Transformed/Parameters/vegetationWindh_Vliet30min_30min_global.RDS"

mask.file = "../../../../Data/Primary/VIC/domain_global.nc"
vic.orig = "../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out = "../../../../Data/VIC/Parameters/global/vegetation_params_VlietAlt_global.nc"

# Load
source(map.support.file)
source(generate.support.file)

cv = readRDS(cv.file)
root.fract = readRDS(root.fract.file)
root.depth = readRDS(root.depth.file)
lai = readRDS(lai.file)
displacement = readRDS(displacement.file)
veg.rough = readRDS(veg.rough.file)
albedo = readRDS(albedo.file)
overstory = readRDS(overstory.file)
rarc = readRDS(rarc.file)
rmin = readRDS(rmin.file)
rgl = readRDS(rgl.file)
sol.atten = readRDS(sol.atten.file)
wind.atten = readRDS(wind.atten.file)
trunk.ratio = readRDS(trunk.ratio.file)
wind.h = readRDS(wind.h.file)

nc = nc_open(mask.file)
mask = ncvar_get(nc, "mask")
nc_close(nc)

# Setup
na.map = is.na(mask) | mask == 0

root.fract[,,,3] = 0
root.depth[,,,3] = 0
lai[,,dim(lai)[3],] = 0
displacement[,,dim(displacement)[3],] = 0
veg.rough[,,dim(veg.rough)[3],] = 0
albedo[,,dim(albedo)[3],] = 0.2

overstory[,,dim(overstory)[3]] = 0
rarc[,,dim(rarc)[3]] = 100
rmin[,,dim(rmin)[3]] = 0
rgl[,,dim(rgl)[3]] = 0
sol.atten[,,dim(sol.atten)[3]] = 0
wind.atten[,,dim(wind.atten)[3]] = 0
trunk.ratio[,,dim(trunk.ratio)[3]] = 0
wind.h[,,dim(wind.h)[3]] = 2

# Calculate
Nveg = apply(X = cv, MARGIN = c(1,2), FUN = function(x){sum(na.omit(x[1:(length(x) - 1)]) > 0)})

Nveg.fill = fillMap(Nveg, na.map, getNearestZero)
cv.fill = fillMap(cv, na.map, getNearestZero)
root.fract.fill = fillMap(root.fract, na.map, getNearestZero)
root.depth.fill = fillMap(root.depth, na.map, getNearestZero)
lai.fill = fillMap(lai, na.map, getNearestZero)
displacement.fill = fillMap(displacement, na.map, getNearestMean)
veg.rough.fill = fillMap(veg.rough, na.map, getNearestMean)
albedo.fill = fillMap(albedo, na.map, getNearestMean)
overstory.fill = fillMap(overstory, na.map, getNearestMean)
rarc.fill = fillMap(rarc, na.map, getNearestMean)
rmin.fill = fillMap(rmin, na.map, getNearestMean)
rgl.fill = fillMap(rgl, na.map, getNearestMean)
sol.atten.fill = fillMap(sol.atten, na.map, getNearestMean)
wind.atten.fill = fillMap(wind.atten, na.map, getNearestMean)
trunk.ratio.fill = fillMap(trunk.ratio, na.map, getNearestMean)
wind.h.fill = fillMap(wind.h, na.map, getNearestMean)

# Save
dir.create(dirname(vic.out))
system(command = paste0("ncks -h -v ", paste0(veg.vars, collapse = ","), " ", vic.orig, " -O ", vic.out))
for(att in unused.atts){
  system(command = paste0("ncatted -h -a ",att, ",global,d,, -O ", vic.out))
}

nc = nc_open(vic.out, write = T)
ncvar_put(nc, "Nveg", Nveg.fill)
ncvar_put(nc, "Cv", cv.fill)
ncvar_put(nc, "root_fract", aperm(root.fract.fill, perm = c(1,2,4,3)))
ncvar_put(nc, "root_depth", aperm(root.depth.fill, perm = c(1,2,4,3)))
ncvar_put(nc, "LAI", aperm(lai.fill, perm = c(1,2,4,3)))
ncvar_put(nc, "displacement", aperm(displacement.fill, perm = c(1,2,4,3)))
ncvar_put(nc, "veg_rough", aperm(veg.rough.fill, perm = c(1,2,4,3)))
ncvar_put(nc, "albedo", aperm(albedo.fill, perm = c(1,2,4,3)))
ncvar_put(nc, "overstory", overstory.fill)
ncvar_put(nc, "rarc", rarc.fill)
ncvar_put(nc, "rmin", rmin.fill)
ncvar_put(nc, "RGL", rgl.fill)
ncvar_put(nc, "rad_atten", sol.atten.fill)
ncvar_put(nc, "wind_atten", wind.atten.fill)
ncvar_put(nc, "trunk_ratio", trunk.ratio.fill)
ncvar_put(nc, "wind_h", wind.h.fill)
nc_close(nc)
