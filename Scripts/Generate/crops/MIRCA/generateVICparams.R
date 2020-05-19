library(fields)
library(ncdf4)
rm(list = ls())

# Input
crop.file = "./Saves/crop_mapping_MIRCA.csv"
function.script <- "../../Support/generateFunctions.R"
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
Ncrop.file = "./Saves/Ncrop_MIRCA_30min_global.RDS"
Nbare.file = "./Saves/Nbare_MIRCA_30min_global.RDS"
vegetation.file <- "../../../Data/Transformed/VIC/Parameters/VIC_params_VlietAlt30min_global.nc"
vegetation.out <- "../../../Data/VIC/Parameters/global/VIC_params_MIRCA_global.nc"

# Load
source(function.script)

crops = read.csv(crop.file, stringsAsFactors = F)

Ncrop.crop = readRDS(Ncrop.file)
Nbare.crop = readRDS(Nbare.file)

nc = nc_open(mask.file)
na.map = ncvar_get(nc, nc$var$mask)
nc_close(nc)
na.map = is.na(na.map) | na.map != 1
image.plot(na.map)

nc = nc_open(vegetation.file)
Cv = ncvar_get(nc, nc$var$Cv)
nc_close(nc)

# Setup
Cv[is.na(Cv)] = 0
Nveg = Cv[,,c(1:10)] > 0
Nbare.veg = Cv[,,c(12)] > 0
image.plot(Nveg[,,1])
image.plot(Nbare.veg)

vic.id.u = unique(crops$vic.id)
Ncrop = array(0, dim = c(dim(Ncrop.crop)[1], dim(Ncrop.crop)[2], length(vic.id.u) + 1))
for(i in 1:nrow(crops)) {
  vic.id = crops$vic.id[i]
  vic.idx = which(vic.id.u == vic.id)
  
  Ncrop[,,vic.idx] = Ncrop[,,vic.idx] | Ncrop.crop[,,i]
}
Ncrop[,,dim(Ncrop)[3]] = Ncrop.crop[,,dim(Ncrop.crop)[3]]
image.plot(Ncrop[,,1])
image.plot(Ncrop[,,dim(Ncrop)[3]])

Nbare = Nbare.veg | Nbare.crop

Ncrop.tot = apply(X = Ncrop, MARGIN = c(1,2), FUN = sum)
Nveg.tot = apply(X = Nveg, MARGIN = c(1,2), FUN = sum)
Nbare.tot = apply(X = Nbare, MARGIN = c(1,2), FUN = sum)
image.plot(Ncrop.tot)
image.plot(Nveg.tot)
image.plot(Nbare.tot)

# Calculate
## Create vegetation fractions
Cv.new = array(NA, dim = c(dim(Cv)[1], dim(Cv)[2], dim(Cv)[3] + length(unique(crops$vic.id))))
for(x in 1:dim(Cv.new)[1]) {
  for(y in 1:dim(Cv.new)[2]) {
    nr = Ncrop.tot[x,y] + Nveg.tot[x,y] + Nbare.tot[x,y]
    if(nr == 0) {
      Cv.new[x,y,] = 0
      next
    }
    frac = 1 / nr
    
    Cvs = c(Nveg[x,y,] * frac,
            Ncrop[x,y,] * frac,
            Nbare[x,y] * frac)
    Cv.new[x,y,] = Cvs
  }
}
Cv.new.sum = apply(X = Cv.new, MARGIN = c(1,2), FUN = sum)
image.plot(Cv.new[,,1])
image.plot(Cv.new.sum)

## Get origional vegetation data
nc = nc_open(vegetation.file)
wind_atten = apply(X = ncvar_get(nc, "wind_atten"), MARGIN = 3, FUN = max, na.rm = T)
rad_atten = apply(X = ncvar_get(nc, "rad_atten"), MARGIN = 3, FUN = max, na.rm = T)
rmin = apply(X = ncvar_get(nc, "rmin"), MARGIN = 3, FUN = max, na.rm = T)
rarc = apply(X = ncvar_get(nc, "rarc"), MARGIN = 3, FUN = max, na.rm = T)
RGL = apply(X = ncvar_get(nc, "RGL"), MARGIN = 3, FUN = max, na.rm = T)
overstory = apply(X = ncvar_get(nc, "overstory"), MARGIN = 3, FUN = max, na.rm = T)
trunk_ratio = apply(X = ncvar_get(nc, "trunk_ratio"), MARGIN = 3, FUN = max, na.rm = T)
wind_h = apply(X = ncvar_get(nc, "wind_h"), MARGIN = 3, FUN = max, na.rm = T)

root_fract = t(apply(X = ncvar_get(nc, "root_fract"), MARGIN = c(3,4), FUN = max, na.rm = T))
root_depth = t(apply(X = ncvar_get(nc, "root_depth"), MARGIN = c(3,4), FUN = max, na.rm = T))

LAI = t(apply(X = ncvar_get(nc, "LAI"), MARGIN = c(3,4), FUN = max, na.rm = T))
albedo = t(apply(X = ncvar_get(nc, "albedo"), MARGIN = c(3,4), FUN = max, na.rm = T))
displacement = t(apply(X = ncvar_get(nc, "displacement"), MARGIN = c(3,4), FUN = max, na.rm = T))
veg_rough = t(apply(X = ncvar_get(nc, "veg_rough"), MARGIN = c(3,4), FUN = max, na.rm = T))
nc_close(nc)

height = displacement / 0.67
height = veg_rough / 0.123

## Adjust vegetation data
wind_atten = c(wind_atten[1:10], rep(0.5, dim(Ncrop)[3]), wind_atten[12])
rad_atten = c(rad_atten[1:10], rep(0.5, dim(Ncrop)[3]), rad_atten[12])
rmin = c(rmin[1:10], rep(100, dim(Ncrop)[3]), rmin[12])
rarc = c(rarc[1:10], rep(2, dim(Ncrop)[3]), rarc[12])
RGL = c(RGL[1:10], rep(100, dim(Ncrop)[3]), RGL[12])
overstory = c(overstory[1:10], rep(0, dim(Ncrop)[3]), overstory[12])
trunk_ratio = c(trunk_ratio[1:10], rep(0.2, dim(Ncrop)[3]), trunk_ratio[12])
wind_h = c(wind_h[1:10], rep(2, dim(Ncrop)[3]), wind_h[12])

root_fract.c = cbind(cbind(rep(0.5, dim(Ncrop)[3]), rep(0.5, dim(Ncrop)[3])), rep(0, dim(Ncrop)[3]))
root_fract = rbind(rbind(root_fract[1:10,], root_fract.c), root_fract[12,])
root_depth.c = cbind(cbind(rep(0.3, dim(Ncrop)[3]), rep(0.7, dim(Ncrop)[3])), rep(0, dim(Ncrop)[3]))
root_depth = rbind(rbind(root_depth[1:10,], root_depth.c), root_depth[12,])

LAI.c = matrix(2, nrow = dim(Ncrop)[3], ncol = 12)
LAI = rbind(rbind(LAI[1:10,], LAI.c), LAI[12,])
albedo.c = matrix(0.1, nrow = dim(Ncrop)[3], ncol = 12)
albedo = rbind(rbind(albedo[1:10,], albedo.c), albedo[12,])
height.c = matrix(2, nrow = dim(Ncrop)[3], ncol = 12)
height = rbind(rbind(height[1:10,], height.c), height[12,])

# Save
removeVegVars(nc.old.file = vegetation.file, nc.new.file = vegetation.out)
addVegVars(nc.file = vegetation.out, nveg_class = dim(Cv.new)[3])
addVegDefaultData(nc.file = vegetation.out, 
                  Cv = Cv.new,  
                  wind_atten = wind_atten,
                  rad_atten = rad_atten,
                  rmin = rmin,
                  rarc = rarc,
                  RGL = RGL,
                  overstory = overstory,
                  trunk_ratio = trunk_ratio,
                  wind_h = wind_h,
                  root_fract = root_fract,
                  root_depth = root_depth,
                  LAI = LAI,
                  albedo = albedo,
                  height = height,
                  na.map = na.map)
                  
## Add description
nc = nc_open(vegetation.out, write = T)
veg.dim = nc$dim$veg_class
nchar.dim = ncdim_def(
  name = "nchar",
  units = "N/A",
  vals = 1:254,
  longname = "Maximum number of string characters"
)
desc.var = ncvar_def(
  name = "veg_desc",
  units = "N/A",
  dim = list(nchar.dim, veg.dim),
  longname = "vegetation description",
  prec = "char"
)

nc = ncvar_add(nc, desc.var)
nc_close(nc)

nc = nc_open(vegetation.out, write = T)

description = rep("", veg.dim$len)
vic.id.u = unique(crops$vic.id)
for(i in 1:length(vic.id.u)){
  crops.v = crops[crops$vic.id == vic.id.u[i],]
  description[vic.id.u[i]] = paste0(crops.v$mirca.name[1], "_", crops.v$water[1])
}

ncvar_put(nc, nc$var$veg_desc, description)
nc_close(nc)

