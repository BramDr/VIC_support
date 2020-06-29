library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
map.support.file = "../../../../Scripts/Support/mapFunctions.R"
generate.support.file = "../../../../Scripts/Support/generateFunctions.R"
vic.orig = "../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out = "../../../../Data/VIC/Parameters/global/elevation_params_Vliet_global.nc"

# Load
source(map.support.file)
source(generate.support.file)

nc = nc_open(vic.orig)
elevation = ncvar_get(nc, "elevation")
area.fract = ncvar_get(nc, "AreaFract")
nc_close(nc)

# Setup
Nelev.var = ncvar_def(name = "Nelev", units = "#", dim = list(nc$dim$lon, nc$dim$lat), longname = "Number of active elevation bands", prec = "integer", missval = -1)

# Calculate
Nelev = apply(X = area.fract, MARGIN = c(1,2), FUN = function(x){sum(x > 0)})
elev = apply(X = elevation * area.fract, MARGIN = c(1,2), FUN = sum)

Nelev.fill = Nelev
elev.fill = elev

# Save
dir.create(dirname(vic.out))
system(command = paste0("ncks -h -v ", paste0(elev.vars, collapse = ","), " ", vic.orig, " -O ", vic.out))
for(att in unused.atts){
  system(command = paste0("ncatted -h -a ",att, ",global,d,, -O ", vic.out))
}

nc = nc_open(vic.out, write = T)
nc = ncvar_add(nc, Nelev.var)
ncvar_put(nc, "Nelev", Nelev.fill)
ncvar_put(nc, "elev", elev.fill)
nc_close(nc)

