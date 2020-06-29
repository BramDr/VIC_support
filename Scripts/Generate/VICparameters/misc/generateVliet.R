library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
map.support.file = "../../../../Scripts/Support/mapFunctions.R"
generate.support.file = "../../../../Scripts/Support/generateFunctions.R"
vic.orig = "../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out = "../../../../Data/VIC/Parameters/global/miscellaneous_params_Vliet_global.nc"

# Load
source(map.support.file)
source(generate.support.file)

# Save
dir.create(dirname(vic.out))
system(command = paste0("ncks -h -v ", paste0(misc.vars, collapse = ","), " ", vic.orig, " -O ", vic.out))
for(att in unused.atts){
  system(command = paste0("ncatted -h -a ",att, ",global,d,, -O ", vic.out))
}
