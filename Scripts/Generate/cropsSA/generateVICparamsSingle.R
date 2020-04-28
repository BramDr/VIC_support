library(fields)
library(ncdf4)
rm(list = ls())

# Input
function.script <- "../../Support/generateFunctions.R"
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
vegetation.file <- "../../../Data/Transformed/VIC/Parameters/Soil/VIC_params_Saxton_global.nc"
vegetation.out <- "../../../Data/VIC/Parameters/global/WOFOST_SA/VIC_params_single_global.nc"

# Load
source(function.script)

nc = nc_open(mask.file)
na.map = ncvar_get(nc, nc$var$mask)
nc_close(nc)
na.map = is.na(na.map) | na.map != 1
image.plot(na.map)

# Save
Cc = array(0, dim = c(dim(na.map), 2))
Cc[,,1] = 1

removeVegVars(nc.old.file = vegetation.file, nc.new.file = vegetation.out)
addVegVars(nc.file = vegetation.out, nveg_class = 2)
addVegDefaultData(nc.file = vegetation.out, Cv = Cc,  na.map = na.map)
