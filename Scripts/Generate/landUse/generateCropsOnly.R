library(fields)
library(ncdf4)
rm(list = ls())

# Input
function.script <- "../../Support/generateFunctions.R"
vegetation.file <- "../../../Data/Primary/VIC/VIC_params_global.nc"
vegetation.out <- "../../../Data/VIC/Parameters/global/SA/VIC_params_WOFOST_SA_global.nc"

# Load
source(function.script)

# Setup
nc = nc_open(vegetation.file)
na.map = ncvar_get(nc, nc$var$run_cell)
nc_close(nc)
na.map = is.na(na.map)
image.plot(na.map)

# Save
removeVegVars(nc.old.file = vegetation.file, nc.new.file = vegetation.out)
addVegVars(nc.file = vegetation.out, nveg_class = 18)
addVegDefaultData(nc.file = vegetation.out, na.map = na.map)
