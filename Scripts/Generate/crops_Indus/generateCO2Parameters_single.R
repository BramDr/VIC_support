library(fields)
library(ncdf4)
rm(list = ls())

# Input
map.script <- "../../Support/mapFunctions.R"
generate.script <- "../../Support/generateFunctions.R"
mask.file <- "../../../Data/VIC/Parameters/Indus_5min/domain_Indus.nc"
co2.file <- "../../../Data/VIC/Parameters/Indus_5min/co2_params_Mirca_Indus.nc"
co2.out <- "../../../Data/VIC/Parameters/Indus_5min/single/co2_params_single_Indus.nc"

# Load
source(map.script)
source(generate.script)

nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

# Setup
veg_class <- 2

na.map <- is.na(mask) | mask == 0

b.co2 <- array(0, dim = c(dim(mask), veg_class))

b.co2[, , dim(b.co2)[3] - 1] <- 0.606965

# Calculate
b.co2.fill <- fillMap(b.co2, na.map, getNearestZero)

# Save
dir.create(dirname(co2.out))
system(command = paste0("ncks -h -x -v ", paste0(c("b_co2", "veg_class"), collapse = ","), " ", co2.file, " -O ", co2.out))
addCo2Vars(nc.file = co2.out, nveg_class = veg_class)

nc <- nc_open(co2.out, write = T)
ncvar_put(nc, "b_co2", b.co2.fill)
nc_close(nc)
