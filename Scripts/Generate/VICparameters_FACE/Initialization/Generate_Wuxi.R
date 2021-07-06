library(ncdf4)
library(plyr)
rm(list = ls())

# Input
generate.support.file <- "../../../Support/generateFunctions.R"
vic.orig <- "../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out <- "../../../../Data/VIC/Parameters/FACE/Wuxi/init_params_Wuxi.nc"

point <- c(31.616667, 120.466667) # lat-lon

# Load
source(generate.support.file)

# Calculate
init.moist = c(0.3, 1.0, 0.3) * 0.2 * 1000

# Save
dir.create(dirname(vic.out))
system(command = paste0("ncks -h",
                        " -d lon,", 1, ",", 1, 
                        " -d lat,", 1, ",", 1, 
                        " -v init_moist ", vic.orig, " -O ", vic.out))
for (att in unused.atts) {
  system(command = paste0("ncatted -h -a ", att, ",global,d,, -O ", vic.out))
}

nc <- nc_open(vic.out, write = T)
ncvar_put(nc, "lat", point[1])
ncvar_put(nc, "lon", point[2])
ncvar_put(nc, "init_moist", init.moist)
nc_close(nc)
