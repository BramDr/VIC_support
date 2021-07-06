library(ncdf4)
rm(list = ls())

# Input
generate.support.file <- "../../../Support/generateFunctions.R"
vic.orig <- "../../../../Data/VIC/Parameters/global/calibration_params_Saxton_global.nc"
vic.out <- "../../../../Data/VIC/Parameters/FACE/Shizukuishi/calibration_params_Shizukuishi.nc"

point <- c(39.633333, 140.950000) # lat-lon

# Load
source(generate.support.file)

# Setup
nc <- nc_open(vic.orig)
lats = nc$dim$lat$vals
lons = nc$dim$lon$vals
nc_close(nc)
y = which.min(abs(lats - point[1]))
x = which.min(abs(lons - point[2]))

# Save
dir.create(dirname(vic.out))
system(command = paste0("ncks -h",
                        " -d lon,", x, ",", x, 
                        " -d lat,", y, ",", y, 
                        " ", vic.orig, " -O ", vic.out))
for (att in unused.atts) {
  system(command = paste0("ncatted -h -a ", att, ",global,d,, -O ", vic.out))
}

nc <- nc_open(vic.out, write = T)
ncvar_put(nc, "lon", point[2])
ncvar_put(nc, "lat", point[1])
nc_close(nc)
