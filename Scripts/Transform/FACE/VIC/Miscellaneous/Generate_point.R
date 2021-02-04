library(ncdf4)
rm(list = ls())

# Input
generate.support.file <- "../../../../Scripts/Support/generateFunctions.R"
vic.orig <- "../../../../Data/VIC/Parameters/global/miscellaneous_params_Vliet_global.nc"
vic.out <- "./miscellaneous_params_point.nc"

point <- c(33.0628, -111.9826) # lat-lon

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
