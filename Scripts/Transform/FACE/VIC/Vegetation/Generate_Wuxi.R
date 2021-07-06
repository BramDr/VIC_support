library(ncdf4)
rm(list = ls())

# Input
generate.support.file <- "../../../../../Scripts/Support/generateFunctions.R"
vic.orig <- "../../../../../Data/Primary/VIC/VIC_params_global.nc"
veg.out.tmp <- "./vegetation_params_point.tmp"
veg.out <- "../../../../../Data/VIC/Parameters/FACE/vegetation_params_Wuxi.nc"

point <- c(31.616667, 120.466667) # lat-lon

# Load
source(generate.support.file)

nc <- nc_open(vic.orig)
lats = nc$dim$lat$vals
lons = nc$dim$lon$vals
nc_close(nc)
y = which.min(abs(lats - point[1]))
x = which.min(abs(lons - point[2]))

# Save
dir.create(dirname(veg.out.tmp))
system(command = paste0("ncks -h",
                        " -d lon,", x, ",", x, 
                        " -d lat,", y, ",", y, 
                        " -v ", paste0(veg.vars, collapse = ","), " ", vic.orig, " -O ", veg.out.tmp))
for (att in unused.atts) {
  system(command = paste0("ncatted -h -a ", att, ",global,d,, -O ", veg.out.tmp))
}
dir.create(dirname(veg.out))
system(command = paste0("ncks -h -x -v ", paste0(c(veg.vars, "veg_class"), collapse = ","), " ", veg.out.tmp, " -O ", veg.out))
file.remove(veg.out.tmp)

addVegVars(nc.file = veg.out, nveg_class = 2)
addVegDefaultData(veg.out)
nc <- nc_open(veg.out, write = T)
ncvar_put(nc, "lon", point[2])
ncvar_put(nc, "lat", point[1])
ncvar_put(nc, "Cv", c(1,0))
nc_close(nc)
