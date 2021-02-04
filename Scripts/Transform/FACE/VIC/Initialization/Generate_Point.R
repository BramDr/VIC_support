library(ncdf4)
library(plyr)
rm(list = ls())

# Input
generate.support.file <- "../../../../Scripts/Support/generateFunctions.R"
init.moist.file <- "./Saves/initial_observed.RDS"
vic.orig <- "../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out <- "./init_params_point.nc"

point <- c(33.0628, -111.9826) # lat-lon

# Load
source(generate.support.file)

init.moist = readRDS(init.moist.file)

nc <- nc_open(vic.orig)
depth <- ncvar_get(nc, "depth")
lats = nc$dim$lat$vals
lons = nc$dim$lon$vals
nc_close(nc)
y = which.min(abs(lats - point[1]))
x = which.min(abs(lons - point[2]))
depth = depth[x,y,]

# # Calculate
init.moist$init.moist = init.moist$moist * 1000 * depth[init.moist$layer]

treatment = init.moist$TRNO[1]
for(treatment in unique(init.moist$TRNO)){
  sel.1 = which(init.moist$TRNO == treatment & init.moist$layer == 1)
  sel.2 = which(init.moist$TRNO == treatment & init.moist$layer == 2)
  sel.3 = which(init.moist$TRNO == treatment & init.moist$layer == 3)
  init.moist.fill <- init.moist$init.moist[c(sel.1, sel.2, sel.3)]
  
  # Save
  vic.out.tmp = gsub(vic.out, pattern = "init_params_", replacement = paste0("init_params_", treatment, "_"))
  dir.create(dirname(vic.out.tmp))
  system(command = paste0("ncks -h",
                          " -d lon,", x, ",", x, 
                          " -d lat,", y, ",", y, 
                          " -v init_moist ", vic.orig, " -O ", vic.out.tmp))
  for (att in unused.atts) {
    system(command = paste0("ncatted -h -a ", att, ",global,d,, -O ", vic.out.tmp))
  }
  
  nc <- nc_open(vic.out.tmp, write = T)
  ncvar_put(nc, "lat", point[1])
  ncvar_put(nc, "lon", point[2])
  ncvar_put(nc, "init_moist", init.moist.fill)
  nc_close(nc)
}
