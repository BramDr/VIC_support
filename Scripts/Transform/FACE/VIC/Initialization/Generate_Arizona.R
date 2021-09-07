library(ncdf4)
library(plyr)
rm(list = ls())

# Input
generate.support.file <- "../../../../../Scripts/Support/generateFunctions.R"
init.moist.file <- "./Saves/initial_Arizona.RDS"
vic.orig <- "../../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out <- "../../../../../Data/VIC/Parameters/FACE/Arizona/init_params_Arizona.nc"

point <- c(33.0628, -111.9826) # lat-lon

# Load
source(generate.support.file)

moist = readRDS(init.moist.file)

# Calculate
treatment = moist$TRNO[1]
for(treatment in unique(moist$TRNO)){
  sel.1 = which(moist$TRNO == treatment & moist$layer == 1)
  sel.2 = which(moist$TRNO == treatment & moist$layer == 2)
  sel.3 = which(moist$TRNO == treatment & moist$layer == 3)
  init.moist <- moist$moist[c(sel.1, sel.2, sel.3)]
  
  # Save
  vic.out.tmp = gsub(vic.out, pattern = "init_params_", replacement = paste0("init_params_", treatment, "_"))
  dir.create(dirname(vic.out.tmp))
  system(command = paste0("ncks -h",
                          " -d lon,", 1, ",", 1, 
                          " -d lat,", 1, ",", 1, 
                          " -v init_moist ", vic.orig, " -O ", vic.out.tmp))
  for (att in unused.atts) {
    system(command = paste0("ncatted -h -a ", att, ",global,d,, -O ", vic.out.tmp))
  }
  
  nc <- nc_open(vic.out.tmp, write = T)
  ncvar_put(nc, "lat", point[1])
  ncvar_put(nc, "lon", point[2])
  ncvar_put(nc, "init_moist", init.moist)
  nc_close(nc)
}
