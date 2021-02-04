rm(list = ls())

# Input
elev.file <- "./Elevation/elevation_params_point.nc"
calib.file <- "./Calibration/calibration_params_point.nc"
misc.file <- "./Miscellaneous/miscellaneous_params_point.nc"
veg.file <- "./Vegetation/vegetation_params_point.nc"
co2.file <- "./CO2/co2_params_point.nc"
soil.file <- "./Soil/soil_params_observed.nc"
init.file <- "./Initialization/init_params_observed.nc"
out.file <- "../../../../Data/VIC/Parameters/FACE/VIC_params_observed.nc"

# Save
for(treatment in 901:916) {
  out.file.tmp = gsub(out.file, pattern = "VIC_params_", replacement = paste0("VIC_params_", treatment, "_"))
  init.file.tmp = gsub(init.file, pattern = "init_params_", replacement = paste0("init_params_", treatment, "_"))
  
  file.remove(out.file.tmp)
  system(command = paste0("ncks -h -A ", elev.file, " ", out.file.tmp))
  system(command = paste0("ncks -h -A ", calib.file, " ", out.file.tmp))
  system(command = paste0("ncks -h -A ", misc.file, " ", out.file.tmp))
  system(command = paste0("ncks -h -A ", veg.file, " ", out.file.tmp))
  system(command = paste0("ncks -h -A ", co2.file, " ", out.file.tmp))
  system(command = paste0("ncks -h -A ", soil.file, " ", out.file.tmp))
  system(command = paste0("ncks -h -A ", init.file.tmp, " ", out.file.tmp))
}
