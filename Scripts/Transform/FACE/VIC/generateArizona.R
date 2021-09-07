rm(list = ls())

# Input
elev.file <- "../../../../Data/VIC/Parameters/FACE/Arizona/elevation_params_Arizona.nc"
calib.file <- "../../../../Data/VIC/Parameters/FACE/Arizona/calibration_params_Arizona.nc"
misc.file <- "../../../../Data/VIC/Parameters/FACE/Arizona/miscellaneous_params_Arizona.nc"
veg.file <- "../../../../Data/VIC/Parameters/FACE/Arizona/vegetation_params_Arizona.nc"
co2.file <- "../../../../Data/VIC/Parameters/FACE/Arizona/co2_params_Arizona.nc"
soil.file <- "../../../../Data/VIC/Parameters/FACE/Arizona/soil_params_Arizona.nc"
init.file <- "../../../../Data/VIC/Parameters/FACE/Arizona/init_params_Arizona.nc"
out.file <- "../../../../Data/VIC/Parameters/FACE/Arizona/VIC_params_Arizona.nc"

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
