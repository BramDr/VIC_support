rm(list = ls())

# Input
elev.file <- "../../../../Data/VIC/Parameters/FACE/Shizukuishi/elevation_params_Shizukuishi.nc"
calib.file <- "../../../../Data/VIC/Parameters/FACE/Shizukuishi/calibration_params_Shizukuishi.nc"
misc.file <- "../../../../Data/VIC/Parameters/FACE/Shizukuishi/miscellaneous_params_Shizukuishi.nc"
veg.file <- "../../../../Data/VIC/Parameters/FACE/Shizukuishi/vegetation_params_Shizukuishi.nc"
co2.file <- "../../../../Data/VIC/Parameters/FACE/Shizukuishi/co2_params_Shizukuishi.nc"
soil.file <- "../../../../Data/VIC/Parameters/FACE/Shizukuishi/soil_params_Shizukuishi.nc"
init.file <- "../../../../Data/VIC/Parameters/FACE/Shizukuishi/init_params_Shizukuishi.nc"
out.file <- "../../../../Data/VIC/Parameters/FACE/Shizukuishi/VIC_params_Shizukuishi.nc"

# Save
file.remove(out.file)
system(command = paste0("ncks -h -A ", elev.file, " ", out.file))
system(command = paste0("ncks -h -A ", calib.file, " ", out.file))
system(command = paste0("ncks -h -A ", misc.file, " ", out.file))
system(command = paste0("ncks -h -A ", veg.file, " ", out.file))
system(command = paste0("ncks -h -A ", co2.file, " ", out.file))
system(command = paste0("ncks -h -A ", soil.file, " ", out.file))
system(command = paste0("ncks -h -A ", init.file, " ", out.file))
