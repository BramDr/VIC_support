rm(list = ls())

# Input
elev.file <- "../../../../Data/VIC/Parameters/FACE/Wuxi/elevation_params_Wuxi.nc"
calib.file <- "../../../../Data/VIC/Parameters/FACE/Wuxi/calibration_params_Wuxi.nc"
misc.file <- "../../../../Data/VIC/Parameters/FACE/Wuxi/miscellaneous_params_Wuxi.nc"
veg.file <- "../../../../Data/VIC/Parameters/FACE/Wuxi/vegetation_params_Wuxi.nc"
co2.file <- "../../../../Data/VIC/Parameters/FACE/Wuxi/co2_params_Wuxi.nc"
soil.file <- "../../../../Data/VIC/Parameters/FACE/Wuxi/soil_params_Wuxi.nc"
init.file <- "../../../../Data/VIC/Parameters/FACE/Wuxi/init_params_Wuxi.nc"
out.file <- "../../../../Data/VIC/Parameters/FACE/Wuxi/VIC_params_Wuxi.nc"

# Save
file.remove(out.file)
system(command = paste0("ncks -h -A ", elev.file, " ", out.file))
system(command = paste0("ncks -h -A ", calib.file, " ", out.file))
system(command = paste0("ncks -h -A ", misc.file, " ", out.file))
system(command = paste0("ncks -h -A ", veg.file, " ", out.file))
system(command = paste0("ncks -h -A ", co2.file, " ", out.file))
system(command = paste0("ncks -h -A ", soil.file, " ", out.file))
system(command = paste0("ncks -h -A ", init.file, " ", out.file))
