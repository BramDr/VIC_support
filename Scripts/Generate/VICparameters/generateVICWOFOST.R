rm(list = ls())

# Input
elev.file <- "/home/bram/Data/VIC/Parameters/global/elevation_params_Vliet_global.nc"
calib.file <- "/home/bram/Data/VIC/Parameters/global/calibration_params_Saxton_global.nc"
misc.file <- "/home/bram/Data/VIC/Parameters/global/miscellaneous_params_Vliet_global.nc"
veg.file <- "/home/bram/Data/VIC/Parameters/global/vegetation_params_MIRCAhybridUniform_global.nc"
soil.file <- "/home/bram/Data/VIC/Parameters/global/soil_params_Saxton_global.nc"
out.file <- "/home/bram/Data/VIC/Parameters/global/VIC_params_VICWOFOST_global.nc"

# Save
file.remove(out.file)
system(command = paste0("ncks -h -A ", elev.file, " ", out.file))
system(command = paste0("ncks -h -A ", calib.file, " ", out.file))
system(command = paste0("ncks -h -A ", misc.file, " ", out.file))
system(command = paste0("ncks -h -A ", veg.file, " ", out.file))
system(command = paste0("ncks -h -A ", soil.file, " ", out.file))
