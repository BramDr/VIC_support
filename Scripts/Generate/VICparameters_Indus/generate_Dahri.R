rm(list = ls())

# Input
elev.file <- "/home/bram/Data/VIC/Parameters/Indus_5min/elevation_params_Dahri_Indus.nc"
calib.file <- "/home/bram/Data/VIC/Parameters/Indus_5min/calibration_params_Dahri_Indus.nc"
misc.file <- "/home/bram/Data/VIC/Parameters/Indus_5min/miscellaneous_params_Dahri_Indus.nc"
veg.file <- "/home/bram/Data/VIC/Parameters/Indus_5min/vegetation_params_Dahri_Indus.nc"
soil.file <- "/home/bram/Data/VIC/Parameters/Indus_5min/soil_params_Dahri_Indus.nc"
out.file <- "/home/bram/Data/VIC/Parameters/Indus_5min/VIC_params_Dahri_Indus.nc"

# Save
file.remove(out.file)
system(command = paste0("ncks -h -A ", elev.file, " ", out.file))
system(command = paste0("ncks -h -A ", calib.file, " ", out.file))
system(command = paste0("ncks -h -A ", misc.file, " ", out.file))
system(command = paste0("ncks -h -A ", veg.file, " ", out.file))
system(command = paste0("ncks -h -A ", soil.file, " ", out.file))
