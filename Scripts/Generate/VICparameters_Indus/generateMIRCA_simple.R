rm(list = ls())

# Input
elev.file <- "../../../Data/VIC/Parameters/Indus_5min/elevation_params_SRTM_Indus.nc"
calib.file <- "../../../Data/VIC/Parameters/Indus_5min/calibration_params_Nijssen_Indus.nc"
misc.file <- "../../../Data/VIC/Parameters/Indus_5min/miscellaneous_params_Vliet_Indus.nc"
veg.file <- "../../../Data/VIC/Parameters/Indus_5min/vegetation_params_Mirca_simple_Indus.nc"
co2.file <- "../../../Data/VIC/Parameters/Indus_5min/co2_params_Mirca_simple_Indus.nc"
soil.file <- "../../../Data/VIC/Parameters/Indus_5min/soil_params_Saxton_Indus.nc"
out.file <- "../../../Data/VIC/Parameters/Indus_5min/VIC_params_Mirca_simple_Indus.nc"

# Save
file.remove(out.file)
system(command = paste0("ncks -h -A ", elev.file, " ", out.file))
system(command = paste0("ncks -h -A ", calib.file, " ", out.file))
system(command = paste0("ncks -h -A ", misc.file, " ", out.file))
system(command = paste0("ncks -h -A ", veg.file, " ", out.file))
system(command = paste0("ncks -h -A ", co2.file, " ", out.file))
system(command = paste0("ncks -h -A ", soil.file, " ", out.file))
