library(fields)
library(ncdf4)
rm(list = ls())

# Input
fluxes.file <- "../../../Data/Primary/VIC/Output/fluxes_global_MIRCA_nat.1979-01-01.nc"
average.out <- "../../../Data/Transformed/VIC/fluxes_global_MIRCA_nat.ydaymean.nc"

# Calculate & Save
system(command = paste0("cdo ydaymean ", fluxes.file, " ", average.out))
