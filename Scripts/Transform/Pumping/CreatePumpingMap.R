library(ncdf4)
library(fields)
rm(list = ls())

# Input
pumping.file <- "../../../Data/Primary/Satanudjaja2018/regional_abstraction_limit.nc"
pumping.out <- "../../../Data/Transformed/Pumping/pumpingCapacity_30min_global.RDS"

# Load
nc <- nc_open(pumping.file)
pumping <- ncvar_get(nc, "regional_pumping_limit")
nc_close(nc)

# Setup
years <- 1960:2015

# Calcualte
pumping <- pumping[, dim(pumping)[2]:1, ]
pumping <- pumping * 10 / 365 # mm day-1
image.plot(pumping[, , 1])

# Save
dir.create(dirname(pumping.out))
saveRDS(object = pumping, file = pumping.out)
