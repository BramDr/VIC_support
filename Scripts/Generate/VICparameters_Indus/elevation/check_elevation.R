library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
vic.out <- "../../../../Data/VIC/Parameters/Indus_5min/elevation_params_SRTM_Indus.nc"

nc = nc_open(vic.out)
area = ncvar_get(nc, nc$var$AreaFract)
nc_close(nc)

area.sum = apply(X = area, MARGIN = c(1,2), FUN = sum)
image.plot(area.sum)
