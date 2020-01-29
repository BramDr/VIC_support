library(ncdf4)
library(fields)
rm(list = ls())

# Input
area.file <- "../../../Data/Primary/VIC/domain_global.nc"
area.out <- "../../../Data/Transformed/Routing/area_30min_global.RDS"

# Load
nc <- nc_open(area.file)
area <- ncvar_get(nc, "area")
nc_close(nc)
image.plot(area)

# Save
dir.create(dirname(area.out))
saveRDS(area, area.out)
