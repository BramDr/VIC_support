library(ncdf4)
library(fields)
rm(list = ls())

# Input
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
mask.out <- "../../../Data/Transformed/Routing/mask_30min_global.RDS"

# Load
nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)
image.plot(mask)

# Save
dir.create(dirname(mask.out))
saveRDS(mask, mask.out)
