library(fields)
library(ncdf4)
library(Rcpp)
rm(list = ls())

# Input
elev.file <- "../../../../Data/Primary/VIC/VIC_params_global.nc"
tfactor.out <- "./Saves/tfactor_30min_global.RDS"
lapse.rate <- -0.0065

# Load
nc <- nc_open(elev.file)
elev <- ncvar_get(nc, "elevation")
area <- ncvar_get(nc, "AreaFract")
nc_close(nc)

# Setup
elev.mean <- apply(X = elev * area, MARGIN = c(1, 2), FUN = sum)

# Calculate
Tfactor <- lapse.rate * (elev[, , 1] - elev.mean)
# image.plot(Tfactor)

# Save
dir.create(dirname(tfactor.out))
saveRDS(Tfactor, tfactor.out)
