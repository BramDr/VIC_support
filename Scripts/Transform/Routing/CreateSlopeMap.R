library(ncdf4)
library(raster)
library(fields)
rm(list = ls())

support.script <- "../../Support/mapFunctions.R"
area.file <- "../../../Data/Primary/VIC/domain_global.nc"
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
slope.file <- "../../../Data/Primary/worldBank/Slope/slope.tif"
slope.out <- "../../../Data/Transformed/Routing/slope_30min_global.RDS"

nc <- nc_open(area.file)
area <- ncvar_get(nc, nc$var$area)
nc_close(nc)

nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)
na.map <- is.na(mask)

dist <- sqrt(sqrt(area)^2 + sqrt(area)^2)
dist[is.na(mask)] <- NA

slope <- raster(slope.file)
extent(slope) <- c(-180, 180, -60, 90)
slope.extend <- extend(slope, extent(-180, 180, -90, 90))
slope.agg <- aggregate(x = slope.extend, fact = 6)
plot(slope.agg)

source(support.script)
slope.mat <- as.matrix(slope.agg)
slope.mat <- t(slope.mat[nrow(slope.mat):1, ])
image.plot(slope.mat)
slope.fill <- fillMap(slope.mat, na.map = na.map, nearest.function = getNearestMean)
image.plot(slope.fill)

slope.fin <- tan(slope.fill * (pi / 180))
image.plot(slope.fin)

dir.create(dirname(slope.out))
saveRDS(slope.fin, slope.out)
