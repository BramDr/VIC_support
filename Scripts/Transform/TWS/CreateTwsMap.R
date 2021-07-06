library(ncdf4)
library(fields)
rm(list = ls())

# Input
grace.dir <- "../../../../Data/Primary/GRACE"
grace.adj.file <- "../../../../Data/Primary/GRACE/CLM4.SCALE_FACTOR.DS.G300KM.RL05.DSTvSCS1409.nc"
tws.out <- "../../../../Data/Transformed/TWS/tws_60min_global.RDS"
tws.adj.out <- "../../../../Data/Transformed/TWS/twsAdj_60min_global.RDS"

# Load
grace.files <- list.files(path = grace.dir, pattern = "*GRCTellus*", full.names = T)
grace.names <- strsplit(x = basename(grace.files), split = "\\.")

nc <- nc_open(grace.adj.file)
adj.factor <- ncvar_get(nc, nc$var$SCALE_FACTOR)
nc_close(nc)
adj.factor <- adj.factor[c(181:360, 1:180), ]
image.plot(adj.factor)

# Setup
nc <- nc_open(grace.files[1])
time <- as.Date(nc$dim$time$vals, origin = "2002-01-01")
nc_close(nc)

# Calculate
tws.avg <- array(0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$time$len))
dimnames(tws.avg)[[3]] <- time
for (i in 1:length(grace.files)) {
  file <- grace.files[i]
  name <- grace.names[[i]][2]

  nc <- nc_open(file)
  tws <- ncvar_get(nc, varid = nc$var$lwe_thickness)
  nc_close(nc)
  tws <- tws[c(181:360, 1:180), , ]
  image.plot(tws[, , 1])
  dimnames(tws)[[3]] <- time

  tws.avg <- tws.avg + tws / length(grace.files)

  tws.tmp.out <- gsub(x = tws.out, pattern = "tws_", replacement = paste0("tws", name, "_"))

  dir.create(dirname(tws.tmp.out))
  saveRDS(object = tws, file = tws.tmp.out)
}
image.plot(tws.avg[, , 1])

tws.adj <- tws.avg
for (z in 1:dim(tws.adj)[3]) {
  tws.adj[, , z] <- tws.avg[, , z] * adj.factor
}
image.plot(tws.adj[, , 1])

# Save
dir.create(dirname(tws.out))
saveRDS(tws.avg, tws.out)
dir.create(dirname(tws.adj.out))
saveRDS(tws.adj, tws.adj.out)
