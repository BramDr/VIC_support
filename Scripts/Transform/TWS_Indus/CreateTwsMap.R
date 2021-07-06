library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
grace.dir <- "../../../../Data/Primary/GRACE"
grace.adj.file <- "../../../../Data/Primary/GRACE/CLM4.SCALE_FACTOR.DS.G300KM.RL05.DSTvSCS1409.nc"
tws.out <- "../../../../Data/Transformed/TWS/tws_5min_Indus.RDS"
tws.adj.out <- "../../../../Data/Transformed/TWS/twsAdj_5min_Indus.RDS"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

extent.out <- extent(min(out.lons) - resolution / 2, 
                     max(out.lons) + resolution / 2, 
                     min(out.lats) - resolution / 2, 
                     max(out.lats) + resolution / 2)

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

adj.indus <- array(0, dim = c(length(out.lons), length(out.lats)))

adj.factor.tmp = adj.factor
adj.factor.tmp = t(adj.factor.tmp[,ncol(adj.factor.tmp):1])
adj.raster = raster(adj.factor.tmp)
extent(adj.raster) = c(-180, 180, -90, 90)

adj.avg.tmp = adj.indus
adj.avg.tmp = t(adj.avg.tmp[,ncol(adj.avg.tmp):1])
adj.template = raster(adj.avg.tmp)
extent(adj.template) = extent.out

adj.resample = resample(adj.raster, adj.template)
adj.matrix = as.matrix(adj.resample)
adj.matrix = t(adj.matrix[nrow(adj.matrix):1,])

adj.indus = adj.matrix
image.plot(adj.indus)

# Calculate
tws.avg <- array(0, dim = c(length(out.lons), length(out.lats), length(time)))
dimnames(tws.avg)[[3]] <- time

i = 1
for (i in 1:length(grace.files)) {
  print(basename(grace.files[i]))
  
  file <- grace.files[i]
  name <- grace.names[[i]][2]

  nc <- nc_open(file)
  tws.global <- ncvar_get(nc, varid = nc$var$lwe_thickness)
  nc_close(nc)
  tws.global <- tws.global[c(181:360, 1:180), , ]
  
  tws.indus <- array(NA, dim = c(length(out.lons), length(out.lats), length(time)))
  
  z = 1
  for(z in 1:dim(tws.global)[3]){
    print(z)
    
    tws.global.tmp = tws.global[,,z]
    tws.global.tmp = t(tws.global.tmp[,ncol(tws.global.tmp):1])
    
    tws.raster = raster(tws.global.tmp)
    extent(tws.raster) = c(-180, 180, -90, 90)
    
    tws.avg.tmp = tws.avg[,,z]
    tws.avg.tmp = t(tws.avg.tmp[,ncol(tws.avg.tmp):1])
    
    tws.template = raster(tws.avg.tmp)
    extent(tws.template) = extent.out
    
    tws.resample = resample(tws.raster, tws.template)
    
    tws.matrix = as.matrix(tws.resample)
    tws.matrix = t(tws.matrix[nrow(tws.matrix):1,])
    
    tws.indus[,,z] = tws.matrix
  }
  image.plot(tws.indus[, , 100])
  dimnames(tws.indus)[[3]] <- time

  tws.avg <- tws.avg + tws.indus / length(grace.files)

  tws.tmp.out <- gsub(x = tws.out, pattern = "tws_", replacement = paste0("tws", name, "_"))

  dir.create(dirname(tws.tmp.out))
  saveRDS(object = tws.indus, file = tws.tmp.out)
}
image.plot(tws.avg[, , 100])

tws.avg.space = apply(X = tws.avg, MARGIN = c(1,2), FUN = mean)
image.plot(tws.avg.space)
tws.avg.time = apply(X = tws.avg, MARGIN = 3, FUN = mean)
plot(time, tws.avg.time, type = "l")

tws.adj <- tws.avg
for (z in 1:dim(tws.adj)[3]) {
  tws.adj[, , z] <- tws.avg[, , z] * adj.indus
}
image.plot(tws.adj[, , 100])

# Save
dir.create(dirname(tws.out))
saveRDS(tws.avg, tws.out)
dir.create(dirname(tws.adj.out))
saveRDS(tws.adj, tws.adj.out)
