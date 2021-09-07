library(fields)
library(raster)
library(ncdf4)
rm(list = ls())

# Input
hist.pop.dir <- "../../../Data/Primary/Hyde/"
fut.pop.dir <- "../../../Data/Primary/Smolenaars2021/"
hist.pop.out <- "../../../Data/Transformed/Population/population_historical_5min_Indus.RDS"
ssp1.pop.out <- "../../../Data/Transformed/Population/population_ssp1_5min_Indus.RDS"
ssp2.pop.out <- "../../../Data/Transformed/Population/population_ssp2_5min_Indus.RDS"
ssp3.pop.out <- "../../../Data/Transformed/Population/population_ssp3_5min_Indus.RDS"
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
hist.pop.files = list.files(hist.pop.dir, pattern = "popc", full.names = T)
ssp1.pop.files = list.files(fut.pop.dir, pattern = "Prosperous", full.names = T)
ssp2.pop.files = list.files(fut.pop.dir, pattern = "Business as Usual", full.names = T)
ssp3.pop.files = list.files(fut.pop.dir, pattern = "Downhill", full.names = T)

# Calculate
## historical
hist.pop = array(NA, dim = c(length(out.lons), length(out.lats), length(hist.pop.files)))
hist.years = c()

hist.pop.file = hist.pop.files[1]
for(hist.pop.file in hist.pop.files){
  print(basename(hist.pop.file))
  
  year = gsub(x = basename(hist.pop.file), pattern = "popc_", replacement = "")
  year = gsub(x = basename(year), pattern = "AD.*", replacement = "")
  year = as.numeric(year)
  
  pop <- raster(hist.pop.file)
  pop <- crop(pop, extent.out)
  #plot(pop)
  
  pop = as.matrix(pop)
  pop = t(pop[nrow(pop):1,])
  pop[is.na(pop)] = 0
  #image.plot(pop)
  
  hist.pop[,,which(hist.pop.files == hist.pop.file)] = pop
  hist.years = c(hist.years, year)
}
image.plot(hist.pop[,,28], zlim = c(0,25e5))

hist.pop = hist.pop[,,order(hist.years)]
dimnames(hist.pop)[[3]] = hist.years[order(hist.years)]

## ssp1
ssp1.pop = array(NA, dim = c(length(out.lons), length(out.lats), length(ssp1.pop.files)))
ssp1.years = c()

ssp1.pop.file = ssp1.pop.files[1]
for(ssp1.pop.file in ssp1.pop.files){
  print(basename(ssp1.pop.file))
  
  year = gsub(x = basename(ssp1.pop.file), pattern = ".*_", replacement = "")
  year = gsub(x = basename(year), pattern = ".nc", replacement = "")
  year = as.numeric(year)
  
  nc = nc_open(ssp1.pop.file)
  pop.lons = nc$dim$lon$vals
  pop.lats = nc$dim$lat$vals
  pop <- ncvar_get(nc, "layer")
  nc_close(nc)
  
  #image.plot(pop, zlim = c(0,25e5))
  
  pop.tmp = ssp1.pop[,,which(ssp1.pop.file == ssp1.pop.files)]
  for(x in 1:dim(pop.tmp)[1]){
    x.diff = abs(pop.lons - out.lons[x])
    if(min(x.diff) >= resolution){
      next
    }
    x.pop = which.min(x.diff)
    
    for(y in 1:dim(pop.tmp)[2]){
      y.diff = abs(pop.lats - out.lats[y])
      if(min(y.diff) >= resolution){
        next
      }
      y.pop = which.min(y.diff)
      
      pop.tmp[x,y] = pop[x.pop, y.pop]
    }
  }
  pop.tmp[is.na(pop.tmp)] = 0
  #image.plot(pop.tmp, zlim = c(0,25e5))
  
  ssp1.pop[,,which(ssp1.pop.file == ssp1.pop.files)] = pop.tmp
  ssp1.years = c(ssp1.years, year)
}
image.plot(ssp1.pop[,,7])

ssp1.pop = ssp1.pop[,,order(ssp1.years)]
dimnames(ssp1.pop)[[3]] = ssp1.years[order(ssp1.years)]

## ssp2
ssp2.pop = array(NA, dim = c(length(out.lons), length(out.lats), length(ssp2.pop.files)))
ssp2.years = c()

ssp2.pop.file = ssp2.pop.files[1]
for(ssp2.pop.file in ssp2.pop.files){
  print(basename(ssp2.pop.file))
  
  year = gsub(x = basename(ssp2.pop.file), pattern = ".*_", replacement = "")
  year = gsub(x = basename(year), pattern = ".nc", replacement = "")
  year = as.numeric(year)
  
  nc = nc_open(ssp2.pop.file)
  pop.lons = nc$dim$lon$vals
  pop.lats = nc$dim$lat$vals
  pop <- ncvar_get(nc, "layer")
  nc_close(nc)
  
  #image.plot(pop, zlim = c(0,25e5))
  
  pop.tmp = ssp2.pop[,,which(ssp2.pop.file == ssp2.pop.files)]
  for(x in 1:dim(pop.tmp)[1]){
    x.diff = abs(pop.lons - out.lons[x])
    if(min(x.diff) >= resolution){
      next
    }
    x.pop = which.min(x.diff)
    
    for(y in 1:dim(pop.tmp)[2]){
      y.diff = abs(pop.lats - out.lats[y])
      if(min(y.diff) >= resolution){
        next
      }
      y.pop = which.min(y.diff)
      
      pop.tmp[x,y] = pop[x.pop, y.pop]
    }
  }
  pop.tmp[is.na(pop.tmp)] = 0
  #image.plot(pop.tmp, zlim = c(0,25e5))
  
  ssp2.pop[,,which(ssp2.pop.file == ssp2.pop.files)] = pop.tmp
  ssp2.years = c(ssp2.years, year)
}
image.plot(ssp2.pop[,,7])

ssp2.pop = ssp2.pop[,,order(ssp2.years)]
dimnames(ssp2.pop)[[3]] = ssp2.years[order(ssp2.years)]

## ssp3
ssp3.pop = array(NA, dim = c(length(out.lons), length(out.lats), length(ssp3.pop.files)))
ssp3.years = c()

ssp3.pop.file = ssp3.pop.files[1]
for(ssp3.pop.file in ssp3.pop.files){
  print(basename(ssp3.pop.file))
  
  year = gsub(x = basename(ssp3.pop.file), pattern = ".*_", replacement = "")
  year = gsub(x = basename(year), pattern = ".nc", replacement = "")
  year = as.numeric(year)
  
  nc = nc_open(ssp3.pop.file)
  pop.lons = nc$dim$lon$vals
  pop.lats = nc$dim$lat$vals
  pop <- ncvar_get(nc, "layer")
  nc_close(nc)
  
  #image.plot(pop, zlim = c(0,25e5))
  
  pop.tmp = ssp3.pop[,,which(ssp3.pop.file == ssp3.pop.files)]
  for(x in 1:dim(pop.tmp)[1]){
    x.diff = abs(pop.lons - out.lons[x])
    if(min(x.diff) >= resolution){
      next
    }
    x.pop = which.min(x.diff)
    
    for(y in 1:dim(pop.tmp)[2]){
      y.diff = abs(pop.lats - out.lats[y])
      if(min(y.diff) >= resolution){
        next
      }
      y.pop = which.min(y.diff)
      
      pop.tmp[x,y] = pop[x.pop, y.pop]
    }
  }
  pop.tmp[is.na(pop.tmp)] = 0
  #image.plot(pop.tmp, zlim = c(0,25e5))
  
  ssp3.pop[,,which(ssp3.pop.file == ssp3.pop.files)] = pop.tmp
  ssp3.years = c(ssp3.years, year)
}
image.plot(ssp3.pop[,,7])

ssp3.pop = ssp3.pop[,,order(ssp3.years)]
dimnames(ssp3.pop)[[3]] = ssp3.years[order(ssp3.years)]

# Save
dir.create(dirname(hist.pop.out))
saveRDS(hist.pop, hist.pop.out)
dir.create(dirname(ssp1.pop.out))
saveRDS(ssp1.pop, ssp1.pop.out)
dir.create(dirname(ssp2.pop.out))
saveRDS(ssp2.pop, ssp2.pop.out)
dir.create(dirname(ssp3.pop.out))
saveRDS(ssp3.pop, ssp3.pop.out)
