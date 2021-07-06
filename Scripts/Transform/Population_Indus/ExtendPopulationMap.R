library(fields)
library(raster)
library(ncdf4)
rm(list = ls())

# Input
hist.pop.file <- "../../../Data/Transformed/Population/population_historical_5min_Indus.RDS"
ssp1.pop.file <- "../../../Data/Transformed/Population/population_ssp1_5min_Indus.RDS"
ssp2.pop.file <- "../../../Data/Transformed/Population/population_ssp2_5min_Indus.RDS"
ssp3.pop.file <- "../../../Data/Transformed/Population/population_ssp3_5min_Indus.RDS"
hist.pop.out <- "../../../Data/Transformed/Population/population_historical_extended_5min_Indus.RDS"
ssp1.pop.out <- "../../../Data/Transformed/Population/population_ssp1_extended_5min_Indus.RDS"
ssp2.pop.out <- "../../../Data/Transformed/Population/population_ssp2_extended_5min_Indus.RDS"
ssp3.pop.out <- "../../../Data/Transformed/Population/population_ssp3_extended_5min_Indus.RDS"

# Load
hist.pop = readRDS(hist.pop.file)
ssp1.pop = readRDS(ssp1.pop.file)
ssp2.pop = readRDS(ssp2.pop.file)
ssp3.pop = readRDS(ssp3.pop.file)

hist.years = as.numeric(dimnames(hist.pop)[[3]])
ssp1.years = as.numeric(dimnames(ssp1.pop)[[3]])
ssp2.years = as.numeric(dimnames(ssp2.pop)[[3]])
ssp3.years = as.numeric(dimnames(ssp3.pop)[[3]])

# Setup
hist.years.ext = min(hist.years):max(hist.years)
ssp1.years.ext = (max(hist.years) + 1):2100
ssp2.years.ext = (max(hist.years) + 1):2100
ssp3.years.ext = (max(hist.years) + 1):2100

# Calculate
hist.pop.ext = array(NA, dim = c(dim(hist.pop)[1:2], length(hist.years.ext)))
ssp1.pop.ext = array(NA, dim = c(dim(ssp1.pop)[1:2], length(ssp1.years.ext)))
ssp2.pop.ext = array(NA, dim = c(dim(ssp2.pop)[1:2], length(ssp2.years.ext)))
ssp3.pop.ext = array(NA, dim = c(dim(ssp3.pop)[1:2], length(ssp3.years.ext)))

year = hist.years.ext[1]
for(year in hist.years.ext) {
  print(year)
  
  prev.idx = max(which(hist.years - year <= 0))
  next.idx = prev.idx + 1
  if(next.idx > dim(hist.pop)[3]){
    prev.idx = prev.idx - 1
    next.idx = next.idx - 1
  }
  
  prev.pop = hist.pop[,,prev.idx]
  next.pop = hist.pop[,,next.idx]
  prev.year = hist.years[prev.idx]
  next.year = hist.years[next.idx]
  diff.pop = (next.pop - prev.pop) / (next.year - prev.year)
  diff.pop[is.na(diff.pop)] = 0
  #image.plot(diff.pop)
  
  step = year - prev.year
  pop = prev.pop + diff.pop * step
  
  hist.pop.ext[,,which(hist.years.ext == year)] = pop
}
hist.pop.temp = apply(X = hist.pop.ext, MARGIN = 3, FUN = sum, na.rm = T)
plot(hist.years.ext, hist.pop.temp)

year = ssp1.years.ext[1]
for(year in ssp1.years.ext) {
  print(year)
  
  if(year <= min(ssp1.years)){
    prev.pop = hist.pop[,,dim(hist.pop)[3]]
    prev.year = hist.years[length(hist.years)]
    next.pop = ssp1.pop[,,1]
    next.year = ssp1.years[1]
  } else if (year >= max(ssp1.years)) {
    prev.pop = ssp1.pop[,,dim(ssp1.pop)[3] - 1]
    prev.year = ssp1.years[length(ssp1.years) - 1]
    next.pop = ssp1.pop[,,dim(ssp1.pop)[3]]
    next.year = ssp1.years[length(ssp1.years)]
  } else {
    prev.idx = max(which(ssp1.years - year <= 0))
    next.idx = prev.idx + 1
    
    prev.pop = ssp1.pop[,,prev.idx]
    next.pop = ssp1.pop[,,next.idx]
    prev.year = ssp1.years[prev.idx]
    next.year = ssp1.years[next.idx]
  }
  #image.plot(prev.pop, zlim = c(0,4e6))
  #image.plot(next.pop, zlim = c(0,4e6))
  
  diff.pop = (next.pop - prev.pop) / (next.year - prev.year)
  diff.pop[is.na(diff.pop)] = 0
  #image.plot(diff.pop)
  step = year - prev.year
  pop = prev.pop + diff.pop * step
  pop[pop < 0] = 0
  #image.plot(pop, zlim = c(0,4e6))
  
  ssp1.pop.ext[,,which(ssp1.years.ext == year)] = pop
}
ssp1.pop.temp = apply(X = ssp1.pop.ext, MARGIN = 3, FUN = sum, na.rm = T)
plot(ssp1.years.ext, ssp1.pop.temp)

year = ssp2.years.ext[1]
for(year in ssp2.years.ext) {
  print(year)
  
  if(year <= min(ssp2.years)){
    prev.pop = hist.pop[,,dim(hist.pop)[3]]
    prev.year = hist.years[length(hist.years)]
    next.pop = ssp2.pop[,,1]
    next.year = ssp2.years[1]
  } else if (year >= max(ssp2.years)) {
    prev.pop = ssp2.pop[,,dim(ssp2.pop)[3] - 1]
    prev.year = ssp2.years[length(ssp2.years) - 1]
    next.pop = ssp2.pop[,,dim(ssp2.pop)[3]]
    next.year = ssp2.years[length(ssp2.years)]
  } else {
    prev.idx = max(which(ssp2.years - year <= 0))
    next.idx = prev.idx + 1
    
    prev.pop = ssp2.pop[,,prev.idx]
    next.pop = ssp2.pop[,,next.idx]
    prev.year = ssp2.years[prev.idx]
    next.year = ssp2.years[next.idx]
  }
  #image.plot(prev.pop, zlim = c(0,4e6))
  #image.plot(next.pop, zlim = c(0,4e6))
  
  diff.pop = (next.pop - prev.pop) / (next.year - prev.year)
  diff.pop[is.na(diff.pop)] = 0
  #image.plot(diff.pop)
  step = year - prev.year
  pop = prev.pop + diff.pop * step
  pop[pop < 0] = 0
  #image.plot(pop, zlim = c(0,4e6))
  
  ssp2.pop.ext[,,which(ssp2.years.ext == year)] = pop
}
ssp2.pop.temp = apply(X = ssp2.pop.ext, MARGIN = 3, FUN = sum, na.rm = T)
plot(ssp2.years.ext, ssp2.pop.temp)

year = ssp3.years.ext[1]
for(year in ssp3.years.ext) {
  print(year)
  
  if(year <= min(ssp3.years)){
    prev.pop = hist.pop[,,dim(hist.pop)[3]]
    prev.year = hist.years[length(hist.years)]
    next.pop = ssp3.pop[,,1]
    next.year = ssp3.years[1]
  } else if (year >= max(ssp3.years)) {
    prev.pop = ssp3.pop[,,dim(ssp3.pop)[3] - 1]
    prev.year = ssp3.years[length(ssp3.years) - 1]
    next.pop = ssp3.pop[,,dim(ssp3.pop)[3]]
    next.year = ssp3.years[length(ssp3.years)]
  } else {
    prev.idx = max(which(ssp3.years - year <= 0))
    next.idx = prev.idx + 1
    
    prev.pop = ssp3.pop[,,prev.idx]
    next.pop = ssp3.pop[,,next.idx]
    prev.year = ssp3.years[prev.idx]
    next.year = ssp3.years[next.idx]
  }
  #image.plot(prev.pop, zlim = c(0,4e6))
  #image.plot(next.pop, zlim = c(0,4e6))
  
  diff.pop = (next.pop - prev.pop) / (next.year - prev.year)
  diff.pop[is.na(diff.pop)] = 0
  #image.plot(diff.pop)
  step = year - prev.year
  pop = prev.pop + diff.pop * step
  pop[pop < 0] = 0
  #image.plot(pop, zlim = c(0,4e6))
  
  ssp3.pop.ext[,,which(ssp3.years.ext == year)] = pop
}
ssp3.pop.temp = apply(X = ssp3.pop.ext, MARGIN = 3, FUN = sum, na.rm = T)
plot(ssp3.years.ext, ssp3.pop.temp)

dimnames(hist.pop.ext)[[3]] = hist.years.ext
dimnames(ssp1.pop.ext)[[3]] = ssp1.years.ext
dimnames(ssp2.pop.ext)[[3]] = ssp2.years.ext
dimnames(ssp3.pop.ext)[[3]] = ssp3.years.ext

# Save
dir.create(dirname(hist.pop.out))
saveRDS(hist.pop.ext, hist.pop.out)
dir.create(dirname(ssp1.pop.out))
saveRDS(ssp1.pop.ext, ssp1.pop.out)
dir.create(dirname(ssp2.pop.out))
saveRDS(ssp2.pop.ext, ssp2.pop.out)
dir.create(dirname(ssp3.pop.out))
saveRDS(ssp3.pop.ext, ssp3.pop.out)
