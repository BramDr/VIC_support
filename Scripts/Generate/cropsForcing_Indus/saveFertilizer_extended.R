library(fields)
library(raster)
library(ncdf4)
rm(list = ls())

fertilizer.file <- "./Saves/fertilizerN_5min_Indus.RDS"
fertilizer.out <- "./Saves/fertilizer_extended_5min_Indus.RDS"
extension.years <- 2016:2100

# Load
fertilizer <- readRDS(fertilizer.file)
years = as.numeric(dimnames(fertilizer)[[3]])

# Setup
calc.slope = function(x) {
  if(is.na(x[1])){
    return(NA)
  }
  slope.years = 30
  #slope.y = as.numeric(x[(length(x) - slope.years):length(x)])
  #slope.x = 1:length(slope.data)
  #slope.lm = lm(formula = slope.y ~ slope.x)
  #slope.value = coefficients(slope.lm)[["slope.x"]]
  
  slope.value1 = (x[length(x)] - x[length(x) - slope.years]) / slope.years
  slope.value2 = (x[length(x)] - x[length(x) - slope.years / 2]) / (slope.years / 2)
  slope.value = mean(slope.value1, slope.value2)
  return(slope.value)
}

# Calculate
fertilizer.n <- array(0, dim = c(dim(fertilizer)[1:2], length(extension.years), dim(fertilizer)[4:5]))
fertilizer.p <- array(0, dim = c(dim(fertilizer)[1:2], length(extension.years), dim(fertilizer)[4:5]))
fertilizer.k <- array(0, dim = c(dim(fertilizer)[1:2], length(extension.years), dim(fertilizer)[4:5]))
fertilizer.dvs <- array(0, dim = c(dim(fertilizer)[1:2], length(extension.years), dim(fertilizer)[4:5]))

fertilizer.dvs[,,,,1] = 0
fertilizer.dvs[,,,,2] = 0.25
fertilizer.dvs[,,,,3] = 0.5
fertilizer.dvs[,,,,4] = 0.75

i <- 5
for(i in 1:dim(fertilizer)[4]){
  print(i)
  
  fertilizer.slope = apply(X = fertilizer[,,,i,1], MARGIN = c(1:2), FUN = calc.slope)
  fertilizer.slope[fertilizer.slope < 0] = 0
  
  j <- 1
  for (j in 1:length(extension.years)) {
    year = extension.years[j]
    print(year)
    
    fertilizer.add = fertilizer.slope * (year - max(years))
    
    k <- 1
    for(k in 1:dim(fertilizer)[5]){
      fertilizer.n[, , j, i, k] <- fertilizer[, , dim(fertilizer)[3], i, k] + fertilizer.add
    }
  }
  
  #image.plot(fertilizer.n[,,extension.years == 2050,i,1] * 4)
  #image.plot(fertilizer.n[,,extension.years == 2100,i,1] * 4)
  
  fertilizer.n.time = apply(X = fertilizer.n[,,,i,1] * 4, MARGIN = 3, FUN = median, na.rm = T)
  plot(fertilizer.n.time)
}

dimnames(fertilizer.n)[[3]] = extension.years
dimnames(fertilizer.p)[[3]] = extension.years
dimnames(fertilizer.k)[[3]] = extension.years
dimnames(fertilizer.dvs)[[3]] = extension.years

image.plot(fertilizer.n[,,80,1,1])

# Save
fertilizer.out.tmp <- gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = "fertilizerDVS_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.dvs, fertilizer.out.tmp)
fertilizer.out.tmp <- gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = "fertilizerN_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.n, fertilizer.out.tmp)
fertilizer.out.tmp <- gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = "fertilizerP_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.p, fertilizer.out.tmp)
fertilizer.out.tmp <- gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = "fertilizerK_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.k, fertilizer.out.tmp)
