rm(list = ls())
library(fields)

tile.file = "../tileset_indus.txt"
cv.file = "../../../../Data/Transformed/MODIS/Cv_5min_Indus.RDS"
albedo.file = "./Saves/albedo_5min_Indus.RDS"
lai.file = "./Saves/LAI_5min_Indus.RDS"
ndvi.file = "./Saves/NDVI_5min_Indus.RDS"

cv = readRDS(cv.file)
albedo = readRDS(albedo.file)
lai = readRDS(lai.file)
ndvi = readRDS(ndvi.file)

albedo.missing = albedo * 0
lai.missing = albedo * 0
ndvi.missing = albedo * 0

v = 1
for(v in 1:dim(cv)[3]){
  sel = cv[,,v] > 0
  
  z = 1
  for(z in 1:dim(albedo)[4]){
    albedo.missing[,,v,z] = is.na(albedo[,,v,z]) & sel
    lai.missing[,,v,z] = is.na(lai[,,v,z]) & sel
    ndvi.missing[,,v,z] = is.na(ndvi[,,v,z]) & sel
  }
}

albedo.missing.max = apply(X = albedo.missing, MARGIN = c(1,2,3), FUN = max)
lai.missing.max = apply(X = lai.missing, MARGIN = c(1,2,3), FUN = max)
ndvi.missing.max = apply(X = ndvi.missing, MARGIN = c(1,2,3), FUN = max)

albedo.mean = apply(X = albedo, MARGIN = c(1,2,3), FUN = mean, na.rm = T)
lai.mean = apply(X = lai, MARGIN = c(1,2,3), FUN = mean, na.rm = T)
ndvi.mean = apply(X = ndvi, MARGIN = c(1,2,3), FUN = mean, na.rm = T)

for(v in 2:(dim(cv)[3] - 1)){
  image.plot(albedo.missing.max[,,v], main = paste0("albedo v ", v))
  if(sum(albedo.mean[,,v], na.rm = T) > 0){
    image.plot(albedo.mean[,,v], main = paste0("albedo v ", v))
  }
  
  image.plot(lai.missing.max[,,v], main = paste0("lai v ", v))
  if(sum(lai.mean[,,v], na.rm = T) > 0){
    image.plot(lai.mean[,,v], main = paste0("lai v ", v))
  }
  
  image.plot(ndvi.missing.max[,,v], main = paste0("ndvi v ", v))
  if(sum(ndvi.mean[,,v], na.rm = T) > 0){
    image.plot(ndvi.mean[,,v], main = paste0("ndvi v ", v))
  }
}
