library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
vegatation.file <- "./Saves/vegetation_mapping.csv"
lai.file <- "../../../../Data/Transformed/MODIS/LAI_5min_Indus.RDS"
albedo.file <- "../../../../Data/Transformed/MODIS/albedo_5min_Indus.RDS"
ndvi.file <- "../../../../Data/Transformed/MODIS/NDVI_5min_Indus.RDS"
lai.out = "./Saves/LAI_5min_Indus.RDS"
albedo.out = "./Saves/albedo_5min_Indus.RDS"
fcanopy.out = "./Saves/fcanopy_5min_Indus.RDS"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
vegetation = read.csv(vegatation.file, stringsAsFactors = F)
lai.modis = readRDS(lai.file)
albedo.modis = readRDS(albedo.file)
ndvi.modis = readRDS(ndvi.file)

# Calculate
lai = array(0, dim = c(length(out.lons), length(out.lats), length(na.omit(unique(vegetation$vic))), 12))
albedo = array(0, dim = c(length(out.lons), length(out.lats), length(na.omit(unique(vegetation$vic))), 12))
ndvi = array(0, dim = c(length(out.lons), length(out.lats), length(na.omit(unique(vegetation$vic))), 12))
count = array(0, dim = c(length(out.lons), length(out.lats), length(na.omit(unique(vegetation$vic))), 12))

for(i in 1:nrow(vegetation)){
  if(is.na(vegetation$vic[i])){
    next
  }
  
  for(z in 1:dim(lai.modis)[4]){
    lai.modis.tmp = lai.modis[,,i,z]
    albedo.modis.tmp = albedo.modis[,,i,z]
    ndvi.modis.tmp = ndvi.modis[,,i,z]
    
    lai.tmp = lai[,1:168,vegetation$vic[i],z]
    albedo.tmp = albedo[,1:168,vegetation$vic[i],z]
    ndvi.tmp = ndvi[,1:168,vegetation$vic[i],z]
    count.tmp = count[,1:168,vegetation$vic[i],z]
    
    sel = !is.na(lai.modis.tmp)
    lai.tmp[sel] = lai.tmp[sel] + lai.modis.tmp[sel]
    albedo.tmp[sel] = albedo.tmp[sel] + albedo.modis.tmp[sel]
    ndvi.tmp[sel] = ndvi.tmp[sel] + ndvi.modis.tmp[sel]
    count.tmp[sel] = count.tmp[sel] + 1
    
    lai[,1:168,vegetation$vic[i],z] = lai.tmp
    albedo[,1:168,vegetation$vic[i],z] = albedo.tmp
    ndvi[,1:168,vegetation$vic[i],z] = ndvi.tmp
    count[,1:168,vegetation$vic[i],z] = count.tmp
  }
}
lai[count > 1] = lai[count > 1] / count[count > 1]
albedo[count > 1] = albedo[count > 1] / count[count > 1]
ndvi[count > 1] = ndvi[count > 1] / count[count > 1]

for(z in 169:180){
  lai[,z,,] = lai[,168,,]
  albedo[,z,,] = albedo[,168,,]
  ndvi[,z,,] = ndvi[,168,,]
}

# Calculate fcanopy
fcanopy = ((ndvi - 0.1) / (0.8 - 0.1)) ^ 2
fcanopy[!is.na(fcanopy) & fcanopy > 1] = 1
fcanopy[!is.na(fcanopy) & fcanopy < 0.01] = 0.01

# Calculate buildup/urban land LAI
lai[,,14,] = 8 * (fcanopy[,,14,] - 0.1) ^ 2

lai.mean = apply(X = lai, MARGIN = c(1,2,3), FUN = mean, na.rm = T)
lai.forest = apply(X = lai.mean[,,1:5], MARGIN = c(1,2), FUN = mean, na.rm = T)
lai.low = apply(X = lai.mean[,,c(6:11)], MARGIN = c(1,2), FUN = mean, na.rm = T)
lai.build = lai.mean[,,13]
lai.crop = lai.mean[,,12]
lai.bare = lai.mean[,,14]
image.plot(lai.forest, main = "lai.forest")
image.plot(lai.low, main = "lai.low")
image.plot(lai.build, main = "lai.build")
image.plot(lai.crop, main = "lai.crop")
image.plot(lai.bare, main = "lai.bare")

fcanopy.mean = apply(X = fcanopy, MARGIN = c(1,2,3), FUN = mean, na.rm = T)
fcanopy.forest = apply(X = fcanopy.mean[,,1:5], MARGIN = c(1,2), FUN = mean, na.rm = T)
fcanopy.low = apply(X = fcanopy.mean[,,c(6:11)], MARGIN = c(1,2), FUN = mean, na.rm = T)
fcanopy.build = fcanopy.mean[,,13]
fcanopy.crop = fcanopy.mean[,,12]
fcanopy.bare = fcanopy.mean[,,14]
image.plot(fcanopy.forest, main = "fcanopy.forest")
image.plot(fcanopy.low, main = "fcanopy.low")
image.plot(fcanopy.build, main = "fcanopy.build")
image.plot(fcanopy.crop, main = "fcanopy.crop")
image.plot(fcanopy.bare, main = "fcanopy.bare")

dir.create(dirname(lai.out))
dir.create(dirname(albedo.out))
dir.create(dirname(fcanopy.out))
saveRDS(lai, lai.out)
saveRDS(albedo, albedo.out)
saveRDS(fcanopy, fcanopy.out)
