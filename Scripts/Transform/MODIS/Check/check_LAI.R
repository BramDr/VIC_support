rm(list = ls())
library(fields)
library(zoo)

cv.file = "../../../../Data/Transformed/MODIS/Cv_5min_Indus.RDS"
lai.file = "../Spatial_aggregation/Saves/LAI_5min_Indus.RDS"
ndvi.file = "../Spatial_aggregation/Saves/NDVI_5min_Indus.RDS"
out.dir = "./Saves"

cv = readRDS(cv.file)
lai = readRDS(lai.file)
ndvi = readRDS(ndvi.file)

fcanopy = ((ndvi - 0.1) / (0.8 - 0.1)) ^ 2
lai2 = 8 * (ndvi - 0.1) ^ 2

lai.adj = lai / fcanopy
lai.adj[fcanopy <= 0] = 0

v = 13
m = 10
image.plot(ndvi[,,v,m], zlim = c(0,1))
image.plot(fcanopy[,,v,m], zlim = c(0,1))
image.plot(lai[,,v,m], zlim = c(0, 6))
image.plot(lai2[,,v,m], zlim = c(0, 6))
image.plot(lai.adj[,,v,m], zlim = c(0,6))
