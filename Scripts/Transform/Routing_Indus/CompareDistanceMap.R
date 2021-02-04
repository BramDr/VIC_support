library(raster)
library(fields)
library(sp)
rm(list = ls())

# Input
distance.file <- "../../../Data/Transformed/Routing/distance_5min_Indus.RDS"
downstream.file <- "../../../Data/Transformed/Routing/downstream_5min_Indus.RDS"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

distance = readRDS(distance.file)
downstream = readRDS(downstream.file)

# Calculate
distance2 = array(0, dim = dim(distance))
x = 20
y = 20
for(x in 1:dim(downstream)[1]){
  for(y in 1:dim(downstream)[2]){
    if(is.na(downstream[x,y,1])){
      next
    }
    
    nx = downstream[x,y,1]
    ny = downstream[x,y,2]
    
    pts = matrix(c(out.lons[x],out.lats[y]), ncol = 2)
    pt = matrix(c(out.lons[nx], out.lats[ny]), ncol = 2)
    colnames(pts) = c("lon", "lat")
    colnames(pt) = c("lon", "lat")
    dist = spDistsN1(pts, pt, longlat = T)
    distance2[x,y] = dist * 1e3
  }
}

image.plot(distance, zlim = c(5000, 15000))
image.plot(distance2, zlim = c(5000, 15000))
