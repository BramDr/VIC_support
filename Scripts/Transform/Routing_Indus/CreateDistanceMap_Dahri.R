library(raster)
library(fields)
library(sp)
rm(list = ls())

# Input
downstream.file <- "../../../Data/Transformed/Routing/downstream_Dahri_5min_Indus.RDS"
distance.out <- "../../../Data/Transformed/Routing/distance_Dahri_5min_Indus.RDS"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

downstream = readRDS(downstream.file)

# Calculate
distance = array(0, dim = dim(downstream)[1:2])
for(x in 1:dim(downstream)[1]){
  for(y in 1:dim(downstream)[2]){
    if(is.na(downstream[x,y,1])){
      next
    }
    
    nx = downstream[x,y,1]
    ny = downstream[x,y,2]
    
    if(nx == x && ny == y){
      nx = nx + 1
      ny = ny + 1
    }
    
    pts = matrix(c(out.lons[x],out.lats[y]), ncol = 2)
    pt = matrix(c(out.lons[nx], out.lats[ny]), ncol = 2)
    colnames(pts) = c("lon", "lat")
    colnames(pt) = c("lon", "lat")
    dist = spDistsN1(pts, pt, longlat = T)
    distance[x,y] = dist * 1e3 # km to m
  }
}

image.plot(distance)

# Save
dir.create(dirname(distance.out))
saveRDS(distance, distance.out)
