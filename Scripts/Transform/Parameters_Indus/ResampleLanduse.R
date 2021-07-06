library(fields)
library(ncdf4)
library(raster)
rm(list = ls())

param.file = "/home/bram/Data/VIC/Forcing/global/coverage_monthly_VICWOFOST/coverage_monthly_VICWOFOST_1979.nc"
param.out = "/home/bram/Data/Transformed/Parameters/global/"

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

nc = nc_open(param.file)
param.lons = nc$dim$lon$vals
param.lats = nc$dim$lat$vals
param.vars = names(nc$var)
nc_close(nc)

param.var = param.vars[1]
for (param.var in param.vars) {
  print(param.var)
  
  nc = nc_open(param.file)
  var.data = ncvar_get(nc, param.var, collapse_degen = F)
  nc_close(nc)
  
  if (length(dim(var.data)) == 4) {
    out.data = array(NA, dim = c(length(out.lons), length(out.lats), dim(var.data)[3:4]))
    z = 1
    for(z in 1:dim(var.data)[3]) {
      print(z)
      
      for(m in 1:dim(var.data)[4]){
      
        var.raster = t(var.data[,,z,m])
        var.raster = var.raster[nrow(var.raster):1,]
        var.raster = raster(var.raster)
        extent(var.raster) = c(-180, 180, -90, 90)
        #plot(var.raster)
        
        var.resample = raster(nrow = length(out.lats), ncol = length(out.lons))
        extent(var.resample) = extent.out
        var.resample = resample(var.raster, var.resample, method = "ngb")
        #plot(var.resample)
        
        var.matrix = as.matrix(var.resample)
        var.matrix = t(var.matrix)
        var.matrix = var.matrix[,ncol(var.matrix):1]
        #image.plot(var.matrix)
        
        out.data[,,z,m] = var.matrix
      }
    }
  } 
  else if (length(dim(var.data)) == 3) {
    out.data = array(NA, dim = c(length(out.lons), length(out.lats), dim(var.data)[3]))
    z = 1
    for(z in 1:dim(var.data)[3]) {
      print(z)
      
      var.raster = t(var.data[,,z])
      var.raster = var.raster[nrow(var.raster):1,]
      var.raster = raster(var.raster)
      extent(var.raster) = c(-180, 180, -90, 90)
      #plot(var.raster)
      
      var.resample = raster(nrow = length(out.lats), ncol = length(out.lons))
      extent(var.resample) = extent.out
      var.resample = resample(var.raster, var.resample, method = "ngb")
      #plot(var.resample)
      
      var.matrix = as.matrix(var.resample)
      var.matrix = t(var.matrix)
      var.matrix = var.matrix[,ncol(var.matrix):1]
      #image.plot(var.matrix)
      
      out.data[,,z] = var.matrix
    }
  } 
  else if (length(dim(var.data)) == 2) {
    out.data = array(NA, dim = c(length(out.lons), length(out.lats)))
      
    var.raster = t(var.data)
    var.raster = var.raster[nrow(var.raster):1,]
    var.raster = raster(var.raster)
    extent(var.raster) = c(-180, 180, -90, 90)
    #plot(var.raster)
    
    var.resample = raster(nrow = length(out.lats), ncol = length(out.lons))
    extent(var.resample) = extent.out
    var.resample = resample(var.raster, var.resample, method = "ngb")
    #plot(var.resample)
    
    var.matrix = as.matrix(var.resample)
    var.matrix = t(var.matrix)
    var.matrix = var.matrix[,ncol(var.matrix):1]
    #image.plot(var.matrix)
    
    out.data = var.matrix
  }
  else if (length(dim(var.data)) == 1) {
    out.data = var.data
  }
  
  param.out.tmp = paste0(param.out, "/", param.var, "_5min_Indus.RDS")
  dir.create(dirname(param.out.tmp))
  saveRDS(out.data, param.out.tmp)
}
