rm(list = ls())

library(ncdf4)
library(fields)
library(ncdf4.helpers)

force.files = c("./Check/ERA5_daily/pr_monthly_ERA5adj_ymonsum.nc", "./Check/ERA5_daily/tas_monthly_ERA5adj_ymonmean.nc")
obs.files = c("./adj_pr_10km.nc", "./tas_cortime_10km.nc")

for(i in 1:length(force.files)){
  force.file = force.files[i]
  obs.file = obs.files[i]
  
  var.name = gsub(x = basename(force.file), pattern = "_.*", replacement = "")
  
  nc = nc_open(obs.file)
  obs.lons = nc$dim$lon$vals
  obs.lats = nc$dim$lat$vals
  obs = ncvar_get(nc, nc$var[[1]])
  obs = obs[,ncol(obs):1,]
  nc_close(nc)
  
  nc = nc_open(force.file)
  lons = nc$dim$lon$vals
  lats = nc$dim$lat$vals
  force = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  
  force.sel = force[round(lons, digits = 4) %in% round(obs.lons, digits = 4), round(lats, digits = 4) %in% round(obs.lats, digits = 4),]
  
  for(z in 1:12){
    image.plot(force.sel[,,z] - obs[,,z], main = paste(var.name, z))
  }
}
