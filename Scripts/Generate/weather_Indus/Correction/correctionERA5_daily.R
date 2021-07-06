rm(list = ls())

library(ncdf4)
library(fields)
library(ncdf4.helpers)

force.files = c("./Saves/ERA5_daily/pr_monthly_ERA5_ymonsum.nc", "./Saves/ERA5_daily/tas_monthly_ERA5_ymonmean.nc")
obs.files = c("./adj_pr_10km.nc", "./tas_cortime_10km.nc")
cor.out = "./Saves/ERA5_daily/"

for(i in 1:length(force.files)){
  force.file = force.files[i]
  obs.file = obs.files[i]
  
  var.name = gsub(x = basename(force.file), pattern = "_.*", replacement = "")
  
  nc = nc_open(obs.file)
  obs.lons = nc$dim$lon$vals
  obs.lats = nc$dim$lat$vals
  obs = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  
  nc = nc_open(force.file)
  lons = nc$dim$lon$vals
  lats = nc$dim$lat$vals
  force = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  
  resolution = lons[2] - lons[1]
  
  if(var.name == "pr") {
    cor = array(1, dim = dim(force))
  } else {
    cor = array(0, dim = dim(force))
  }
  
  for(x in 1:length(lons)){
    obs.min.diff = abs(obs.lons - lons[x])
    if(min(obs.min.diff) > resolution / 2){
      next
    }
    
    obs.x = which.min(obs.min.diff)
    if(length(obs.x) == 0){
      next
    }
    for(y in 1:length(lats)){
      obs.min.diff = abs(obs.lats - lats[y])
      if(min(obs.min.diff) > resolution / 2){
        next
      }
      
      obs.y = which.min(obs.min.diff)
      if(length(obs.y) == 0){
        next
      }
      
      if(var.name == "pr") {
        if(!is.na(obs[obs.x, obs.y, 1])){
          cor[x,y,] =  obs[obs.x, obs.y,] / force[x,y,]
          cor[x,y,force[x,y,] == 0] = 1
        }
      } else {
        if(!is.na(obs[obs.x, obs.y, 1])){
          cor[x,y,] =  obs[obs.x, obs.y,] - force[x,y,]
        }
      }
    }
  }
  
  # Save
  dim.lon <- ncdim_def(
    name = "lon",
    units = "degrees_east",
    vals = lons,
    longname = "longitude of grid cell center"
  )
  dim.lat <- ncdim_def(
    name = "lat",
    units = "degrees_north",
    vals = lats,
    longname = "latitude of grid cell center"
  )
  time = seq.Date(from = as.Date("1970-01-01"), 
                    to = as.Date("1970-12-01"), 
                    by = "month")
  dim.time = ncdim_def(
    name = "time",
    units = paste0("days since 1970-01-01"),
    vals = as.numeric(time),
    calendar = "standard"
  )
  
  if(var.name == "pr") {
    var.cor = ncvar_def(
      name = paste0(var.name, "_corfactor"),
      units = "fraction", 
      dim = list(dim.lon, dim.lat, dim.time), 
      longname = "correction factor"
    )
  } else {
    var.cor = ncvar_def(
      name = paste0(var.name, "_coroffset"),
      units = "offset", 
      dim = list(dim.lon, dim.lat, dim.time), 
      longname = "correction offset"
    )
  }
  
  cor.out.name = gsub(x = basename(force.file), pattern = "ymonmean", replacement = "correction")
  cor.out.name = gsub(x = basename(cor.out.name), pattern = "ymonsum", replacement = "correction")
  cor.out.file = paste0(cor.out, cor.out.name)
  
  print(cor.out.file)

  dir.create(dirname(cor.out.file))
  nc = nc_create(cor.out.file, vars = list(var.cor))
  ncvar_put(nc, varid = var.cor, cor)
  nc_close(nc)
}

