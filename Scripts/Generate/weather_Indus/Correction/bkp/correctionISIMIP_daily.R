rm(list = ls())

library(ncdf4)
library(fields)
library(ncdf4.helpers)

obs.pr.file = "./adj_pr_10km.nc"
obs.tas.file = "./tas_cortime_10km.nc"
sub.dirs = c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
sub.dirs = c("GFDL-ESM4")

for(sub.dir in sub.dirs) {
  pr.file = paste0("./Saves/", sub.dir, "/pr_monthly_", sub.dir, "_ymonsum.nc")
  tas.file = paste0("./Saves/", sub.dir, "/tas_monthly_", sub.dir, "_ymonmean.nc")
  cor.pr.out = paste0("./Saves/", sub.dir, "/pr_correction_monthly_", sub.dir, ".nc")
  cor.tas.out = paste0("./Saves/", sub.dir, "/tas_correction_monthly_", sub.dir, ".nc")

  # Load
  nc = nc_open(obs.pr.file)
  obs.lons = nc$dim$lon$vals
  obs.lats = nc$dim$lat$vals
  obs.pr = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)

  nc = nc_open(obs.tas.file)
  obs.tas = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)

  nc = nc_open(pr.file)
  lons = nc$dim$lon$vals
  lats = nc$dim$lat$vals
  resolution = lons[2] - lons[1]
  pr = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)

  nc = nc_open(tas.file)
  tas = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)

  # Calculate
  pr.cor = array(1, dim = dim(pr))
  tas.cor = array(0, dim = dim(tas))

  x = 16
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
      
      if(!is.na(obs.pr[obs.x, obs.y, 1])){
        pr.cor[x,y,] =  obs.pr[obs.x, obs.y,] / pr[x,y,]
      }
      
      if(!is.na(obs.tas[obs.x, obs.y, 1])){
        tas.cor[x,y,] =  obs.tas[obs.x, obs.y,] - tas[x,y,]
      }
    }
  }
  pr.cor[pr == 0] = 1
  image.plot(pr.cor[,,1])
  image.plot(tas.cor[,,1])

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
  var.pr = ncvar_def(
    name = "pr_corfactor",
    units = "fraction", 
    dim = list(dim.lon, dim.lat, dim.time), 
    longname = "precipitation correction factor"
  )
  var.tas = ncvar_def(
    name = "tas_coroffset",
    units = "degrees_celsius", 
    dim = list(dim.lon, dim.lat, dim.time), 
    longname = "temperature correction offset"
  )


  dir.create(dirname(cor.pr.out))
  nc = nc_create(cor.pr.out, vars = list(var.pr))
  ncvar_put(nc, varid = var.pr, pr.cor)
  nc_close(nc)

  dir.create(dirname(cor.tas.out))
  nc = nc_create(cor.tas.out, vars = list(var.tas))
  ncvar_put(nc, varid = var.tas, tas.cor)
  nc_close(nc)
}
