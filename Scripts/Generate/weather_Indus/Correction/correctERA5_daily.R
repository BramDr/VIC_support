rm(list = ls())

library(ncdf4)
library(fields)
library(ncdf4.helpers)

force.dirs = c("../Combined_daily/pr_daily_ERA5/", "../Combined_daily/tas_daily_ERA5/")
cor.files = c("./Saves/ERA5_daily/pr_monthly_ERA5_correction.nc", "./Saves/ERA5_daily/tas_monthly_ERA5_correction.nc")
out.dir = "./Out/ERA5_daily/"

for(i in 1:length(force.dirs)){
  force.dir = force.dirs[i]
  cor.file = cor.files[i]
  
  var.name = gsub(x = basename(force.dir), pattern = "_.*", replacement = "")
  
  force.files = list.files(force.dir, full.names = T)

  nc = nc_open(cor.file)
  cor = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)

  out.dir.name = gsub(x = basename(force.dir), pattern = "ERA5", replacement = "ERA5adj")
  dir.create(paste0(out.dir, out.dir.name), recursive = T)

  for(force.file in force.files) {
    print(basename(force.file)) 
    
    out.name = gsub(x = basename(force.file), pattern = "ERA5", replacement = "ERA5adj")
    out.file = paste0(out.dir, out.dir.name, "/", out.name)
    
    file.copy(from = force.file, to = out.file, overwrite = T)
    
    nc = nc_open(out.file)
    time = nc.get.time.series(nc)
    force = ncvar_get(nc, nc$var[[1]])
    nc_close(nc)
    
    time.months = as.numeric(format(time, "%m"))
    force.adj = force
    
    if(var.name == "pr") {
      for(z in 1:length(time.months)){
        force.adj[,,z] = force[,,z] * cor[,,time.months[z]]
      }
    } else {
      for(z in 1:length(time.months)){
        force.adj[,,z] = force[,,z] + cor[,,time.months[z]]
      }
    }
    
    nc = nc_open(out.file, write = T)
    ncvar_put(nc, nc$var[[1]], force.adj)
    nc_close(nc)
  }
}
