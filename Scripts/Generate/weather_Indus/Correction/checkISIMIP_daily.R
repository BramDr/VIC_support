rm(list = ls())

library(ncdf4)
library(fields)
library(ncdf4.helpers)

mask.file = "./adj_pr_10km.nc"
obs.files = c("./Saves/ERA5adj_daily/pr_monthly_ERA5adj_ymonsum.nc",
              "./Saves/ERA5adj_daily/tas_monthly_ERA5adj_ymonmean.nc",
              "./Saves/ERA5adj_daily/lwdown_monthly_ERA5_ymonmean.nc",
              "./Saves/ERA5adj_daily/swdown_monthly_ERA5_ymonmean.nc",
              "./Saves/ERA5adj_daily/psurf_monthly_ERA5_ymonmean.nc",
              "./Saves/ERA5adj_daily/wind10_monthly_ERA5_ymonmean.nc",
              "./Saves/ERA5adj_daily/vp_monthly_ERA5_ymonmean.nc")
obs.files = c("./Saves/ERA5adj_daily/pr_monthly_ERA5adj_ymonsum.nc",
              "./Saves/ERA5adj_daily/tas_monthly_ERA5adj_ymonmean.nc",
              "./Saves/ERA5adj_daily/lwdown_monthly_ERA5_ymonmean.nc",
              "./Saves/ERA5adj_daily/swdown_monthly_ERA5_ymonmean.nc")
sub.dirs = c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
#sub.dirs = c("GFDL-ESM4")

# Load
nc = nc_open(mask.file)
mask.lons = nc$dim$lon$vals
mask.lats = nc$dim$lat$vals
mask = ncvar_get(nc, nc$var[[1]])
nc_close(nc)

for(sub.dir in sub.dirs) {
  force.files = c(paste0("./Check/", sub.dir, "/pr_monthly_", sub.dir, "adj_historical_ymonsum.nc"),
                  paste0("./Check/", sub.dir, "/tas_monthly_", sub.dir, "adj_historical_ymonmean.nc"),
                  paste0("./Check/", sub.dir, "/lwdown_monthly_", sub.dir, "adj_historical_ymonmean.nc"),
                  paste0("./Check/", sub.dir, "/swdown_monthly_", sub.dir, "adj_historical_ymonmean.nc"),
                  paste0("./Check/", sub.dir, "/psurf_monthly_", sub.dir, "adj_historical_ymonmean.nc"),
                  paste0("./Check/", sub.dir, "/wind10_monthly_", sub.dir, "adj_historical_ymonmean.nc"),
                  paste0("./Check/", sub.dir, "/vp_monthly_", sub.dir, "adj_historical_ymonmean.nc"))
  force.files = c(paste0("./Check/", sub.dir, "/pr_monthly_", sub.dir, "adj_historical_ymonsum.nc"),
                  paste0("./Check/", sub.dir, "/tas_monthly_", sub.dir, "adj_historical_ymonmean.nc"),
                  paste0("./Check/", sub.dir, "/lwdown_monthly_", sub.dir, "adj_historical_ymonmean.nc"),
                  paste0("./Check/", sub.dir, "/swdown_monthly_", sub.dir, "adj_historical_ymonmean.nc"))

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
  
    na.map = array(NA, dim = dim(force)[1:2])
    for(x in 1:length(lons)){
      mask.min.diff = abs(mask.lons - lons[x])
      if(min(mask.min.diff) > resolution / 2){
        next
      }
      
      mask.x = which.min(mask.min.diff)
      if(length(mask.x) == 0){
        next
      }
      for(y in 1:length(lats)){
        mask.min.diff = abs(mask.lats - lats[y])
        if(min(mask.min.diff) > resolution / 2){
          next
        }
        
        mask.y = which.min(mask.min.diff)
        if(length(mask.y) == 0){
          next
        }
        
        if(is.na(mask[mask.x, mask.y, 1])){
          next
        }
        
        na.map[x,y] = 1
      }
    }
    
    force.sel = force
    for(z in 1:12){
      force.sel[,,z] = force[,,z] * na.map
    }
    
    for(z in 1:12){
      image.plot(force.sel[,,z] - obs[,,z], main = paste(sub.dir, var.name, z))
    }
  }
}
