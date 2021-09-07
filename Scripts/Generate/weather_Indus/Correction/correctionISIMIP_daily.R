#rm(list = ls())

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
  force.files = c(paste0("./Saves/", sub.dir, "/pr_monthly_", sub.dir, "_historical_ymonsum.nc"), 
                  paste0("./Saves/", sub.dir, "/tas_monthly_", sub.dir, "_historical_ymonmean.nc"), 
                  paste0("./Saves/", sub.dir, "/lwdown_monthly_", sub.dir, "_historical_ymonmean.nc"), 
                  paste0("./Saves/", sub.dir, "/swdown_monthly_", sub.dir, "_historical_ymonmean.nc"), 
                  paste0("./Saves/", sub.dir, "/psurf_monthly_", sub.dir, "_historical_ymonmean.nc"), 
                  paste0("./Saves/", sub.dir, "/wind10_monthly_", sub.dir, "_historical_ymonmean.nc"), 
                  paste0("./Saves/", sub.dir, "/vp_monthly_", sub.dir, "_historical_ymonmean.nc"))
  force.files = c(paste0("./Saves/", sub.dir, "/pr_monthly_", sub.dir, "_historical_ymonsum.nc"), 
                  paste0("./Saves/", sub.dir, "/tas_monthly_", sub.dir, "_historical_ymonmean.nc"), 
                  paste0("./Saves/", sub.dir, "/lwdown_monthly_", sub.dir, "_historical_ymonmean.nc"), 
                  paste0("./Saves/", sub.dir, "/swdown_monthly_", sub.dir, "_historical_ymonmean.nc"))
  cor.out = paste0("./Saves/", sub.dir, "/")
  
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

    if(var.name %in% c("pr", "lwdown", "swdown", "wind10")) {
      cor = array(1, dim = dim(force))
    } else {
      cor = array(0, dim = dim(force))
    }

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
        
        if(var.name %in% c("pr", "lwdown", "swdown", "wind10")) {
          if(!is.na(obs[x, y, 1])){
            cor[x,y,] =  obs[x, y,] / force[x,y,]
            cor[x,y,force[x,y,] == 0] = 1
          }
        } else {
          if(!is.na(obs[x, y, 1])){
            cor[x,y,] =  obs[x, y,] - force[x,y,]
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
}
