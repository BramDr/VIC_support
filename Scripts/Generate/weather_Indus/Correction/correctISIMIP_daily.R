rm(list = ls())

library(ncdf4)
library(fields)
library(ncdf4.helpers)

sub.dirs = c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
#sub.dirs = c("GFDL-ESM4")
scenarios = c("historical", "ssp126", "ssp370", "ssp585")
#scenarios = c("historical")

for(sub.dir in sub.dirs) {
  cor.files = c(paste0("./Saves/", sub.dir, "/pr_monthly_", sub.dir, "_historical_correction.nc"),
                paste0("./Saves/", sub.dir, "/tas_monthly_", sub.dir, "_historical_correction.nc"),
                paste0("./Saves/", sub.dir, "/lwdown_monthly_", sub.dir, "_historical_correction.nc"),
                paste0("./Saves/", sub.dir, "/swdown_monthly_", sub.dir, "_historical_correction.nc"),
                paste0("./Saves/", sub.dir, "/psurf_monthly_", sub.dir, "_historical_correction.nc"),
                paste0("./Saves/", sub.dir, "/wind10_monthly_", sub.dir, "_historical_correction.nc"),
                paste0("./Saves/", sub.dir, "/vp_monthly_", sub.dir, "_historical_correction.nc"))
  cor.files = c(paste0("./Saves/", sub.dir, "/pr_monthly_", sub.dir, "_historical_correction.nc"),
                paste0("./Saves/", sub.dir, "/tas_monthly_", sub.dir, "_historical_correction.nc"),
                paste0("./Saves/", sub.dir, "/lwdown_monthly_", sub.dir, "_historical_correction.nc"),
                paste0("./Saves/", sub.dir, "/swdown_monthly_", sub.dir, "_historical_correction.nc"))
  out.dir = paste0("./Out/", sub.dir, "/")
  
  for(scenario in scenarios){
    force.dirs = c(paste0("../Disaggregated_daily/pr_daily_", sub.dir, "_", scenario, "/"),
                   paste0("../Disaggregated_daily/tas_daily_", sub.dir, "_", scenario, "/"),
                   paste0("../Disaggregated_daily/lwdown_daily_", sub.dir, "_", scenario, "/"),
                   paste0("../Disaggregated_daily/swdown_daily_", sub.dir, "_", scenario, "/"),
                   paste0("../Disaggregated_daily/psurf_daily_", sub.dir, "_", scenario, "/"),
                   paste0("../Disaggregated_daily/wind10_daily_", sub.dir, "_", scenario, "/"),
                   paste0("../Converted_daily/vp_daily_", sub.dir, "_", scenario, "/"))
    force.dirs = c(paste0("../Disaggregated_daily/pr_daily_", sub.dir, "_", scenario, "/"),
                   paste0("../Disaggregated_daily/tas_daily_", sub.dir, "_", scenario, "/"),
                   paste0("../Disaggregated_daily/lwdown_daily_", sub.dir, "_", scenario, "/"),
                   paste0("../Disaggregated_daily/swdown_daily_", sub.dir, "_", scenario, "/"))

    for(i in 1:length(force.dirs)){
      force.dir = force.dirs[i]
      cor.file = cor.files[i]
  
      var.name = gsub(x = basename(force.dir), pattern = "_.*", replacement = "")
      
      force.files = list.files(force.dir, full.names = T)

      nc = nc_open(cor.file)
      cor = ncvar_get(nc, nc$var[[1]])
      nc_close(nc)

      out.dir.name = gsub(x = basename(force.dir), pattern = sub.dir, replacement = paste0(sub.dir, "adj"))
      dir.create(paste0(out.dir, out.dir.name), recursive = T)

      for(force.file in force.files) {
        print(basename(force.file)) 
        
        out.name = gsub(x = basename(force.file), pattern = sub.dir, replacement = paste0(sub.dir, "adj"))
        out.file = paste0(out.dir, out.dir.name, "/", out.name)
        
        file.copy(from = force.file, to = out.file, overwrite = T)
        
        nc = nc_open(out.file)
        time = nc.get.time.series(nc)
        force = ncvar_get(nc, nc$var[[1]])
        nc_close(nc)
        
        time.months = as.numeric(format(time, "%m"))
        force.adj = force
        
        if(var.name %in% c("pr", "lwdown", "swdown", "wind10")) {
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
  }
}
