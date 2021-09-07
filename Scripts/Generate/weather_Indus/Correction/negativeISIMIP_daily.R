rm(list = ls())

library(ncdf4)
library(fields)
library(ncdf4.helpers)

sub.dirs = c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
sub.dirs = c("GFDL-ESM4")
scenarios = c("historical", "ssp126", "ssp370", "ssp585")
scenarios = c("historical")

for(sub.dir in sub.dirs) {  
  for(scenario in scenarios){
    force.dirs = c(paste0("./Out/", sub.dir, "/pr_daily_", sub.dir, "adj_", scenario, "/"),
                   paste0("./Out/", sub.dir, "/tas_daily_", sub.dir, "adj_", scenario, "/"),
                   paste0("./Out/", sub.dir, "/lwdown_daily_", sub.dir, "adj_", scenario, "/"),
                   paste0("./Out/", sub.dir, "/swdown_daily_", sub.dir, "adj_", scenario, "/"),
                   paste0("./Out/", sub.dir, "/psurf_daily_", sub.dir, "adj_", scenario, "/"),
                   paste0("./Out/", sub.dir, "/wind10_daily_", sub.dir, "adj_", scenario, "/"),
                   paste0("./Out/", sub.dir, "/vp_daily_", sub.dir, "adj_", scenario, "/"))
    force.dirs = c(paste0("./Out/", sub.dir, "/pr_daily_", sub.dir, "adj_", scenario, "/"),
                   paste0("./Out/", sub.dir, "/tas_daily_", sub.dir, "adj_", scenario, "/"),
                   paste0("./Out/", sub.dir, "/lwdown_daily_", sub.dir, "adj_", scenario, "/"),
                   paste0("./Out/", sub.dir, "/swdown_daily_", sub.dir, "adj_", scenario, "/"))

    for(i in 1:length(force.dirs)){
      force.dir = force.dirs[i]
      print(basename(force.dir))
  
      var.name = gsub(x = basename(force.dir), pattern = "_.*", replacement = "")
      
      if(var.name == "tas"){
        next
      }
      
      force.files = list.files(force.dir, full.names = T)
      
      for(force.file in force.files) {
        
        nc = nc_open(force.file)
        force = ncvar_get(nc, nc$var[[1]])
        nc_close(nc)
        
        negative = sum(force < 0, na.rm = T)
        if(negative > 0) {
          print(basename(force.file))
          print(negative)
        }
      }
    }
  }
}
