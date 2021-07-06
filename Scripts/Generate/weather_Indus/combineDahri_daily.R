rm(list = ls())

library(ncdf4)
library(fields)
library(ncdf4.helpers)
library(abind)

weather.dir.in = "./Saves/Disaggregated_daily/"
weather.dir.out = "./Saves/Setup_daily/"
weather.dir.tmp = "./Saves/Combined_daily/"
years = 1979:2018
variable.merge = data.frame(Dahri = c("PREC", "PRESSURE", "TMIN", "TMAX", "SHORTWAVE", "LONGWAVE", "WIND", "QAIR"),
                            VIC = c("pr", "psurf", "tasmin", "tasmax", "swdown", "lwdown", "wind10", "qair"),
                            stringsAsFactors = F)

# Load
in.files = list.files(weather.dir.in, pattern = "Dahri", full.names = T, recursive = T)
out.files = list.files(weather.dir.out, full.names = T, recursive = T)
tmp.files = list.files(weather.dir.tmp, pattern = "Dahri", full.names = T, recursive = T)

#for(tmp.file in tmp.files){
#  file.remove(tmp.file)
#}

year = years[3]
for(year in years) {
  print(year)
  
  in.file = grep(x = in.files, pattern = year, value = T)

  in.weather = readRDS(in.file)
  in.varnames = names(in.weather)
  
  in.varname = in.varnames[1]
  in.varname = "pr"
  for(in.varname in in.varnames) {
    print(in.varname)
    
    out.varname = variable.merge$VIC[variable.merge$Dahri == in.varname]
    
    out.file = grep(x = out.files, pattern = paste0("/", out.varname, "_.*_", year), value = T)
    tmp.file = gsub(x = out.file, pattern = weather.dir.out, replacement = weather.dir.tmp)
    tmp.file = gsub(x = tmp.file, pattern = paste0(out.varname, "_daily"), replacement = paste0(out.varname, "_daily_Dahri"))
    if(file.exists(tmp.file)){
      next
    }
    
    # Create
    dir.create(dirname(tmp.file), recursive = T)
    file.copy(out.file, tmp.file)
    print(basename(tmp.file))
    
    # Save
    nc = nc_open(tmp.file, write = T)
    ncvar_put(nc, nc$var[[1]],
              vals = in.weather[[in.varname]])
    nc_close(nc)
  }
}
