rm(list = ls())

library(ncdf4)
library(fields)
library(humidity)

weather.dir.in = "./Saves/Combined_daily/"
weather.dir.out = "./Saves/Setup_daily/"
weather.dir.tmp = "./Saves/Converted_daily/"

# Load
in.files = list.files(weather.dir.in, pattern = "Dahri", full.names = T, recursive = T)
out.files = list.files(weather.dir.out, full.names = T, recursive = T)
tmp.files = list.files(weather.dir.tmp, pattern = "Dahri", full.names = T, recursive = T)

#for(tmp.file in tmp.files){
#  file.remove(tmp.file)
#}

# Setup
years = 1979:2018

# Convert temperature
year = years[1]
for(year in years){
  in.file.tasmin = grep(x = in.files, pattern = paste0("/tasmin_.*", year), value = T)
  in.file.tasmax = grep(x = in.files, pattern = paste0("/tasmax_.*", year), value = T)
  out.file.tas = grep(x = out.files, pattern = paste0("/tas_.*", year), value = T)
  tmp.file.tas = gsub(x = out.file.tas, pattern = weather.dir.out, replacement = weather.dir.tmp)
  tmp.file.tas = gsub(x = tmp.file.tas, pattern = "ERA5", replacement = "Dahri")
  if(file.exists(tmp.file.tas)){
    next
  }
  
  nc = nc_open(in.file.tasmin)
  tasmin = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  nc = nc_open(in.file.tasmax)
  tasmax = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  
  tas = (tasmax + tasmin) / 2
  
  print(basename(tmp.file.tas))
  dir.create(dirname(tmp.file.tas), recursive = T)
  file.copy(out.file.tas, tmp.file.tas)
  
  nc = nc_open(tmp.file.tas, write = T)
  ncvar_put(nc, nc$var[[1]], tas)
  nc_close(nc)
}
rm(tasmin, tasmax, tas)

# Convert vapour pressure
svp.cc = function(t, isK = T){
  if(!isK){
    t = t + 273.15
  }
  6.11 * exp((2.5e6 / 461.52) * (1/273.15 - 1/t))
}

year = years[1]
for(year in years){
  in.file.qair = grep(x = in.files, pattern = paste0("/qair_.*", year), value = T)
  in.file.tasmin = grep(x = in.files, pattern = paste0("/tasmin_.*", year), value = T)
  in.file.tasmax = grep(x = in.files, pattern = paste0("/tasmax_.*", year), value = T)
  in.file.psurf = grep(x = in.files, pattern = paste0("/psurf_.*", year), value = T)
  out.file.vp = grep(x = out.files, pattern = paste0("/vp_.*", year), value = T)
  tmp.file.vp = gsub(x = out.file.vp, pattern = weather.dir.out, replacement = weather.dir.tmp)
  tmp.file.vp = gsub(x = tmp.file.vp, pattern = "ERA5", replacement = "Dahri")
  if(file.exists(tmp.file.vp)){
    next
  }

  nc = nc_open(in.file.qair)
  qair = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  nc = nc_open(in.file.tasmin)
  tasmin = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  nc = nc_open(in.file.tasmax)
  tasmax = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  nc = nc_open(in.file.psurf)
  psurf = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  
  tas = (tasmax + tasmin) / 2
  svp = svp.cc(tas, isK = F)
  psi = SH2RH(qair, tas, psurf * 1e3, isK = F)
  vp = WVP2(psi, svp) * 1e-3
  #image.plot(vp[,,5])
  
  print(basename(tmp.file.vp))
  dir.create(dirname(tmp.file.vp), recursive = T)
  file.copy(out.file.vp, tmp.file.vp)
  
  nc = nc_open(tmp.file.vp, write = T)
  ncvar_put(nc, nc$var[[1]], vp)
  nc_close(nc)
}
rm(qair, tasmin, tasmax, psurf, tas, svp, psi, vp)
