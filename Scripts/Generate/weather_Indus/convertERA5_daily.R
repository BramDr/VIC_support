rm(list = ls())

library(ncdf4)
library(fields)
library(humidity)

weather.dir.in = "./Saves/Combined_daily/"
weather.dir.out = "./Saves/Setup_daily/"
weather.dir.tmp = "./Saves/Converted_daily/"

# Load
in.files = list.files(weather.dir.in, pattern = "ERA5", full.names = T, recursive = T)
out.files = list.files(weather.dir.out, full.names = T, recursive = T)
tmp.files = list.files(weather.dir.tmp, pattern = "ERA5", full.names = T, recursive = T)

#for(tmp.file in tmp.files){
#  file.remove(tmp.file)
#}

# Setup
years = 1979:2018

# Convert wind 10m
year = years[1]
for(year in years){
  in.file.ucomponent = grep(x = in.files, pattern = paste0("/uwind10_.*", year), value = T)
  in.file.vcomponent = grep(x = in.files, pattern = paste0("/vwind10_.*", year), value = T)
  out.file.wind = grep(x = out.files, pattern = paste0("/wind10_.*", year), value = T)
  tmp.file.wind = gsub(x = out.file.wind, pattern = weather.dir.out, replacement = weather.dir.tmp)
  
  if(file.exists(tmp.file.wind)){
    next
  }
  if(length(in.file.ucomponent) == 0 || length(in.file.vcomponent) == 0){
    next
  }

  nc = nc_open(in.file.ucomponent)
  ucomponent = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  nc = nc_open(in.file.vcomponent)
  vcomponent = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  
  wind = sqrt(ucomponent ^ 2 + vcomponent ^ 2)
  
  dir.create(dirname(tmp.file.wind), recursive = T)
  file.copy(out.file.wind, tmp.file.wind)
  print(basename(tmp.file.wind))
  
  nc = nc_open(tmp.file.wind, write = T)
  ncvar_put(nc, nc$var[[1]], wind)
  nc_close(nc)
}
rm(ucomponent, vcomponent, wind)

# Convert wind 100m
year = years[1]
for(year in years){
  in.file.ucomponent = grep(x = in.files, pattern = paste0("/uwind100_.*", year), value = T)
  in.file.vcomponent = grep(x = in.files, pattern = paste0("/vwind10_.*", year), value = T)
  out.file.wind = grep(x = out.files, pattern = paste0("/wind100_.*", year), value = T)
  tmp.file.wind = gsub(x = out.file.wind, pattern = weather.dir.out, replacement = weather.dir.tmp)
  
  if(file.exists(tmp.file.wind)){
    next
  }
  if(length(in.file.ucomponent) == 0 || length(in.file.vcomponent) == 0){
    next
  }
  
  nc = nc_open(in.file.ucomponent)
  ucomponent = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  nc = nc_open(in.file.vcomponent)
  vcomponent = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  
  wind = sqrt(ucomponent ^ 2 + vcomponent ^ 2)
  
  dir.create(dirname(tmp.file.wind), recursive = T)
  file.copy(out.file.wind, tmp.file.wind)
  print(basename(tmp.file.wind))
  
  nc = nc_open(tmp.file.wind, write = T)
  ncvar_put(nc, nc$var[[1]], wind)
  nc_close(nc)
}
rm(ucomponent, vcomponent, wind)

# Convert vapour pressure
year = years[1]
for(year in years){
  in.file.tdew = grep(x = in.files, pattern = paste0("/tdew_.*", year), value = T)
  out.file.vp = grep(x = out.files, pattern = paste0("/vp_.*", year), value = T)
  tmp.file.vp = gsub(x = out.file.vp, pattern = weather.dir.out, replacement = weather.dir.tmp)
  
  if(file.exists(tmp.file.wind)){
    next
  }
  if(length(in.file.tdew) == 0){
    next
  }

  nc = nc_open(in.file.tdew)
  tdew = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  
  vp = WVP1(tdew, isK = F) * 1e-1
  
  dir.create(dirname(tmp.file.vp), recursive = T)
  file.copy(out.file.vp, tmp.file.vp)
  print(basename(tmp.file.vp))
  
  nc = nc_open(tmp.file.vp, write = T)
  ncvar_put(nc, nc$var[[1]], vp)
  nc_close(nc)
}
rm(tdew, vp)
