rm(list = ls())
library(ncdf4)
library(fields)
library(ncdf4.helpers)
library(abind)

weather.dir.in = "/home/bram/Data/Primary/ERA5/"

# Load
in.files = list.files(weather.dir.in, full.names = T, recursive = T)
in.years = 1979:2018

out.data = array(NA, dim = c(69, 61, length(in.years)))

in.file = in.files[3]
for(in.file in in.files){
  print(in.file)
  
  nc = nc_open(in.file)
  time = nc.get.time.series(nc)
  years = as.numeric(format(time, "%Y"))
  varname = nc$var[[1]]$name
  nc_close(nc)
  
  if(varname != "tp"){
    next
  }
  
  nc = nc_open(in.file)
  in.data = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  
  year = 1993
  for(year in unique(years)){
    if(! (year %in% in.years)){
      next
    }
    
    print(year)
    in.data.mean = apply(X = in.data[,,years == year], MARGIN = c(1,2), FUN = mean)
    out.data[,,which(in.years == year)] = in.data.mean
  }
  rm(in.data)
}

out.data.time = apply(X = out.data, MARGIN = 3, FUN = sum)
plot(in.years, out.data.time, type = "l")
