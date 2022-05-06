rm(list = ls())
library(fields)
library(ncdf4)
library(Rcpp)

support.rcpp = "./Rcpp_support.cpp"
weather.in = "../hourly/"
weather.out = "./"

# Load
sourceCpp(support.rcpp)
weather.files = list.files(weather.in, recursive = T, pattern = ".nc$", full.names = T)

# Calculate
weather.file.in = weather.files[1]
for(weather.file.in in weather.files){
  weather.file.out = gsub(x = weather.file.in, pattern = weather.in, replacement = weather.out)
  weather.file.out = gsub(x = weather.file.out, pattern = "_hourly_", replacement = "_daily_")
  if(!file.exists(weather.file.out)){
    next
  }
  print(basename(weather.file.out))
  
  nc = nc_open(weather.file.in)
  dim.lon.in = nc$dim$lon
  dim.lat.in = nc$dim$lat
  dim.time.in = nc$dim$time
  data.in = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  
  nc = nc_open(weather.file.out)
  dim.lon.out = nc$dim$lon
  dim.lat.out = nc$dim$lat
  dim.time.out = nc$dim$time
  nc_close(nc)
  
  dim.time.in.adj = dim.time.out$len * 24
  data.out = array(0, dim = c(dim.lon.out$len, dim.lat.out$len, dim.time.out$len))
  
  if(dim.time.in$len != dim.time.in.adj){
    print(paste0("Input time (", dim.time.in$len, " hours) is not a 24 division of output time (", dim.time.in.adj, " hours), first file?"))
    diff = dim.time.in.adj - dim.time.in$len
    
    data.in.adj = array(0, dim = c(dim.lon.in$len, dim.lat.in$len, dim.time.in.adj))
    data.in.adj[,,(diff + 1):dim.time.in.adj] = data.in
    data.in.adj[,,1:(diff)] = data.in[,,(dim.time.in$len - diff + 1):dim.time.in$len] # fill with last hours
    
    data.in = data.in.adj
    rm(data.in.adj)
  }
  
  time.mapping = rep(1:dim.time.out$len, each = 24)  
  data.out = aggregate_weather(in_data = data.in,
                               out_data = data.out,
                               z_mapping = time.mapping)
  
  #image.plot(data.out[,,1])
  #image.plot(apply(X = data.in[,,1:24], MARGIN = c(1,2), FUN = mean))
  
  nc = nc_open(weather.file.out, write = T)
  ncvar_put(nc, nc$var[[1]], data.out)
  nc_close(nc)
}
