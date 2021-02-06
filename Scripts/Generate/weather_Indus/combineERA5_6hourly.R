rm(list = ls())

library(ncdf4)
library(fields)
library(ncdf4.helpers)
library(abind)

weather.dir.in = "./Saves/Disaggregated_6hourly/"
weather.dir.out = "./Saves/Setup_6hourly/"
weather.dir.tmp = "./Saves/Combined_6hourly/"

# Load
in.files = list.files(weather.dir.in, pattern = "ERA5", full.names = T, recursive = T)
out.files = list.files(weather.dir.out, full.names = T, recursive = T)
tmp.files = list.files(weather.dir.tmp, pattern = "ERA5", full.names = T, recursive = T)

#for(tmp.file in tmp.files){
#  file.remove(tmp.file)
#}

# Disaggregate
out.file = out.files[1]
for(out.file in out.files){
  tmp.file = gsub(x = out.file, pattern = weather.dir.out, replacement = weather.dir.tmp)
  
  if(file.exists(tmp.file)){
    next
  }
  
  nc = nc_open(out.file)
  out.time = nc.get.time.series(nc)
  out.varname = nc$var[[1]]$name
  nc_close(nc)
  
  out.year = as.numeric(unique(format(out.time, "%Y")))
  
  # Get input
  in.spinup.file = grep(x = in.files, pattern = paste0("/", out.varname, "_.*_", out.year, ".spinup"), value = T)
  in.this.file = grep(x = in.files, pattern = paste0("/", out.varname, "_.*_", out.year, ".", out.year), value = T)
  in.prev.file = grep(x = in.files, pattern = paste0("/", out.varname, "_.*_", out.year, ".", out.year - 1), value = T)  
  if(length(in.this.file) == 0){
    next
  }
  
  in.this = readRDS(in.this.file)
  if(length(in.spinup.file) > 0){
    in.spinup = readRDS(in.spinup.file)
  }
  if(length(in.prev.file) > 0){
    in.prev = readRDS(in.prev.file)
  }
  
  # Combine
  in.data = in.this
  if(exists("in.prev")){
    in.data.tmp = in.data[,,1:dim(in.prev)[3], drop = F]
    in.data.tmp = in.data.tmp + in.prev
    in.data[,,1:dim(in.prev)[3]] = in.data.tmp
    rm(in.prev)
  }
  if(exists("in.spinup")){
    add.size = (length(out.time) - dim(in.data)[3])
    copy.size = dim(in.spinup)[3] - add.size
    
    if(add.size > 0) {
      in.data = abind(in.spinup[,,1:add.size], in.data, along = 3)
    }
    if(copy.size > 0){
      in.data[,,(add.size + 1):(add.size + copy.size)] = in.spinup[,,(add.size + copy.size)]
    }
    rm(in.spinup)
  }
  rm(in.this)
  
  # Create
  dir.create(dirname(tmp.file), recursive = T)
  file.copy(out.file, tmp.file)
  print(basename(tmp.file))
  
  # Save
  out.z.start = 1
  out.z.count = dim(in.data)[3]
  nc = nc_open(tmp.file, write = T)
  ncvar_put(nc, nc$var[[1]],
            vals = in.data,
            start = c(1,1,out.z.start),
            count = c(-1,-1,out.z.count))
  nc_close(nc)
}
