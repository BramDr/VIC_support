rm(list = ls())

library(ncdf4)
library(PCICt)

weather.in = "../hourly/"
weather.out = "./"

# Load
weather.files = list.files(weather.in, recursive = T, pattern = ".nc$", full.names = T)

# Calculate
weather.file = weather.files[1]
for(weather.file in weather.files){
  print(basename(weather.file))
  
  weather.file.out = gsub(x = weather.file, pattern = weather.in, replacement = weather.out)
  weather.file.out = gsub(x = weather.file.out, pattern = "_hourly_", replacement = "_daily_")
  if(file.exists(weather.file.out)){
    next
  }
  
  nc = nc_open(weather.file)
  lon.dim = nc$dim$lon
  lat.dim = nc$dim$lat
  var = nc$var[[1]]
  nc_close(nc)
  if(!("time" %in% names(nc$dim))) {
    print("No time dimension present")
    next
  }
  
  patterns = strsplit(basename(weather.file), "_")[[1]]
  varname = patterns[1]
  modelname = patterns[2]
  corrname = patterns[3]
  year.month = patterns[4]
  
  year = as.numeric(substring(year.month, 1, 4))
  month = as.numeric(substring(year.month, 5, 6))
  year.next = year
  month.next = month + 1
  if(month.next > 12){
    month.next = month.next - 12
    year.next = year.next + 1
  }
  times = seq.Date(from = as.Date(paste0(year, "-", month, "-", "01")),
                   to = as.Date(paste0(year.next, "-", month.next, "-", "01")),
                   by = "day")
  times = times[1:(length(times) - 1)]

  time.dim.out = ncdim_def(name = "time",
                       units = "days since 1970-01-01 00:00:00",
                       vals = as.numeric(times),
                       longname = "Time",
                       calendar = "standard",
                       unlim = T)
  var.out = ncvar_def(name = var$name,
                      units = var$units,
                      dim = list(lon.dim, lat.dim, time.dim.out),
                      longname = var$longname,
                      missval = var$missval,
                      compression = 3,
                      chunksizes = c(lon.dim$len, lat.dim$len, 1))
  
  dir.create(dirname(weather.file.out))
  nc = nc_create(weather.file.out, list(var.out))
  nc_close(nc)
}
