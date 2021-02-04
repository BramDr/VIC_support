rm(list = ls())

library(ncdf4)
library(fields)
library(ncdf4.helpers)

weather.dir.in = "../../../Data/VIC_4.1.2/Output/Shigr_5min/"
weather.dir.out = "./Saves/Setup_6hourly/"
weather.dir.tmp = "./Saves/Combined_6hourly/"
timestep = 6
flux.variables = c("OUT_EVAP", "OUT_SNOW_MELT", "OUT_RUNOFF", "OUT_BASEFLOW", "OUT_SNOW_COVER", "OUT_SNOW_DEPTH", "OUT_SWE", "OUT_AIR_TEMP", "OUT_SHORTWAVE", "OUT_LONGWAVE", "OUT_PRESSURE", "OUT_VP", "OUT_WIND", "OUT_PREC")
variable.merge = data.frame(Dahri = c("OUT_AIR_TEMP", "OUT_SHORTWAVE", "OUT_LONGWAVE", "OUT_PRESSURE", "OUT_VP", "OUT_WIND", "OUT_PREC"),
                            VIC = c("tas", "swdown", "lwdown", "psurf", "vp", "wind", "pr"),
                            factor = c(1,1,1,1,1,1,1),
                            stringsAsFactors = F)

# Load
in.files = list.files(weather.dir.in, full.names = T, recursive = T)
out.files = list.files(weather.dir.out, full.names = T, recursive = T)
tmp.files = list.files(weather.dir.tmp, pattern = "Dahri", full.names = T, recursive = T)

#for(tmp.file in tmp.files){
#  file.remove(tmp.file)
#}

# Check input
in.weather = read.table(in.files[1])
in.time = as.POSIXct(paste0(in.weather[,1], "-", in.weather[,2], "-", in.weather[,3], " ", in.weather[,4], ":00:00"))
in.years = as.numeric(format.Date(in.time, "%Y"))

# Disaggregate
out.file = out.files[41]
for(out.file in out.files){
  tmp.file = gsub(x = out.file, pattern = weather.dir.out, replacement = weather.dir.tmp)
  tmp.file = gsub(x = tmp.file, pattern = "ERA5", replacement = "Dahri")
  if(file.exists(tmp.file)){
    next
  }
  
  # Check output
  nc = nc_open(out.file)
  out.lons = nc$dim$lon$vals
  out.lats = nc$dim$lat$vals
  out.time = as.POSIXct(nc.get.time.series(nc))
  out.varname = nc$var[[1]]$name
  nc_close(nc)
  out.year = as.numeric(format.Date(out.time, "%Y"))[1]
  
  if(!(out.year %in% in.years)){
    next
  }
  
  in.varname = variable.merge$Dahri[variable.merge$VIC == out.varname]
  in.factor = variable.merge$factor[variable.merge$VIC == out.varname]
  if(length(in.varname) == 0){
    print(paste0("Could not merge Dahri variable name ", out.varname))
    next
  }
  if(!(in.varname %in% flux.variables)){
    next
  }
  
  # Get output
  nc = nc_open(out.file)
  out.weather = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  
  # Prepare temporary
  dir.create(dirname(tmp.file), recursive = T)
  file.copy(out.file, tmp.file)
  print(basename(tmp.file))
  
  in.file = in.files[1]
  for(in.file in in.files){
    print(basename(in.file))
    
    in.lon = gsub(x = basename(in.file), pattern = ".*_", replacement = "")
    in.lat = gsub(x = basename(in.file), pattern = paste0("_", in.lon), replacement = "")
    in.lat = gsub(x = in.lat, pattern = paste0("fluxes_"), replacement = "")
    in.lon = as.numeric(in.lon)
    in.lat = as.numeric(in.lat)
    
    x = which.min(abs(in.lon - out.lons))
    y = which.min(abs(in.lat - out.lats))
    
    in.weather = read.table(in.file)
    colnames(in.weather) = c("year", "month", "day", "hour", flux.variables)
    
    in.weather.tmp = in.weather[in.years == out.year, in.varname] 
    out.weather[x,y,] = in.weather.tmp
  }  
    
  # Save
  nc = nc_open(tmp.file, write = T)
  ncvar_put(nc, nc$var[[1]], out.weather)
  nc_close(nc)
}
