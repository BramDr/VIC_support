rm(list = ls())

library(ncdf4)
library(fields)
library(ncdf4.helpers)

weather.dir.in = "../../../Data/Primary/Dahri2020/vic_in/Forcing_ERA5"
weather.dir.out = "./Saves/Setup_daily/"
weather.dir.tmp = "./Saves/Disaggregated_daily/"
timestep = 24
variable.merge = data.frame(Dahri = c("PREC", "PRESSURE", "TMIN", "TMAX", "SHORTWAVE", "LONGWAVE", "WIND", "QAIR"),
                            VIC = c("pr", "psurf", "tasmin", "tasmax", "swdown", "lwdown", "wind", "qair"),
                            stringsAsFactors = F)

# Load
in.files = list.files(weather.dir.in, full.names = T, recursive = T)
out.files = list.files(weather.dir.out, full.names = T, recursive = T)
tmp.files = list.files(weather.dir.tmp, pattern = "Dahri", full.names = T, recursive = T)

#for(tmp.file in tmp.files){
#  file.remove(tmp.file)
#}

# Setup
in.weather = read.table(in.files[1])
colnames(in.weather) = c("PREC", "TMAX", "TMIN", "WIND", "SHORTWAVE", "LONGWAVE", "QAIR", "PRESSURE")

in.time = seq(from = as.Date("1979-01-01"), by = "day", length.out = nrow(in.weather))
in.years = as.numeric(format(in.time, "%Y"))
in.varnames = colnames(in.weather)

nc = nc_open(out.files[1])
out.lons = nc$dim$lon$vals
out.lats = nc$dim$lat$vals
nc_close(nc)

in.year = in.years[1]
in.year = 1981
for (in.year in unique(in.years)) {
  print(in.year)
  
  tmp.file = paste0(weather.dir.tmp, "/all_daily_Dahri/all_daily_Dahri_", in.year)  
  if(file.exists(tmp.file)){
    next
  }
  print(basename(tmp.file))
  
  out.maps = list()
  for (in.varname in in.varnames) {
  
    out.varname = variable.merge$VIC[!is.na(variable.merge$Dahri) & variable.merge$Dahri == in.varname]
    if(length(out.varname) == 0){
      print(paste0("Could not merge Dahri variable name ", in.varname))
      next
    }
      
    out.maps[[in.varname]] = array(NA, dim = c(length(out.lons), length(out.lats), sum(in.years == in.year)))
  } 
  
  in.file = in.files[1]
  for (in.file in in.files) {
    print(basename(in.file))
    
    in.lon = gsub(x = basename(in.file), pattern = ".*_", replacement = "")
    in.lon = gsub(x = in.lon, pattern = ".RDS", replacement = "")
    in.lat = gsub(x = basename(in.file), pattern = paste0("_", in.lon), replacement = "")
    in.lat = gsub(x = in.lat, pattern = paste0("forcing_"), replacement = "")
    in.lon = as.numeric(in.lon)
    in.lat = as.numeric(in.lat)
    
    x = which.min(abs(in.lon - out.lons))
    y = which.min(abs(in.lat - out.lats))
  
    in.weather = read.table(in.file)[in.years == in.year,]
    colnames(in.weather) = c("PREC", "TMAX", "TMIN", "WIND", "SHORTWAVE", "LONGWAVE", "QAIR", "PRESSURE")
    
    for (in.varname in in.varnames) {
    
      out.varname = variable.merge$VIC[!is.na(variable.merge$Dahri) & variable.merge$Dahri == in.varname]
      if(length(out.varname) == 0){
        print(paste0("Could not merge Dahri variable name ", in.varname))
        next
      }
      
      out.map = out.maps[[in.varname]]
      out.map[x,y,] = in.weather[, in.varname]
      out.maps[[in.varname]] = out.map
    } 
  }
    
  # Save
  dir.create(dirname(tmp.file), recursive = T)
  saveRDS(out.maps, tmp.file)
}
