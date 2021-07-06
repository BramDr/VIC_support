library(ncdf4)
library(humidity)
rm(list = ls())

# Input
wfdei.file = "./Saves/weather_Arizona_WFDEI.RDS"
observed.file = "./Saves/weather_Arizona.RDS"
vic.out <- "../../../Data/VIC/Forcing/FACE/Arizona/forcing_daily.nc"
years = 1992:1997

point <- c(33.0628, -111.9826) # lat-lon

# Load
wfdei = readRDS(wfdei.file)
observed = readRDS(observed.file)

# Setup
dim.lon = ncdim_def(name = "lon", units = "degrees_east", vals = point[2])
dim.lat = ncdim_def(name = "lat", units = "degrees_north", vals = point[1])

# Calculate
year = years[1]
for(year in years){
  time = seq.Date(from = as.Date(paste0(year, "-01-01")), to = as.Date(paste0(year, "-12-31")), by = "day")
  dim.time = ncdim_def(name = "time", units = "days since 1970-01-01", vals = as.numeric(time), calendar = "standard")
  
  date.sel = match(time, observed$W_DATE)
  print(rle(is.na(date.sel)))
  tas = observed$TAVG[date.sel]
  swdown = observed$SRAD[date.sel]
  wind = observed$WIND[date.sel]
  pr = observed$RAIN[date.sel]
  vp = observed$VP[date.sel]
  
  date.sel = match(time, wfdei$date)
  lwdown = wfdei$lwdown[date.sel]
  psurf = wfdei$psurf[date.sel]
    
  vic.out.tmp = gsub(x = vic.out, pattern = ".nc", replacement = paste0("_", year, ".nc"))
  dir.create(dirname(vic.out.tmp))
  
  var.lwdown = ncvar_def(name = "lwdown", units = "W m-2", dim = list(dim.lon, dim.lat, dim.time), prec = "double")
  var.swdown = ncvar_def(name = "swdown", units = "W m-2", dim = list(dim.lon, dim.lat, dim.time), prec = "double")
  var.psurf = ncvar_def(name = "psurf", units = "kPa", dim = list(dim.lon, dim.lat, dim.time), prec = "double")
  var.tas = ncvar_def(name = "tas", units = "C", dim = list(dim.lon, dim.lat, dim.time), prec = "double")
  var.vp = ncvar_def(name = "vp", units = "kPa", dim = list(dim.lon, dim.lat, dim.time), prec = "double")
  var.pr = ncvar_def(name = "pr", units = "mm", dim = list(dim.lon, dim.lat, dim.time), prec = "double")
  var.wind = ncvar_def(name = "wind", units = "m s-1", dim = list(dim.lon, dim.lat, dim.time), prec = "double")
  
  nc = nc_create(filename = vic.out.tmp, 
                 vars = list(var.lwdown, var.swdown, var.psurf, var.tas, var.vp, var.pr, var.wind))
  ncvar_put(nc, var.lwdown, lwdown)
  ncvar_put(nc, var.swdown, swdown)
  ncvar_put(nc, var.psurf, psurf)
  ncvar_put(nc, var.tas, tas)
  ncvar_put(nc, var.vp, vp)
  ncvar_put(nc, var.pr, pr)
  ncvar_put(nc, var.wind, wind)
  nc_close(nc)
}
