library(ncdf4)
library(humidity)
rm(list = ls())

# Input
weather.dir = "./Saves_point"
vic.out <- "../../../../Data/VIC/Forcing/FACE/forcing_daily_WFDEI_point.nc"
years = 1992:1997

point <- c(33.0628, -111.9826) # lat-lon

# Load
weather.files = list.files(weather.dir, full.names = T)

# Setup
dim.lon = ncdim_def(name = "lon", units = "degrees_east", vals = point[2])
dim.lat = ncdim_def(name = "lat", units = "degrees_north", vals = point[1])

# Calculate
year = years[1]
for(year in years){
  
  lwdown.file = grep(x = weather.files, pattern = paste0("LWdown.*", year, ".nc"), value = T)
  swdown.file = grep(x = weather.files, pattern = paste0("SWdown.*", year, ".nc"), value = T)
  psurf.file = grep(x = weather.files, pattern = paste0("PSurf.*", year, ".nc"), value = T)
  tas.file = grep(x = weather.files, pattern = paste0("Tair.*", year, ".nc"), value = T)
  wind.file = grep(x = weather.files, pattern = paste0("Wind.*", year, ".nc"), value = T)
  rainf.file = grep(x = weather.files, pattern = paste0("Rainf.*", year, ".nc"), value = T)
  snowf.file = grep(x = weather.files, pattern = paste0("Snowf.*", year, ".nc"), value = T)
  qair.file = grep(x = weather.files, pattern = paste0("Qair.*", year, ".nc"), value = T)
  
  nc = nc_open(lwdown.file)
  lwdown = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  nc = nc_open(swdown.file)
  swdown = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  nc = nc_open(psurf.file)
  psurf = ncvar_get(nc, nc$var[[1]]) * 1e-3
  nc_close(nc)
  nc = nc_open(tas.file)
  tas = ncvar_get(nc, nc$var[[1]]) - 273.15
  nc_close(nc)
  nc = nc_open(wind.file)
  wind = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  nc = nc_open(rainf.file)
  rainf = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  nc = nc_open(snowf.file)
  snowf = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  pr = (rainf + snowf) * 60 * 60 * 24
  nc = nc_open(qair.file)
  qair = ncvar_get(nc, nc$var[[1]])
  nc_close(nc)
  vp = WVP2(SH2RH(qair, tas + 273.15, psurf * 1e3), SVP(tas + 273.15)) * 1e-3
  
  vic.out.tmp = gsub(x = vic.out, pattern = ".nc", replacement = paste0("_", year, ".nc"))
  dir.create(dirname(vic.out.tmp))
  
  time = seq.Date(from = as.Date(paste0(year, "-01-01")), to = as.Date(paste0(year, "-12-31")), by = "day")
  dim.time = ncdim_def(name = "time", units = "days since 1970-01-01", vals = as.numeric(time), calendar = "standard")
  
  var.lwdown = ncvar_def(name = "lwdown", units = "W m-2", dim = list(dim.lon, dim.lat, dim.time))
  var.swdown = ncvar_def(name = "swdown", units = "W m-2", dim = list(dim.lon, dim.lat, dim.time))
  var.psurf = ncvar_def(name = "psurf", units = "kPa", dim = list(dim.lon, dim.lat, dim.time))
  var.tas = ncvar_def(name = "tas", units = "C", dim = list(dim.lon, dim.lat, dim.time))
  var.vp = ncvar_def(name = "vp", units = "kPa", dim = list(dim.lon, dim.lat, dim.time))
  var.pr = ncvar_def(name = "pr", units = "mm", dim = list(dim.lon, dim.lat, dim.time))
  var.wind = ncvar_def(name = "wind", units = "m s-1", dim = list(dim.lon, dim.lat, dim.time))
  
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
