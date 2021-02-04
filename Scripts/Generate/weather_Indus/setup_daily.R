rm(list = ls())

library(ncdf4)
library(PCICt)

weather.out = "./Saves/Setup_daily/"
timestep = 24
variables = c("psurf", "tas", "swdown", "lwdown", "vp", "wind100", "wind10", "pr", "tdew", "uwind100", "vwind100", "uwind10", "vwind10", "tasmin", "tasmax", "qair")
units = c("kPa", "degrees_celsius", "W m-2", "W m-2", "kPa", "m s-1", "m s-1", "mm", "degrees_celsius", "m s-1", "m s-1", "m s-1", "m s-1", "degrees_celsius", "degrees_celsius", "kg kg-1")
longname = c(paste0("Average of hourly instantaneous surface atmospheric pressure (average over the previous ", timestep, " hours)"), 
             paste0("Average of hourly instantaneous 2m above surface air temperature (average over the previous ", timestep, " hours)"), 
             paste0("Rate of hourly accumulated surface downward longwave radiation (rate over the previous ", timestep, " hours)"),
             paste0("Rate of hourly accumulated surface downward shortwave radiation (rate over the previous ", timestep, " hours)"),
             paste0("Average of hourly instantaneous 2m above surface vapour pressure (average over the previous ", timestep, " hours)"),
             paste0("Average of hourly instantaneous 100m above surface wind speed (average over the previous ", timestep, " hours)"),
             paste0("Average of hourly instantaneous 10m above surface wind speed (average over the previous ", timestep, " hours)"),
             paste0("Sum of hourly accumulated surface precipitation (sum over the previous ", timestep, " hours)"), 
             paste0("Average of hourly instantaneous 2m above surface dewpoint air temperature (average over the previous ", timestep, " hours)"),
             paste0("Average of hourly instantaneous 100m above surface wind speed eastward component (average over the previous ", timestep, " hours)"),
             paste0("Average of hourly instantaneous 100m above surface wind speed northward component (average over the previous ", timestep, " hours)"),
             paste0("Average of hourly instantaneous 10m above surface wind speed eastward component (average over the previous ", timestep, " hours)"),
             paste0("Average of hourly instantaneous 10m above surface wind speed northward component (average over the previous ", timestep, " hours)"), 
             paste0("Minimum of hourly instantaneous 2m above surface air temperature (minimum over the previous ", timestep, " hours)"), 
             paste0("Maximum of hourly instantaneous 2m above surface air temperature (maximum over the previous ", timestep, " hours)"),
             paste0("Average of hourly instantaneous 2m above surface specific humidity (average over the previous ", timestep, " hours)"))

# Setup
years = 1979:2018
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Calculate
year = years[1]
for(year in years){
  print(year)
  
  # For the reanalysis, the accumulation period is over the 1 hour up to the validity date and time
  year.start = as.POSIXct(paste0(year, "-01-01 ", "00:00:00"))
  year.end = as.POSIXct(paste0(year, "-12-31 ", 24 - timestep, ":00:00"))
  out.time = seq(from = year.start, to = year.end, by = paste0(timestep, " hour"))
  
  # Create
  dim.lon <- ncdim_def(
    name = "lon",
    units = "degrees_east",
    vals = out.lons,
    longname = "longitude of grid cell center"
  )
  dim.lat <- ncdim_def(
    name = "lat",
    units = "degrees_north",
    vals = out.lats,
    longname = "latitude of grid cell center"
  )
  dim.time = ncdim_def(
    name = "time",
    units = paste0("seconds since 1970-01-01 01:00:00"),
    vals = as.numeric(out.time),
    calendar = "standard"
  )
  
  variable = variables[1]
  for(variable in variables) {
    print(variable)
    z = which(variables == variable)
    
    weather.out.tmp = paste0(weather.out, "/", variable, "_", "daily_ERA5/", variable, "_", "daily_ERA5_", year, ".nc")
    if(file.exists(weather.out.tmp)){
      next
    }
    
    var.variable = ncvar_def(
      name = variable, 
      units = units[z], 
      dim = list(dim.lon, dim.lat, dim.time), 
      longname = longname[z], 
      prec = "float", 
      compression = 2, 
      missval = 1e20,
      chunksizes = c(length(out.lons), length(out.lats), 1)
    )
    
    dir.create(dirname(weather.out.tmp), recursive = T)
    nc = nc_create(filename = weather.out.tmp, 
                   vars = list(var.variable))
    ncvar_put(nc, var.variable, vals = array(NA, dim = c(length(out.lons), length(out.lats), length(out.time))))
    nc_close(nc)
  }
}
