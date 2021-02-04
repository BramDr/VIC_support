rm(list = ls())

library(ncdf4)
library(fields)

generate.support.file <- "../../../Scripts/Support/generateFunctions.R"
weather.dir = "../../../Data/VIC/Forcing/Indus/"
weather.out = "../../../Data/VIC/Forcing/Indus_5min_WFDEI/"
variables = c("psurf", "tas", "swdown", "lwdown", "vp", "wind", "pr")

# Load
source(generate.support.file)

# Setup
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

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

# Calculate mapping
template.file = list.files(weather.dir, 
                           full.names = T, 
                           recursive = T, 
                           pattern = paste0(variables[1], "_6hourly"))[1]
nc = nc_open(template.file)
in.lons = nc$dim$lon$vals
in.lats = nc$dim$lat$vals
nc_close(nc)

mapping = array(NA, dim = c(length(out.lons), length(out.lats), 2))
for(x in 1:length(out.lons)){
  for(y in 1:length(out.lats)){
    map.x = which.min(abs(in.lons - out.lons[x]))
    map.y = which.min(abs(in.lats - out.lats[y]))
    mapping[x,y,] = c(map.x, map.y)
  }
}

# Disaggregate
variable = variables[1]
for(variable in variables){
  print(variable)
  
  weather.files = list.files(weather.dir, 
                             full.names = T, 
                             recursive = T, 
                             pattern = paste0(variable, "_6hourly"))
  
  weather.file = weather.files[1]
  for(weather.file in weather.files){
    print(basename(weather.file))
    
    nc = nc_open(weather.file)
    in.weather = ncvar_get(nc = nc, varid = nc$var[[1]])
    time = as.Date(nc$dim$time$vals, origin = "0000-12-30")
    
    dim.time = nc$dim$time
    var.variable = nc$var[[1]]
    nc_close(nc)
    
    out.weather = array(NA, dim = c(length(out.lons), 
                                    length(out.lats), 
                                    length(time)))
    for(x in 1:dim(out.weather)[1]){
      for(y in 1:dim(out.weather)[2]){
        map.x = mapping[x,y,1]
        map.y = mapping[x,y,2]
        out.weather[x,y,] = in.weather[map.x, map.y, ]
      }
    }
    #image.plot(out.weather[,,1])
    
    # Create
    weather.file.tmp = gsub(x = weather.file, 
                            pattern = weather.dir, 
                            replacement = weather.out)
    
    var.variable.tmp = cropyCreateVariable(base.variable = var.variable,
                                           dim = list(dim.lon, dim.lat, dim.time), 
                                           chunksizes = c(length(out.lons), length(out.lats), 1))
    
    dir.create(dirname(weather.file.tmp), recursive = T)
    nc = nc_create(filename = weather.file.tmp, 
                   vars = list(var.variable.tmp))
    nc_close(nc)
    
    # Save
    nc = nc_open(weather.file.tmp, write = T)
    ncvar_put(nc = nc, varid = nc$var[[1]], vals = out.weather)
    nc_close(nc)
  }
}
