library(ncdf4)
library(fields)
library(raster)
rm(list = ls())

# Input
vegatation.file <- "./Saves/vegetation_mapping.csv"
vars = c("overstory", "rarc", "rmin", "rgl", "sol.atten", "wind.atten", "trunk.ratio", "wind.h")
vars.names = c("overstory", "rarc", "rmin", "rgl", "sol_atten", "wind_atten", "trunk_ratio", "wind_h")
vars.2 = c("veg.rough", "displacement")
vars.names.2 = c("veg_rough", "displacement")
vars.3 = c("root.depth", "root.frac")
vars.names.3 = c("root_depth", "root_frac")
out.dir = "./Saves/"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
vegetation = read.csv(vegatation.file, stringsAsFactors = F)

# Calculate
maps = list()

for(var in vars){
  print(var)
  var.data = array(0, dim = c(length(out.lons), length(out.lats), length(na.omit(unique(vegetation$vic)))))
  var.count = array(0, dim = c(length(out.lons), length(out.lats), length(na.omit(unique(vegetation$vic)))))
  for(i in 1:nrow(vegetation)){
    if(is.na(vegetation$vic[i])){
      next
    }
    
    var.data[,,vegetation$vic[i]] = var.data[,,vegetation$vic[i]] + vegetation[i, var]
    var.count[,,vegetation$vic[i]] = var.count[,,vegetation$vic[i]] + 1
  }
  
  var.data[var.count > 1] = var.data[var.count > 1] / var.count[var.count > 1]
  maps[[var]] = var.data
}

for(var in vars.2){
  print(var)
  var.data = array(0, dim = c(length(out.lons), length(out.lats), length(na.omit(unique(vegetation$vic))), 12))
  var.count = array(0, dim = c(length(out.lons), length(out.lats), length(na.omit(unique(vegetation$vic))), 12))
  
  factor = 1
  if(var == "veg.rough"){
    factor = 0.123
  } else if (var == "displacement"){
    factor = 0.67
  }
  
  for(i in 1:nrow(vegetation)){
    if(is.na(vegetation$vic[i])){
      next
    }
    
    var.data[,,vegetation$vic[i],] = var.data[,,vegetation$vic[i],] + vegetation[i, "height"] * factor
    var.count[,,vegetation$vic[i],] = var.count[,,vegetation$vic[i],] + 1
  }
  
  var.data[var.count > 1] = var.data[var.count > 1] / var.count[var.count > 1]
  maps[[var]] = var.data
}

for(var in vars.3){
  print(var)
  var.data = array(0, dim = c(length(out.lons), length(out.lats), length(na.omit(unique(vegetation$vic))), 3))
  var.count = array(0, dim = c(length(out.lons), length(out.lats), length(na.omit(unique(vegetation$vic))), 3))
  
  for(i in 1:nrow(vegetation)){
    if(is.na(vegetation$vic[i])){
      next
    }
    
    var.data[,,vegetation$vic[i],1] = var.data[,,vegetation$vic[i],1] + vegetation[i, paste0(var, ".1")]
    var.data[,,vegetation$vic[i],2] = var.data[,,vegetation$vic[i],2] + vegetation[i, paste0(var, ".2")]
    var.count[,,vegetation$vic[i],] = var.count[,,vegetation$vic[i],] + 1
  }
  
  var.data[var.count > 1] = var.data[var.count > 1] / var.count[var.count > 1]
  maps[[var]] = var.data
}

for(var in vars){
  plot(maps[[var]][1,1,], main = var, type = "l")
}

# Save
for(i in 1:length(vars)){
  map = maps[[vars[i]]]
  
  out.file = paste0(out.dir, "/", vars.names[i], "_5min_Indus.RDS")
  dir.create(dirname(out.file))
  saveRDS(map, out.file)
}
for(i in 1:length(vars.2)){
  map = maps[[vars.2[i]]]
  
  out.file = paste0(out.dir, "/", vars.names.2[i], "_5min_Indus.RDS")
  dir.create(dirname(out.file))
  saveRDS(map, out.file)
}
for(i in 1:length(vars.3)){
  map = maps[[vars.3[i]]]
  
  out.file = paste0(out.dir, "/", vars.names.3[i], "_5min_Indus.RDS")
  dir.create(dirname(out.file))
  saveRDS(map, out.file)
}
