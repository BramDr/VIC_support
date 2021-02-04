rm(list = ls())

library(ncdf4)
library(fields)
library(Rcpp)
library(ncdf4.helpers)

rcpp.support.file <- "./Rcpp_support.cpp"
weather.dir.in = "../../../Data/Primary/HIAWARE/"
weather.dir.out = "./Saves/Setup_daily/"
weather.dir.tmp = "./Saves/Disaggregated_daily/"
timestep = 24
variable.merge = data.frame(HIAWARE = c("pr", "lwra", "swra", "tas", "tasmin", "tasmax"),
                            VIC = c("pr", "lwdown", "swdown", "tas", "tasmin", "tasmax"),
                            factor = c(24 / timestep,24 / timestep,24 / timestep,24 / timestep,24 / timestep,24 / timestep),
                            offset = c(0,0,0,0,0,0),
                            units = c("mm", "W m-2", "W m-2", "degrees_celsius", "degrees_celsius", "degrees_celsius"),
                            stringsAsFactors = F)

# Load
sourceCpp(rcpp.support.file)

in.files = list.files(weather.dir.in, full.names = T, recursive = T)
out.files = list.files(weather.dir.out, full.names = T, recursive = T)
tmp.files = list.files(weather.dir.tmp, pattern = "HIAWARE", full.names = T, recursive = T)

#for(tmp.file in tmp.files){
#  file.remove(tmp.file)
#}

# Disaggregate
in.file = in.files[1]
for(in.file in in.files){
  print(basename(in.file))
  
  nc = nc_open(in.file)
  in.lons = nc$dim$lon$vals
  in.lats = nc$dim$lat$vals
  in.varnames = names(nc$var)
  nc_close(nc)
  
  in.year = gsub(x = basename(in.file), pattern = ".*_", replacement= "")
  in.year = gsub(x = in.year, pattern = ".nc", replacement= "")
  in.year = as.numeric(in.year)
  in.time = seq(from = as.Date(paste0(in.year, "-01-01")), 
                to = as.Date(paste0(in.year, "-12-31")), 
                by = "day")
  
  in.lon.res = abs(in.lons[2] - in.lons[1])
  in.lat.res = abs(in.lats[2] - in.lats[1])
  in.time.res = abs(in.time[2] - in.time[1])
  
  in.years = as.numeric(format(in.time, "%Y"))
  in.year = in.years[1]
  for(in.year in unique(in.years)){
    print(in.year)
    
    in.time.sel = which(in.years == in.year)
    in.time.year = in.time[in.time.sel]
    
    in.varname = in.varnames[1]
    for(in.varname in in.varnames) {
      print(in.varname)
    
      out.varname = variable.merge$VIC[!is.na(variable.merge$HIAWARE) & variable.merge$HIAWARE == in.varname]
      out.factor = variable.merge$factor[!is.na(variable.merge$HIAWARE) & variable.merge$HIAWARE == in.varname]
      out.offset = variable.merge$offset[!is.na(variable.merge$HIAWARE) & variable.merge$HIAWARE == in.varname]
      if(length(out.varname) == 0){
        print(paste0("Could not merge HIAWARE variable name ", in.varname))
        next
      }
      
      out.year = in.year
        
      out.file = grep(x = out.files, pattern = paste0(".*", out.varname, "_.*", out.year), value = T)
      tmp.file = gsub(x = out.file, pattern = weather.dir.out, replacement = weather.dir.tmp)
      tmp.file = gsub(x = tmp.file, pattern = "ERA5", replacement = "HIAWARE")
      if(length(out.file) == 0){
        print(paste0("Could not find VIC forcing for variable name ", out.varname, " and year ", out.year))
        next
      }
      
      if(file.exists(tmp.file)){
        next
      }
      
      nc = nc_open(out.file)
      out.lons = nc$dim$lon$vals
      out.lats = nc$dim$lat$vals
      out.time = nc.get.time.series(nc)
      out.varname = nc$var[[1]]$name
      nc_close(nc)
      
      out.lon.res = abs(out.lons[2] - out.lons[1])
      out.lat.res = abs(out.lats[2] - out.lats[1])
      out.time.res = abs(out.time[2] - out.time[1])
      
      # Mapping
      mapping.x = rep(NA, length(out.lons))
      mapping.y = rep(NA, length(out.lats))
      mapping.z = rep(NA, length(in.time.year))
      for(x in 1:length(out.lons)){
        x.dist = abs(in.lons - out.lons[x])
        if(min(x.dist) > in.lon.res / 2){
          # DO NOT skip if the cell is too far from the output cell
          # domains are different, take the nearest cell
          # next
        }
        mapping.x[x] = which.min(x.dist)
      }
      for(y in 1:length(out.lats)){
        y.dist = abs(in.lats - out.lats[y])
        if(min(y.dist) > in.lat.res / 2){
          # DO NOT skip if the cell is too far from the output cell
          # domains are different, take the nearest cell
          # next
        }
        mapping.y[y] = which.min(y.dist)
      }
      mapping.z = 1:length(in.time.year)
      
      if(length(na.omit(mapping.z)) == 0){
        print(paste0("No HIAWARE data for year ", in.year, " in VIC forcing year ", out.year))
        next
      }
      
      # Get input
      nc = nc_open(in.file)
      in.weather = ncvar_get(nc = nc, varid = in.varname,
                            collapse_degen = F)
      nc_close(nc)
      
      in.weather = (in.weather + out.offset) / out.factor
      
      # Get output
      nc = nc_open(out.file)
      out.weather = ncvar_get(nc = nc, varid = out.varname,
                            collapse_degen = F)
      nc_close(nc)
      
      out.weather[is.na(out.weather)] = 0
      
      # Apply mapping
      out.weather = aggregate_weather(in_data = in.weather,
                                      out_data = out.weather,
                                      x_mapping = mapping.x,
                                      y_mapping = mapping.y,
                                      z_mapping = mapping.z)
      
      # Check
      if( out.varname %in% c("pr")) {
        # Check for accumulated variables
        in.weather.sum = apply(X = in.weather, MARGIN = c(1,2), FUN = sum, na.rm = T)
        out.weather.sum = apply(X = out.weather, MARGIN = c(1,2), FUN = sum, na.rm = T)
        image.plot(in.weather.sum, main = paste0("in: ", in.varname))
        image.plot(out.weather.sum, main = paste0("out: ", out.varname))
      } else {
        # Check for instantanious variables
        in.weather.sum = apply(X = in.weather, MARGIN = c(1,2), FUN = mean, na.rm = T)
        out.weather.sum = apply(X = out.weather, MARGIN = c(1,2), FUN = mean, na.rm = T)
        image.plot(in.weather.sum, main = paste0("in: ", in.varname))
        image.plot(out.weather.sum, main = paste0("out: ", out.varname))
      }
      
      # Save
      dir.create(dirname(tmp.file), recursive = T)
      file.copy(out.file, tmp.file)
      print(basename(tmp.file))
      
      nc = nc_open(tmp.file, write = T)
      ncvar_put(nc, out.varname, out.weather)
      nc_close(nc)
    }
  }
}
