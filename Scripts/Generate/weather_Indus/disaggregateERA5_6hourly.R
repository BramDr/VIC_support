rm(list = ls())

library(ncdf4)
library(fields)
library(Rcpp)
library(ncdf4.helpers)

rcpp.support.file <- "./Rcpp_support.cpp"
weather.dir.in = "../../../Data/Primary/ERA5/"
weather.dir.out = "./Saves/Setup_6hourly/"
weather.dir.tmp = "./Saves/Disaggregated_6hourly/"
timestep = 6
variable.merge = data.frame(ERA5 = c("tp", "sp", "t2m", "ssrd", "strd", "d2m", "u100", "v100", "u10", "v10"),
                            VIC = c("pr", "psurf", "tas", "swdown", "lwdown", "tdew", "uwind100", "vwind100", "uwind10", "vwind10"),
                            factor = c(1 * 1e3, 1 / timestep * 1e-3, 1 / timestep, 1 / timestep * 1 / 3600, 1 / timestep * 1 / 3600, 1 / timestep, 1 / timestep, 1 / timestep, 1 / timestep, 1 / timestep),
                            offset = c(0, 0, -273.15, 0, 0, -273.15, 0, 0, 0, 0),
                            stringsAsFactors = F)

# Load
sourceCpp(rcpp.support.file)

in.files = list.files(weather.dir.in, full.names = T, recursive = T)
out.files = list.files(weather.dir.out, full.names = T, recursive = T)
tmp.files = list.files(weather.dir.tmp, pattern = "ERA5", full.names = T, recursive = T)

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
  in.time = nc.get.time.series(nc)
  in.varnames = names(nc$var)
  nc_close(nc)
  
  in.lon.res = abs(in.lons[2] - in.lons[1])
  in.lat.res = abs(in.lats[2] - in.lats[1])
  in.time.res = abs(in.time[2] - in.time[1])
  
  in.years = as.numeric(format(in.time, "%Y"))
  in.year = in.years[1]
  for(in.year in unique(in.years)){
    print(in.year)
    
    in.varname = in.varnames[7]
    for(in.varname in in.varnames) {
      print(in.varname)
    
      out.varname = variable.merge$VIC[!is.na(variable.merge$ERA5) & variable.merge$ERA5 == in.varname]
      out.factor = variable.merge$factor[!is.na(variable.merge$ERA5) & variable.merge$ERA5 == in.varname]
      out.offset = variable.merge$offset[!is.na(variable.merge$ERA5) & variable.merge$ERA5 == in.varname]
      if(length(out.varname) == 0){
        print(paste0("Could not merge ERA5 variable name ", in.varname))
        next
      }
      
      out.year = in.year
      for(out.year in in.year + c(0, 1)) {
        if(out.year > 2018) {
          next
        }
        
        print(paste0("in.year ", in.year, " -> out.year ", out.year))
        
        out.file = grep(x = out.files, pattern = paste0(".*/", out.varname, "_.*", out.year), value = T)
        tmp.file = gsub(x = out.file, pattern = weather.dir.out, replacement = weather.dir.tmp)
        tmp.file = gsub(x = tmp.file, pattern = paste0(out.varname, "_6hourly"), replacement = paste0(out.varname, "_6hourly_ERA5"))
        tmp.file = gsub(x = tmp.file, pattern = ".nc", replacement = paste0(".", in.year, ".RDS"))
        if(length(out.file) == 0){
          print(paste0("Could not find VIC forcing for variable name ", out.varname, " and year ", out.year))
          next
        }
        
        if(file.exists(tmp.file)){
          next
        }
        dir.create(dirname(tmp.file), recursive = T)
        print(basename(tmp.file))
        
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
        mapping.z = rep(NA, length(in.time))
        for(x in 1:length(out.lons)){
          x.dist = abs(in.lons - out.lons[x])
          if(min(x.dist) > in.lon.res / 2){
            # skip if the cell is too far from the output cell
            next
          }
          mapping.x[x] = which.min(x.dist)
        }
        for(y in 1:length(out.lats)){
          y.dist = abs(in.lats - out.lats[y])
          if(min(y.dist) > in.lat.res / 2){
            # skip if the cell is too far from the output cell
            next
          }
          mapping.y[y] = which.min(y.dist)
        }
        for(z in 1:length(in.time)){
          z.sel = which(out.time >= in.time[z])
          if(length(z.sel) == 0){
            # skip if the time exceeds the output time
            next
          }
          z.dist = abs(out.time[min(z.sel)] - in.time[z])
          if(min(z.dist) >= out.time.res){
            # skip if the time preceeds the output time
            next
          }
          mapping.z[z] = min(z.sel)
        }
        
        mapping.z[1:25]
        mapping.z[(length(mapping.z) - 25):length(mapping.z)]
        
        if(length(na.omit(mapping.z)) == 0){
          print(paste0("No ERA5 data for year ", in.year, " in VIC forcing year ", out.year))
          next
        }
        
        # Get input
        in.z.start = min(which(!is.na(mapping.z)))
        in.z.count = sum(!is.na(mapping.z))
        nc = nc_open(in.file)
        in.weather = ncvar_get(nc = nc, varid = in.varname,
                              start = c(1,1,in.z.start),
                              count = c(-1,-1,in.z.count), 
                              collapse_degen = F)
        nc_close(nc)
        
        in.weather = (in.weather + out.offset) * out.factor
        
        # Get output
        out.z.start = min(mapping.z, na.rm = T)
        out.z.count = length(na.omit(unique(mapping.z)))
        nc = nc_open(out.file)
        out.weather = ncvar_get(nc = nc, varid = out.varname,
                              start = c(1,1,out.z.start),
                              count = c(-1,-1,out.z.count), 
                              collapse_degen = F)
        nc_close(nc)
        
        out.weather[is.na(out.weather)] = 0
        
        # Apply mapping
        mapping.z.adj = mapping.z[!is.na(mapping.z)] - out.z.start + 1
        out.weather = aggregate_weather(in_data = in.weather,
                                        out_data = out.weather,
                                        x_mapping = mapping.x,
                                        y_mapping = mapping.y,
                                        z_mapping = mapping.z.adj)
        
        # Check
        if( out.varname %in% c("pr", "swdown", "lwdown")) {
          # Check for accumulated variables
          in.weather.sum = apply(X = in.weather, MARGIN = c(1,2), FUN = sum, na.rm = T)
          out.weather.sum = apply(X = out.weather, MARGIN = c(1,2), FUN = sum, na.rm = T)
          image.plot(in.weather.sum, main = paste0("in: ", in.varname, " ", in.year, " -> ", out.year))
          image.plot(out.weather.sum, main = paste0("out: ", out.varname, " ", in.year, " -> ", out.year))
        } else {
          # Check for instantanious variables
          in.weather.sum = apply(X = in.weather / out.factor, MARGIN = c(1,2), FUN = mean, na.rm = T)
          out.weather.sum = apply(X = out.weather, MARGIN = c(1,2), FUN = mean, na.rm = T)
          image.plot(in.weather.sum, main = paste0("in: ", in.varname, " ", in.year, " -> ", out.year))
          image.plot(out.weather.sum, main = paste0("out: ", out.varname, " ", in.year, " -> ", out.year))
        }
        
        # Save
        saveRDS(out.weather, tmp.file)
        
        if(in.year == 1979 && out.year == 1979) {
          if( out.varname %in% c("pr", "swdown", "lwdown")) {
            # Spinup for accumulated variables
            out.file = grep(x = out.files, pattern = paste0(".*", out.varname, "_.*", in.year), value = T)
            tmp.file = gsub(x = out.file, pattern = weather.dir.out, replacement = weather.dir.tmp)
            tmp.file = gsub(x = tmp.file, pattern = paste0(out.varname, "_6hourly"), replacement = paste0(out.varname, "_6hourly_ERA5"))
            tmp.file = gsub(x = tmp.file, pattern = ".nc", replacement = paste0(".spinup", ".RDS"))
            
            if(file.exists(tmp.file)){
              next
            }
            dir.create(dirname(tmp.file), recursive = T)
            print(basename(tmp.file))
            
            # Fill first 6 hours of the first day, with first 6 hours of last day
            missing.start = as.POSIXct(paste0("1979-01-01 ", "00:00:00"), tz = "GMT")
            missing.end = as.POSIXct(paste0("1979-01-01 ", "06:00:00"), tz = "GMT")
            missing.z = 1:min(which(as.POSIXct(out.time) >= missing.end))
            fill.start = as.POSIXct(paste0("1979-12-31 ", "00:00:00"), tz = "GMT")
            fill.end = as.POSIXct(paste0("1979-12-31 ", "06:00:00"), tz = "GMT")
            fill.z = which(as.POSIXct(out.time) >= fill.start & as.POSIXct(out.time) <= fill.end)
            
            # Get fill data
            fill.weather = array(NA, dim = c(dim(out.weather)[1:2], length(missing.z)))
            fill.weather[,,1:length(missing.z)] = out.weather[,,fill.z - out.z.start + 1]
            
            # Save
            saveRDS(fill.weather, tmp.file)
          } else {
            # Spinup for instantanious variables
            
            out.file = grep(x = out.files, pattern = paste0(".*", out.varname, "_.*", in.year), value = T)
            tmp.file = gsub(x = out.file, pattern = weather.dir.out, replacement = weather.dir.tmp)
            tmp.file = gsub(x = tmp.file, pattern = paste0(out.varname, "_6hourly"), replacement = paste0(out.varname, "_6hourly_ERA5"))
            tmp.file = gsub(x = tmp.file, pattern = ".nc", replacement = paste0(".spinup", ".RDS"))
            
            if(file.exists(tmp.file)){
              next
            }
            dir.create(dirname(tmp.file), recursive = T)
            print(basename(tmp.file))
            
            # Fill first 6 hours of the first day, with first 6 hours of last day
            missing.start = as.POSIXct(paste0("1979-01-01 ", "00:00:00"), tz = "GMT")
            missing.end = as.POSIXct(paste0("1979-01-01 ", "00:00:00"), tz = "GMT")
            missing.z = 1:min(which(as.POSIXct(out.time) >= missing.end))
            fill.start = as.POSIXct(paste0("1979-12-31 ", "00:00:00"), tz = "GMT")
            fill.end = as.POSIXct(paste0("1979-12-31 ", "00:00:00"), tz = "GMT")
            fill.z = which(as.POSIXct(out.time) >= fill.start & as.POSIXct(out.time) <= fill.end)
            
            # Get fill data
            fill.weather = array(NA, dim = c(dim(out.weather)[1:2], length(missing.z)))
            fill.weather[,,1:length(missing.z)] = out.weather[,,fill.z - out.z.start + 1]
            
            # Save
            saveRDS(fill.weather, tmp.file)
          }
        }
      }
    }
  }
}
