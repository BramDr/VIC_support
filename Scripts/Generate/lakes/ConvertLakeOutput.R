library(ncdf4)
library(lubridate)

rm(list = ls())

vic.file = "/home/bram/Data/VIC/output/lakes/fluxes_lakes_global_GFDL-ESM2M_historical.1861-01-01.nc"
out.dir = "/home/bram/Projects/VICsupport/Lakes/Output"

nc = nc_open(vic.file)
startyear = format.Date(as.Date(nc$dim$time$vals[1], origin = "0000-12-30"), format = "%Y")
endyear = format.Date(as.Date(nc$dim$time$vals[nc$dim$time$len], origin = "0000-12-30"), format = "%Y")
nc_close(nc)

years = startyear:endyear

ice.start.fun = function(x) {
  return(min(which(x > 0)))
}
ice.end.fun = function(x) {
  return(max(which(x > 0)))
}
ice.dur.fun = function(x) {
  return(sum(x > 0))
}

ice.start = array(NA, dim = c(nc$dim$lon$len, nc$dim$lat$len, length(years) - 1))
ice.end = array(NA, dim = c(nc$dim$lon$len, nc$dim$lat$len, length(years) - 1))
ice.dur = array(NA, dim = c(nc$dim$lon$len, nc$dim$lat$len, length(years) - 1))

nc = nc_open(vic.file)
time.full = as.Date(nc$dim$time$vals, origin = "0000-12-30")
for(z in 1:length(years)) {
  year = years[z]
  print(paste0("Working on year ", year, " in range ", min(years), " - ", max(years)))
  
  start.date = paste0(year,"-09-01")
  end.date = paste0(year + 1,"-08-30")
  start.jday = as.numeric(format.Date(start.date, format = "%j"))
  end.jday = as.numeric(format.Date(end.date, format = "%j"))
  
  start.idx = min(which(time.full == start.date))
  end.idx = max(which(time.full == end.date))
  count.idx = end.idx - start.idx + 1
  
  ice.frac = ncvar_get(nc, nc$var$OUT_LAKE_ICE_FRACT, start = c(1,1,1,start.idx), count = c(-1,-1,1,count.idx))
  
  ice.start.year = array(NA, dim = c(nc$dim$lon$len, nc$dim$lat$len))
  ice.end.year = array(NA, dim = c(nc$dim$lon$len, nc$dim$lat$len))
  ice.dur.year = array(NA, dim = c(nc$dim$lon$len, nc$dim$lat$len))
  for(x in 1:dim(ice.frac)[1]){
    for(y in 1:dim(ice.frac)[2]){
      if(is.na(ice.frac[x,y,1])){
        next
      }
      ice.start.year[x,y] = ice.start.fun(ice.frac[x,y,]) + start.jday - 1
      ice.end.year[x,y] = ice.end.fun(ice.frac[x,y,]) + start.jday - 1
      ice.dur.year[x,y] = ice.dur.fun(ice.frac[x,y,])
      
      # Handle end of year
      ice.start.year[x,y] = ice.start.year[x,y] %% (365 + leap_year(year))
      ice.end.year[x,y] = ice.end.year[x,y] %% (365 + leap_year(year))
    }
  }
  ice.start.year[is.infinite(ice.start.year)] = NA
  ice.end.year[is.infinite(ice.end.year)] = NA
  
  #image.plot(ice.start.year)
  #image.plot(ice.end.year)
  
  ice.start[,,z] = ice.start.year
  ice.end[,,z] = ice.end.year
  ice.dur[,,z] = ice.dur.year
}
nc_close(nc)

dim.lat = ncdim_def(name = "lat", units = "degrees_north", 
                    vals = nc$dim$lat$vals, longname = "longitude")
dim.lon = ncdim_def(name = "lon", units = "degrees_east", 
                    vals = nc$dim$lon$vals, longname = "latitude")
dim.time = ncdim_def(name = "time", units = "years since 1861", 
                     vals = years[1:(length(years)) - 1] - 1861, calendar = "standard")
var.icestart = ncvar_def(name = "icestart", units = "julian day",
                         dim = list(dim.lon, dim.lat, dim.time),
                         longname = "first day of ice, as measured from this year september to next year september")
var.iceend = ncvar_def(name = "iceend", units = "julian day",
                       dim = list(dim.lon, dim.lat, dim.time),
                       longname = "last day of ice, as measured from this year september to next year september")
var.icedur = ncvar_def(name = "icedur", units = "days",
                       dim = list(dim.lon, dim.lat, dim.time),
                       longname = "total days of ice, as measured from this year september to next year september")

filename = basename(vic.file)
filename.details = gsub(filename, pattern = "fluxes_lakes_global_", replacement = "")
filename.details = gsub(filename.details, pattern = "\\..*", replacement = "")

ghm = "vic-lakes"
gcm = gsub(filename.details, pattern = "_.*", replacement = "")
gcm = tolower(gcm)
biascorr = "ewembi"
climate.scenario = gsub(filename.details, pattern = ".*_", replacement = "")
socio.scenario = "nosoc"
co2.scenario = "co2"
region = "global"
timestep = "yearly"

variable = "icestart"
out.start.file = paste0(out.dir, "/",
                        ghm, "_", 
                        gcm, "_", 
                        biascorr, "_",
                        climate.scenario, "_",
                        socio.scenario, "_",
                        co2.scenario, "_",
                        variable, "_",
                        region, "_",
                        timestep, "_",
                        startyear, "_", 
                        endyear, ".nc4")
basename(out.start.file)

variable = "iceend"
out.end.file = paste0(out.dir, "/",
                      ghm, "_", 
                      gcm, "_", 
                      biascorr, "_",
                      climate.scenario, "_",
                      socio.scenario, "_",
                      co2.scenario, "_",
                      variable, "_",
                      region, "_",
                      timestep, "_",
                      startyear, "_", 
                      endyear, ".nc4")
basename(out.end.file)

variable = "icedur"
out.dur.file = paste0(out.dir, "/",
                      ghm, "_", 
                      gcm, "_", 
                      biascorr, "_",
                      climate.scenario, "_",
                      socio.scenario, "_",
                      co2.scenario, "_",
                      variable, "_",
                      region, "_",
                      timestep, "_",
                      startyear, "_", 
                      endyear, ".nc4")
basename(out.dur.file)

add.attributes = function(nc) {
  ncatt_put(
    nc = nc,
    varid = 0,
    attname = "Description",
    attval = "Output from the Variable Infiltration Capacity (VIC) Macroscale Hydrologic Model, customized for lake simulations (VIC-LAKES) at Wageningen University"
  )
  ncatt_put(
    nc = nc,
    varid = 0,
    attname = "Institution",
    attval = "Wageningen University (WU)"
  )
  ncatt_put(
    nc = nc,
    varid = 0,
    attname = "Contact",
    attval = "Bram Droppers (bram.droppers@wur.nl) and Annette Janssen (annette.janssen@wur.nl)"
  )
  ncatt_put(
    nc = nc,
    varid = 0,
    attname = "Reference",
    attval = "Bowling, L.C. and D.P. Lettenmaier, 2010: Modeling the effects of lakes and wetlands on the water balance of Arctic Environments, J. Hydromet., 11, 276-295, doi:10.1175/2009JHM1084.1"
  )
  
  ncatt_put(
    nc = nc,
    varid = "lon",
    attname = "standard_name",
    attval = "longitude"
  )
  ncatt_put(
    nc = nc,
    varid = "lon",
    attname = "axis",
    attval = "X"
  )
  ncatt_put(
    nc = nc,
    varid = "lat",
    attname = "standard_name",
    attval = "latitude"
  )
  ncatt_put(
    nc = nc,
    varid = "lat",
    attname = "axis",
    attval = "Y"
  )
  
  ncatt_put(
    nc = nc,
    varid = "lat",
    attname = "axis",
    attval = "Y"
  )
}

nc = nc_create(out.start.file, list(var.icestart))
add.attributes(nc)
ncvar_put(nc, nc$var$icestart, ice.start)
nc_close(nc)

nc = nc_create(out.end.file, list(var.iceend))
add.attributes(nc)
ncvar_put(nc, nc$var$iceend, ice.end)
nc_close(nc)

nc = nc_create(out.dur.file, list(var.icedur))
add.attributes(nc)
ncvar_put(nc, nc$var$icedur, ice.dur)
nc_close(nc)
