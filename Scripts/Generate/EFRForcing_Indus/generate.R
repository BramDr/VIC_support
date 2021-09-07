library(fields)
library(ncdf4)
library(zoo)
rm(list = ls())

# Input
efr.dir <- "./Saves/"
efr.out <- "../../../Data/VIC/Forcing/Indus_5min/efrDischarge_daily/efrDischarge_daily_"
years = 1968:2100

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
efr.files = list.files(efr.dir, pattern = "*_historical_*", full.names = T)

efr.file = efr.files[1]
for(efr.file in efr.files) {
  
  patterns = strsplit(basename(efr.file), "_")[[1]]
  model = patterns[2]
  scenario = patterns[3]
  syear = patterns[4]
  eyear = patterns[5]
  
  period.pattern = paste0(model, "_", scenario, "_", syear, "_", eyear)
  
  print(paste(basename(efr.file), period.pattern))
  
  efr = readRDS(efr.file)

  lon.dim <- ncdim_def(
    name = "lon",
    units = "degrees_east",
    vals = out.lons,
    longname = "longitude of cell centre"
  )
  lat.dim <- ncdim_def(
    name = "lat",
    units = "degrees_north",
    vals = out.lats,
    longname = "latitude of cell centre"
  )

  z = 1
  for (z in 1:length(years)) {
    year <- years[z]

    efr.out.tmp <- gsub(x = efr.out, pattern = "_daily", replacement = paste0("_daily", "_", period.pattern))
    efr.out.tmp <- paste0(efr.out.tmp, year, ".nc")
    
    if(file.exists(efr.out.tmp)){
      next
    }
    
    print(paste0("Working on year ", year))

    times <- seq(
      from = as.Date(paste0(year, "-01-01")),
      to = as.Date(paste0(year, "-12-31")),
      by = "day",
      origin = "1900-01-01"
    )

    time.dim <- ncdim_def(
      name = "time",
      units = "days since 1970-01-01",
      vals = as.numeric(times),
      unlim = T,
      calendar = "standard"
    )

    var <- ncvar_def(
      name = "discharge",
      units = "m3 s-1",
      dim = list(lon.dim, lat.dim, time.dim),
      missval = -1,
      longname = "Discharge requirements for environmental flow",
      prec = "double",
      compression = 2
    )

    dir.create(dirname(efr.out.tmp), showWarnings = F)
    nc <- nc_create(efr.out.tmp, list(var))
    ncvar_put(nc, var$name, efr[,,1:length(times)])
    nc_close(nc)
  }
}
