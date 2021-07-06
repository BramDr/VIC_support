library(ncdf4)
library(ncdf4.helpers)
rm(list = ls())

wfdei.dir <- "./WFDEI_in/Arizona/"
out.weather <- "./Saves/weather_Arizona_WFDEI.RDS"

point <- c(33.0628, -111.9826) # lat-lon
years = 1992:1997

wfdei.files = list.files(wfdei.dir, pattern = "6hourly_WFDEI.*.nc", full.names = T, recursive = T)

dates = seq(from = as.Date(paste0(years[1], "-01-01")), 
            to = as.Date(paste0(years[length(years)], "-12-31")),
            by = "day")
wfdei.df = data.frame(date = dates,
                      lwdown = NA,
                      swdown = NA,
                      tas = NA,
                      pr = NA,
                      vp = NA,
                      wind = NA,
                      psurf = NA)

wfdei.file = wfdei.files[14]
for(wfdei.file in wfdei.files){
  nc = nc_open(wfdei.file)
  varname = nc$var[[1]]$name
  lons = nc$dim$lon$vals
  lats = nc$dim$lat$vals
  dates = nc.get.time.series(nc)
  nc_close(nc)
  
  year = as.numeric(format.Date(dates, "%Y")[1])
  if(! (year %in% years)){
    next
  }
  
  print(basename(wfdei.file))
  
  x = which.min(abs(lons - point[2]))
  y = which.min(abs(lats - point[1]))
  nc = nc_open(wfdei.file)
  vardata = ncvar_get(nc, nc$var[[1]], start = c(x,y,1), count = c(1,1,-1))
  nc_close(nc)
  
  dates.agg = as.Date(format.Date(dates, "%Y-%m-%d"))
  if(varname == "pr") {
    vardata.agg = aggregate(x = vardata, by = list(dates.agg), FUN = sum)[,2]
  } else {
    vardata.agg = aggregate(x = vardata, by = list(dates.agg), FUN = mean)[,2]
  }
  
  wfdei.df[wfdei.df$date %in% dates.agg, varname] = vardata.agg
}

dir.create(dirname(out.weather))
saveRDS(wfdei.df, out.weather)
