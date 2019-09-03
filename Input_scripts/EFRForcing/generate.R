library(fields)
library(ncdf4)
library(zoo)
rm(list = ls())

# Input
in.efr = "Input/fluxes_global_MIRCA_nat.1979-01-01.nc"
out.dir = "Output/"
years = 1979:2016

# Load
nc = nc_open(in.efr)
efr.dis = ncvar_get(nc, "OUT_DISCHARGE")
efr.base = ncvar_get(nc, "OUT_BASEFLOW")
nc_close(nc)
print("Loaded")

# Setup
res = 0.5
lons = seq(
  from = -180 + res / 2,
  to = 180 - res / 2,
  by = res
)
lats = seq(
  from = -90 + res / 2,
  to = 90 - res / 2,
  by = res
)

lon.dim = ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = lons,
  longname = "longitude of cell centre"
)
lat.dim = ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = lats,
  longname = "latitude of cell centre"
)

# Calculate and save
efr.dis2 = array(NA, dim = dim(efr.dis))
efr.base2 = array(NA, dim = dim(efr.base))
for(x in 1:dim(efr.dis2)[1]){
  for(y in 1:dim(efr.dis2)[2]){
    dis = efr.dis[x,y,]
    base = efr.base[x,y,]
    
    if(is.na(dis[1])){
      next
    }
    
    dis = rollapply(data = rep(dis, 3), width = 30, FUN = mean, partial = TRUE)
    dis = dis[(nc$dim$time$len + 1):(nc$dim$time$len * 2)]
    
    base = rollapply(data = rep(base, 3), width = 30, FUN = mean, partial = TRUE)
    base = base[(nc$dim$time$len + 1):(nc$dim$time$len * 2)]
    
    efr = dis
    sel = dis <= 0.4 * mean(dis)
    efr[sel] = 0.6 * dis[sel]
    sel = dis > 0.8 * mean(dis)
    efr[sel] = 0.3 * dis[sel]
    sel = dis > 0.4 * mean(dis) & dis <= 0.8 * mean(dis)
    efr[sel] = 0.45 * dis[sel]
    
    efr.dis2[x,y,] = efr
    
    efr = 0.9 * base / 4 # day-1 to step-1
    
    efr.base2[x,y,] = efr
  }
}

rm(efr.dis, efr.base)
print("Calculated")

for(z in 1:length(years)){
  year = years[z]
  print(paste0("Working on year ", year))
  
  times = seq(
    from = as.Date(paste0(year,"-01-01")),
    to = as.Date(paste0(year, "-12-31")),
    by = "day",
    origin = "1900-01-01"
  )
  
  time.dim = ncdim_def(
    name = "time",
    units = "days since 1970-01-01",
    vals = as.numeric(times), 
    unlim = T,
    calendar = "standard"
  )
  
  out.name = paste0("efr_discharge_6hourly_", year, ".nc")
  out.sdir = paste0("/efr_discharge_6hourly/")
  out.file = paste0(out.dir, out.sdir, out.name)
  
  var = ncvar_def(
    name = "discharge",
    units = "m3 s-1",
    dim = list(lon.dim, lat.dim, time.dim),
    missval = -1,
    longname = "Discharge requirements for environmental flow",
    prec = "double",
    compression = 9
  )
  
  dir.create(dirname(out.file), showWarnings = F)
  nc = nc_create(out.file, list(var))
  ncvar_put(nc, var$name, efr.dis2[,,1:length(times)])
  nc_close(nc)
  
  out.name = paste0("efr_baseflow_6hourly_", year, ".nc")
  out.sdir = paste0("/efr_baseflow_6hourly/")
  out.file = paste0(out.dir, out.sdir, out.name)
  
  var = ncvar_def(
    name = "baseflow",
    units = "mm",
    dim = list(lon.dim, lat.dim, time.dim),
    missval = -1,
    longname = "Baseflow requirements for environmental flow",
    prec = "double",
    compression = 9
  )
  
  dir.create(dirname(out.file), showWarnings = F)
  nc = nc_create(out.file, list(var))
  ncvar_put(nc, var$name, efr.base2[,,1:length(times)])
  nc_close(nc)
}

rm(efr.dis2, efr.base2)
