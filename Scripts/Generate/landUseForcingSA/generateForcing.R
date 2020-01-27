library(fields)
library(ncdf4)

rm(list = ls())

# Input
Cc.monthly.file = "Saves/CcMonthly_30min_global.RDS"
Cv.monthly.file = "Saves/CvMonthly_30min_global.RDS"
out.dir = "../../../Output/Forcing/global/SA/coverage_monthly_MIRCA2000/coverage_monthly_MIRCA2000.nc"

# Load
Cc.monthly = readRDS(Cc.monthly.file)
Cv.monthly = readRDS(Cv.monthly.file)

# Setup
years = 1979:2016

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
vegs = 1:(dim(Cc.monthly)[3] + dim(Cv.monthly)[3] - 1)

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
veg.dim = ncdim_def(
  name = "veg_class",
  units = "N/A",
  vals = vegs,
  longname = "Vegetation class identification number"
)

data = array(NA, dim = c(length(lons), length(lats), length(vegs), 12))
data[,,1:10,] = Cv.monthly[,,1:10,]
data[,,11:62,] = Cc.monthly[,,1:52,]
data[,,63,] = Cv.monthly[,,11,] + Cc.monthly[,,53,]

# Calculate and save
for(z in 1:length(years)){
  year = years[z]
  
  print(paste0("Working on year ", year))
  
  out.name = paste0("coverage_monthly_MIRCA2000_", year, ".nc")
  out.sdir = paste0("/coverage_monthly_MIRCA2000/")
  out.file = paste0(out.dir, out.sdir, out.name)
  
  times = seq(
    from = as.Date(paste0(year,"-01-01")),
    to = as.Date(paste0(year, "-12-31")),
    by = "month"
  )
  
  time.dim = ncdim_def(
    name = "time",
    units = "days since 1970-01-01",
    vals = as.numeric(times), 
    unlim = T,
    calendar = "standard"
  )
  
  var = ncvar_def(
    name = "coverage",
    units = "fraction",
    dim = list(lon.dim, lat.dim, veg.dim, time.dim),
    missval = -1,
    longname = "vegetation coverage",
    prec = "double",
    compression = 9
  )
  
  dir.create(dirname(out.file), recursive = T)
  nc = nc_create(out.file, list(var))
  ncvar_put(nc, var$name, data)
  nc_close(nc)
}
