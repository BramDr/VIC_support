library(fields)
library(ncdf4)
rm(list = ls())

# Input
out.file <- "../../../Data/VIC/Forcing/Indus_5min/sectoralGround_yearly/sectoralGround_yearly_"
years <- 1850:2100

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Setup
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

# Calculate and save
data = array(0, dim = c(length(out.lons), length(out.lats)))

for (z in 1:length(years)) {
  year <- years[z]
  
  print(year)
  
  out.file.tmp <- paste0(out.file, year, ".nc")
  
  times <- as.Date(paste0(year, "-01-01"))

  time.dim <- ncdim_def(
    name = "time",
    units = "days since 1970-01-01",
    vals = as.numeric(times),
    unlim = T,
    calendar = "standard"
  )

  var <- ncvar_def(
    name = "groundwater",
    units = "fraction",
    dim = list(lon.dim, lat.dim, time.dim),
    missval = -1,
    longname = paste0("sectoral groundwater fraction"),
    prec = "double",
    compression = 9
  )

  dir.create(dirname(out.file.tmp))
  nc <- nc_create(out.file.tmp, list(var))
  ncvar_put(nc, var, data)
  nc_close(nc)
}
