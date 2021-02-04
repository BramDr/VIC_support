library(ncdf4)
library(fields)
rm(list = ls())

# Input
out.dir <- "../../../Data/VIC/Forcing/global/co2_yearly_538/co2_yearly_538_"
years <- 1979:2099

# Setup
res <- 0.5
lons <- seq(
  from = -180 + res / 2,
  to = 180 - res / 2,
  by = res
)
lats <- seq(
  from = -90 + res / 2,
  to = 90 - res / 2,
  by = res
)

lon.dim <- ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = lons,
  longname = "longitude of cell centre"
)
lat.dim <- ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = lats,
  longname = "latitude of cell centre"
)

# Calculate and save
for (z in 1:length(years)) {
  year <- years[z]

  out.file <- paste0(out.dir, year, ".nc")

  times <- as.Date(paste0(year, "-01-01"))

  time.dim <- ncdim_def(
    name = "time",
    units = "days since 1970-01-01",
    vals = as.numeric(times),
    unlim = T,
    calendar = "standard"
  )

  var <- ncvar_def(
    name = "co2",
    units = "ppm",
    dim = list(lon.dim, lat.dim, time.dim),
    missval = -1,
    longname = "Atmospheric CO2 concentration",
    prec = "double",
    compression = 9
  )

  data <- array(538, dim = c(lon.dim$len, lat.dim$len, time.dim$len))

  dir.create(dirname(out.file))
  nc <- nc_create(out.file, list(var))
  ncvar_put(nc, var, data)
  nc_close(nc)
}
