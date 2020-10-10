library(ncdf4)
library(fields)
rm(list = ls())

# Input
co2.file <- "../../../../Data/Primary/GGCMI/Phase1/NASA.CO2.Monthly.1850-2013.csv"
out.dir <- "../../../../Data/VIC/Forcing/global/GGCMI_1/co2_monthly/co2_monthly_"
years <- 1850:2014

# Load
co2 <- read.csv(co2.file, header = T, skip = 1)

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
  
  times <- seq(
    from = as.Date(paste0(year, "-01-01")),
    to = as.Date(paste0(year, "-12-31")),
    by = "month"
  )
  
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
    longname = "atmospheric CO2 concentration",
    prec = "double",
    compression = 9
  )
  
  co2.current <- co2$CO2[co2$YEAR == year]
  data <- array(NA, dim = c(lon.dim$len, lat.dim$len, time.dim$len))
  for(z in 1:time.dim$len){
    data[,,z] = co2.current[z]
  }
  
  dir.create(dirname(out.file))
  nc <- nc_create(out.file, list(var))
  ncvar_put(nc, var, data)
  nc_close(nc)
}
