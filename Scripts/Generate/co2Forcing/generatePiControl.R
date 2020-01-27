library(ncdf4)
library(fields)
rm(list = ls())

# Input
co2.dir <- "../../../Data/Primary/ISIMIP/CO2/piControl"
out.dir <- "../../../Data/VIC/Forcing/global/co2_monthly_ISIMIP_piControl/co2_monthly_ISIMIP_piControl_"
years <- 1661:2299

# Load
co2.files <- list.files(co2.dir, full.names = T)

co2 <- data.frame(YEARS = numeric(), CO2 = numeric())
for(co2.file in co2.files){
  co2.f <- read.table(co2.file, header = T)
  co2 <- rbind(co2, co2.f)
}

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
  
  co2.current <- co2$CO2[co2$YEARS == year]
  data <- array(co2.current, dim = c(lon.dim$len, lat.dim$len, time.dim$len))
  
  dir.create(dirname(out.file))
  nc <- nc_create(out.file, list(var))
  ncvar_put(nc, var, data)
  nc_close(nc)
}
