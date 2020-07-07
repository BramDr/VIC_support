library(fields)
library(ncdf4)
rm(list = ls())

# Input
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
out.dir <- "../../../Data/VIC/Forcing/global/pumpCap_yearly_stub/pumpCap_yearly_stub_"
years <- 1960:2016

# Load
nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

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

# Calculate
pumping.tot = array(99999, dim = c(length(lons), length(lats)))

# Save
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
    name = "pumping_capacity",
    units = "mm day-1",
    dim = list(lon.dim, lat.dim, time.dim),
    missval = -1,
    longname = paste0("Pumping capacity for groundwater withdrawals"),
    prec = "double",
    compression = 9
  )

  dir.create(dirname(out.file))
  nc <- nc_create(out.file, list(var))

  ncvar_put(nc, var$name, pumping.tot)
  nc_close(nc)
}
