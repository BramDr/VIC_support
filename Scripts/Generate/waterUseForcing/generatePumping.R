library(fields)
library(ncdf4)
rm(list = ls())

# Input
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
pumping.file <- "../../../Data/Transformed/Pumping/pumpingCapacity_30min_global.RDS"
out.dir <- "../../../Data/VIC/Forcing/global/pumpCap_daily/pumpCap_daily_"
years <- 1960:2016

# Load
nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

pumping <- readRDS(pumping.file)

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
pumping.tot <- array(NA, dim = c(dim(pumping)[1:2], dim(pumping)[3] + 1))
pumping.tot[, , 1:dim(pumping)[3]] <- pumping
pumping.tot[, , dim(pumping.tot)[3]] <- pumping[, , dim(pumping)[3]]

for (x in 1:dim(mask)[1]) {
  for (y in 1:dim(mask)[2]) {
    if (is.na(mask[x, y]) || mask[x, y] == 0) {
      pumping.tot[x, y, ] <- NA
    }
  }
}
image.plot(pumping.tot[, , dim(pumping.tot)[3]])

# Save
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

  for (m in 1:12) {
    ncvar_put(nc, var$name, pumping.tot[, , z], start = c(1, 1, m), count = c(-1, -1, 1))
  }
  nc_close(nc)
}
