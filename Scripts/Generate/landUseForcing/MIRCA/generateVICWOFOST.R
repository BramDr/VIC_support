library(fields)
library(ncdf4)

rm(list = ls())

# Input
Cv.monthly.file <- "Hybrid/Saves/Cv_monthly_30min_global.RDS"
out.file <- "../../../../Data/VIC/Forcing/global/coverage_monthly_VICWOFOST/coverage_monthly_VICWOFOST.nc"

# Load
Cv.monthly <- readRDS(Cv.monthly.file)

# Setup
years <- 1979:2016

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
vegs <- 1:dim(Cv.monthly)[3]

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
veg.dim <- ncdim_def(
  name = "veg_class",
  units = "N/A",
  vals = vegs,
  longname = "Vegetation class identification number"
)

# Calculate and save
dir.create(dirname(out.file), recursive = T)
for (z in 1:length(years)) {
  year <- years[z]
  print(year)

  out.file.tmp <- gsub(x = out.file, pattern = ".nc$", replacement = paste0("_", year, ".nc"))

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
    name = "coverage",
    units = "fraction",
    dim = list(lon.dim, lat.dim, veg.dim, time.dim),
    missval = -1,
    longname = "vegetation coverage",
    prec = "double",
    compression = 9
  )

  nc <- nc_create(out.file.tmp, list(var))
  ncvar_put(nc, var$name, Cv.monthly)
  nc_close(nc)
}
