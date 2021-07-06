library(fields)
library(ncdf4)

rm(list = ls())

# Input
coverage.file <- "../VICparameters_Indus/agriculture/Saves/coverage_adjusted_monthly_5min_Indus.RDS"
out.file <- "../../../Data/VIC/Forcing/Indus_5min/coverage_monthly_MIRCA/coverage_monthly_MIRCA.nc"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
coverage <- readRDS(coverage.file)

# Setup
years <- 1850:2100

vegs <- 1:dim(coverage)[3]

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
    prec = "float",
    compression = 9
  )

  nc <- nc_create(out.file.tmp, list(var))
  ncvar_put(nc, var$name, coverage)
  nc_close(nc)
}
