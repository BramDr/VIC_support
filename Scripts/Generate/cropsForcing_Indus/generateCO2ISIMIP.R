library(ncdf4)
library(fields)
rm(list = ls())

# Input
co2.dir = "../../../Data/Primary/ISIMIP3b/"
out.dir <- "../../../Data/VIC/Forcing/Indus_5min/co2_yearly_fixed/co2_yearly_fixed_"
periods = data.frame(scenario = c("historical", "ssp126", "ssp126", "ssp370", "ssp370", "ssp585", "ssp585"),
                     syear = c(1970, 2020, 2070, 2020, 2070, 2020, 2070),
                     eyear = c(2000, 2050, 2100, 2050, 2100, 2050, 2100),
                     stringsAsFactors = F)
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]
years <- 1850:2100

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
i = 6
for(i in 1:nrow(periods)) {
  
  co2.dir.tmp = paste0(co2.dir, "/", periods$scenario[i], "/")
  co2.file = list.files(co2.dir.tmp, pattern = periods$scenario[i], full.names = T)
  
  co2 = read.table(co2.file)
  co2.mean = mean(co2$V2[co2$V1 == periods$eyear[i]])
  
  z = 1
  for (z in 1:length(years)) {
    year <- years[z]

    out.file <- paste0(out.dir, year, ".nc")
    out.file = gsub(out.file, pattern = "fixed", replacement = paste0(periods$scenario[i], "_", periods$syear[i], "_", periods$eyear[i]))

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

    data <- array(co2.mean, dim = c(lon.dim$len, lat.dim$len, time.dim$len))

    dir.create(dirname(out.file))
    nc <- nc_create(out.file, list(var))
    ncvar_put(nc, var, data)
    nc_close(nc)
  }
}
