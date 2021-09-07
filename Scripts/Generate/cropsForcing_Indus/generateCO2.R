library(ncdf4)
library(fields)
rm(list = ls())

# Input
co2.file = "../../../Data/Primary/ISIMIP3b/historical/co2_historical_annual_1850_2014.txt"
out.dir <- "../../../Data/VIC/Forcing/Indus_5min/co2_yearly/co2_yearly_"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]
years <- 1850:2018

# Load
co2 = read.table(co2.file)

# Setup
co2.adj = co2[co2[,1] %in% years,]
co2.slope = (co2.adj[nrow(co2.adj),2] - co2.adj[nrow(co2.adj) - 15,2]) / 15
co2.extended = data.frame(V1 = years[(nrow(co2.adj) + 1):length(years)],
                          V2 = co2.adj[nrow(co2.adj),2] + co2.slope * 1:(length(years) - nrow(co2.adj)))
co2.adj = rbind(co2.adj, co2.extended)
plot(co2.adj[,1], co2.adj[,2])

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
  
z = 1
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

  data <- array(co2.adj[z,2], dim = c(lon.dim$len, lat.dim$len, time.dim$len))

  dir.create(dirname(out.file))
  nc <- nc_create(out.file, list(var))
  ncvar_put(nc, var, data)
  nc_close(nc)
}
