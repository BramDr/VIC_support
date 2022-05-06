library(ncdf4)
library(fields)
rm(list = ls())

# Input
co2.dir = "../../../Data/Primary/ISIMIP3b/"
out.dir <- "../../../Data/VIC/Forcing/global/co2_yearly_fixed/co2_yearly_fixed_"
scenarios = c("historical", "ssp585")

resolution = 0.5
out.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
out.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)

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
i = 1
for(i in 1:length(scenarios)) {
  scenario = scenarios[i]
  
  co2.dir.tmp = paste0(co2.dir, "/", scenario, "/")
  co2.file = list.files(co2.dir.tmp, pattern = scenario, full.names = T)
  co2 = read.table(co2.file)
  
  j = 1
  for (j in 1:nrow(co2)) {
    year = co2$V1[j]
    co2.year = co2$V2[j]
    
    out.file <- paste0(out.dir, year, ".nc")
    out.file = gsub(out.file, pattern = "fixed", replacement = scenario)

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

    data <- array(co2.year, dim = c(lon.dim$len, lat.dim$len, time.dim$len))

    dir.create(dirname(out.file))
    nc <- nc_create(out.file, list(var))
    ncvar_put(nc, var, data)
    nc_close(nc)
  }
}
