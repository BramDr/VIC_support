library(ncdf4)
rm(list = ls())

# Input
co2.file <- "./Saves/co2_Arizona.RDS"
out.dir <- "../../../../Data/VIC/Forcing/FACE/Arizona/co2_yearly/co2_yearly_"
years <- 1992:1997

point <- c(33.0628, -111.9826) # lat-lon

# Load
co2 = readRDS(co2.file)

# Setup
lon.dim <- ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = point[2],
  longname = "longitude of cell centre"
)
lat.dim <- ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = point[1],
  longname = "latitude of cell centre"
)

# Calculate and save
treatment = co2$treatment[1]
for(treatment in co2$treatment){
  z = 1
  for (z in 1:length(years)) {
    year <- years[z]

    out.file <- paste0(out.dir, year, ".nc")
    out.file = gsub(out.file, pattern = "_yearly", replacement = paste0("_yearly_", treatment))

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

    data <- array(co2$co2[co2$treatment == treatment], dim = c(lon.dim$len, lat.dim$len, time.dim$len))

    dir.create(dirname(out.file), recursive = T)
    nc <- nc_create(out.file, list(var))
    ncvar_put(nc, var, data)
    nc_close(nc)
  }
}
