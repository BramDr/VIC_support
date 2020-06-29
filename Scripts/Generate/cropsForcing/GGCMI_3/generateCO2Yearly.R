library(ncdf4)
library(fields)
rm(list = ls())

# Input
co2.dir <- "../../../../Data/Primary/GGCMI/Phase3/co2/"
out.dir <- "../../../../Data/VIC/Forcing/global/GGCMI_3/co2_yearly/co2_yearly_"

# Load
co2.files = list.files(co2.dir, full.names = T)

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

for(co2.file in co2.files){
  scenario = gsub(x = basename(co2.file), pattern = "^co2_", replacement = "")
  scenario = gsub(x = scenario, pattern = "_annual.*", replacement = "")
  
  print(paste0(basename(co2.file), ": ", scenario))
  
  co2 <- read.table(co2.file, header = T, skip = 1)
  
  years = co2[,1]
  co2 = co2[,2]
  
  for (z in 1:length(years)) {
    year <- years[z]
    
    out.dir.tmp = gsub(x = out.dir, pattern = "_yearly", replacement = paste0("_yearly_", scenario))
    out.file <- paste0(out.dir.tmp, year, ".nc")
    
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
      longname = "atmospheric CO2 concentration",
      prec = "double",
      compression = 9
    )
    
    co2.current <- co2[years == year]
    data <- array(co2.current, dim = c(lon.dim$len, lat.dim$len, time.dim$len))
    
    dir.create(dirname(out.file))
    nc <- nc_create(out.file, list(var))
    ncvar_put(nc, var, data)
    nc_close(nc)
  }
}

