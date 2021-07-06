library(fields)
library(ncdf4)
rm(list = ls())

# Input
dom.file = "Domestic/Saves/domesticDemand_periods_5min_Indus.RDS"
ind.file = "Industrial/Saves/industrialDemand_periods_5min_Indus.RDS"
liv.file = "Livestock/Saves/livestockDemand_periods_5min_Indus.RDS"
out.file <- "../../../Data/VIC/Forcing/Indus_5min/sectoralDemand_monthly/sectoralDemand_monthly_"
periods = data.frame(scenario = c("historical", "ssp126", "ssp126", "ssp370", "ssp370", "ssp585", "ssp585"),
                     syear = c(1970, 2020, 2070, 2020, 2070, 2020, 2070),
                     eyear = c(2000, 2050, 2100, 2050, 2100, 2050, 2100),
                     stringsAsFactors = F)
years <- 1968:2100

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
dom = readRDS(dom.file)
ind = readRDS(ind.file)
liv = readRDS(liv.file)

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

# Calculate
i = 1
for(i in 1:nrow(periods)) {
  period.pattern = paste0(periods$scenario[i], "_", periods$syear[i], "_", periods$eyear[i])
  print(period.pattern)
  
  dem = dom[[period.pattern]]
  dem = dem + liv[[period.pattern]]
  for(z in 1:12){
    dem[,,z] = dem[,,z] + ind[[period.pattern]]
  }
  
  # Save
  z = 1
  for (z in 1:length(years)) {
    year <- years[z]
    
    print(year)
  
    out.file.tmp <- paste0(out.file, year, ".nc")
    out.file.tmp = gsub(x = out.file.tmp, pattern = "sectoralDemand_monthly", replacement = paste0("sectoralDemand_monthly_", period.pattern))
    
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
      name = "demand",
      units = "mm day-1",
      dim = list(lon.dim, lat.dim, time.dim),
      missval = -1,
      longname = "sectoral water demand",
      prec = "double",
      compression = 9
    )
  
    dir.create(dirname(out.file.tmp))
    nc <- nc_create(out.file.tmp, list(var))
    ncvar_put(nc, var, dem)
    nc_close(nc)
  }
}
