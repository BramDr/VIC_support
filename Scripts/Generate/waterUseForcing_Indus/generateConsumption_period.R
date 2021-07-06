library(fields)
library(ncdf4)
rm(list = ls())

# Input
dom.file = "Domestic/Saves/domesticDemand_periods_5min_Indus.RDS"
ind.file = "Industrial/Saves/industrialDemand_periods_5min_Indus.RDS"
liv.file = "Livestock/Saves/livestockDemand_periods_5min_Indus.RDS"
out.file <- "../../../Data/VIC/Forcing/Indus_5min/sectoralConsump_monthly/sectoralConsump_monthly_"
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

# Calculate and save
i = 1
for(i in 1:nrow(periods)) {
  period.pattern = paste0(periods$scenario[i], "_", periods$syear[i], "_", periods$eyear[i])
  print(period.pattern)
    
  dem = dom[[period.pattern]]
  dem = dem + liv[[period.pattern]]
  for(m in 1:12){
    dem[,,m] = dem[,,m] + ind[[period.pattern]]
  }
  
  cons = dom[[period.pattern]] * 0.15
  cons = cons + liv[[period.pattern]] * 1
  for(m in 1:12){
    cons[,,m] = cons[,,m] + ind[[period.pattern]] * 0.1
  }
  
  cons = cons / dem
  cons[dem == 0] = 0
  cons[cons > 1] = 1
  cons[cons < 0] = 0
  #image.plot(cons[,,1], zlim = c(0,1))
  
  for (z in 1:length(years)) {
    year <- years[z]
    
    print(year)
    out.file.tmp <- paste0(out.file, year, ".nc")
    out.file.tmp = gsub(x = out.file.tmp, pattern = "sectoralConsump_monthly", replacement = paste0("sectoralConsump_monthly_", period.pattern))
    
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
      name = "consumption",
      units = "fraction",
      dim = list(lon.dim, lat.dim, time.dim),
      missval = -1,
      longname = paste0("sectoral consumption fraction"),
      prec = "double",
      compression = 9
    )
    
    dir.create(dirname(out.file.tmp))
    nc <- nc_create(out.file.tmp, list(var))
    ncvar_put(nc, var, cons)
    nc_close(nc)
  }
}
