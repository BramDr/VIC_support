library(fields)
library(ncdf4)
rm(list = ls())

# Input
iso.file <- "../../../../Data/Primary/ISO3166/ISO3166_dev.csv"
with.file <- "../../../../Data/Transformed/Domestic/domesticWithdrawal_country.csv"
tmon.file <- "../../../../Data/Transformed/ERA5/tas_daily_ERA5adj_ymonmean_1980_2010.nc"
tyear.file <- "../../../../Data/Transformed/ERA5/tas_daily_ERA5adj_yearmean_1980_2010.nc"
tyear.max.file <- "../../../../Data/Transformed/ERA5/tas_daily_ERA5adj_ymonmeanmax_1980_2010.nc"
tyear.min.file <- "../../../../Data/Transformed/ERA5/tas_daily_ERA5adj_ymonmeanmin_1980_2010.nc"
area.file <- "../../../../Data/Transformed/Routing/area_5min_Indus.RDS"
pop.file <- "../../../../Data/Transformed/Population/population_historical_extended_5min_Indus.RDS"
dom.out <- "Saves/domesticDemand_5min_Indus.RDS"
years = 1900:2017

# Load
iso <- read.csv(file = iso.file, stringsAsFactors = F)
with <- read.csv(with.file, stringsAsFactors = F)
area <- readRDS(area.file)
pop <- readRDS(pop.file)

nc <- nc_open(tmon.file)
tmon <- ncvar_get(nc, "tas")
nc_close(nc)
nc <- nc_open(tyear.file)
tyear <- ncvar_get(nc, "tas")
nc_close(nc)
nc <- nc_open(tyear.max.file)
tyear.max <- ncvar_get(nc, "tas")
nc_close(nc)
nc <- nc_open(tyear.min.file)
tyear.min <- ncvar_get(nc, "tas")
nc_close(nc)

# Setup
pop.years = as.numeric(dimnames(pop)[[3]])
pop = pop[,,which(pop.years %in% years)]

dom.int <- with[with$Country_number == iso$Country_number[iso$Country == "Pakistan"],]$WithPc

# Calculate
dom.dem <- array(0, dim = c(dim(pop), 12))
y = 1
for(y in 1:dim(pop)[3]) {
  m = 1
  for (m in 1:12) {
    frac <- 1 + (tmon[, , m] - tyear) / (tyear.max - tyear.min) * 0.45
    dom.dem[, , y, m] <- dom.int * pop[,,y] / 365 / area * 1e3 * frac # m3 cap-1 y-1 to mm day-1
  }
}
dom.dem.mean = apply(X = dom.dem, MARGIN = c(1,2), FUN = mean)
image.plot(dom.dem.mean)

dimnames(dom.dem)[[3]] = years

# Save
dir.create(dirname(dom.out))
saveRDS(dom.dem, dom.out)
