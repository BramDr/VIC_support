library(fields)
library(ncdf4)
rm(list = ls())

# Input
iso.file <- "../../../../Data/Primary/ISO3166/ISO3166_dev.csv"
country.file <- "../../../../Data/Transformed/Country/country_5min_Indus.RDS"
light.file <- "../../../../Data/Transformed/Light/light_5min_Indus.RDS"
with.file <- "../../../../Data/Transformed/Industrial/industrialWithdrawal_country.csv"
area.file <- "../../../../Data/Transformed/Routing/area_5min_Indus.RDS"
ind.out <- "Saves/industrialDemand_periods_5min_Indus.RDS"
periods = data.frame(scenario = c("historical", "ssp126", "ssp126", "ssp370", "ssp370", "ssp585", "ssp585"),
                     syear = c(1970, 2020, 2070, 2020, 2070, 2020, 2070),
                     eyear = c(2000, 2050, 2100, 2050, 2100, 2050, 2100),
                     stringsAsFactors = F)

# Load
iso <- read.csv(file = iso.file, stringsAsFactors = F)
with <- read.csv(with.file, stringsAsFactors = F)
country <- readRDS(country.file)
area <- readRDS(area.file)
light <- readRDS(light.file)

# Calculate
ind.tot = mean(with[with$Country_number == iso$Country_number[iso$Country == "Pakistan"],]$With)
ind.int = ind.tot / sum(light[country == 586], na.rm = T)
ind.dem = light * ind.int / 365 / area * 1e3 # m3 y-1 to mm day-1
image.plot(ind.dem)

# Save
ind.dem.data = list()
for(i in 1:nrow(periods)){
  period.pattern = paste0(periods$scenario[i], "_", periods$syear[i], "_", periods$eyear[i])
  print(period.pattern)
  
  ind.dem.data[[period.pattern]] = ind.dem
}

dir.create(dirname(ind.out))
saveRDS(ind.dem.data, ind.out)
