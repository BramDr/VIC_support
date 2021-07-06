library(fields)
library(ncdf4)
rm(list = ls())

# Input
iso.file <- "../../../../Data/Primary/ISO3166/ISO3166_dev.csv"
country.file <- "../../../../Data/Transformed/Country/country_5min_Indus.RDS"
light.file <- "../../../../Data/Transformed/Light/light_5min_Indus.RDS"
with.file <- "../../../../Data/Transformed/Industrial/industrialWithdrawal_country.csv"
area.file <- "../../../../Data/Transformed/Routing/area_5min_Indus.RDS"
ind.out <- "Saves/industrialDemand_5min_Indus.RDS"

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
dir.create(dirname(ind.out))
saveRDS(ind.dem, ind.out)
