library(fields)
library(raster)
rm(list = ls())

# Input
coef.file <- "Saves/industrialCoef_country.csv"
gdp.file <- "../../../../Data/Transformed/GDP/GDPInterpolated_country.csv"
gva.file <- "../../../../Data/Transformed/GVA/GVAInterpolated_country.csv"
light.file <- "../../../../Data/Transformed/Light/lightCountryFraction_6min_global.RDS"
country.file <- "../../../../Data/Transformed/Country/country_6min_global.RDS"
ind.tmp <- "Saves/industrialDemandSpread_30min_global.RDS"
years <- 1979:2016

# Load
coef <- read.csv(coef.file, stringsAsFactors = F)
gdp <- read.csv(gdp.file, stringsAsFactors = F)
gva <- read.csv(gva.file, stringsAsFactors = F)
light <- readRDS(light.file)

country <- readRDS(country.file)

# Setup
lats <- seq(-89.75, 89.75, by = 0.5)
lons <- seq(-179.75, 179.75, by = 0.5)

data <- merge(gdp, gva)
data <- merge(data, coef, all.x = T)

data$Int <- 1
data$Int[data$OECD == 1 & data$Year > 1960 & data$Year < 2000] <- 0.976
data$Int[data$OECD == 1 & data$Year >= 2000] <- 0.99
data$Int[data$OECD == 0 & data$Year > 1980 & data$Year < 2000] <- 0.976
data$Int[data$OECD == 0 & data$Year >= 2000] <- 0.99

data$Gva <- data$Gdp * (data$GvaPGdp / 100)

# Calculate
data$with <- data$Intense * data$Gva *
  data$Int^(data$Year - data$BaseYear)

ind.with <- array(NA, dim = c(length(lons), length(lats), length(years)))
for (z in 1:length(years)) {
  print(paste0("Working on year ", years[z]))

  ind.with.y <- array(NA, dim = dim(country))
  for (x in 1:dim(country)[1]) {
    for (y in 1:dim(country)[2]) {
      if (is.na(country[x, y]) || country[x, y] == 10) {
        next
      }

      row.start <- min(which(data$Country_number == country[x, y]))
      ind.with.y[x, y] <- data$with[row.start + z - 1] * light[x, y]
    }
  }
  # image.plot(ind.with.y)

  ind.with.y.r <- raster(ind.with.y)
  extent(ind.with.y.r) <- c(-90, 90, -180, 180)
  # plot(ind.with.y.r)

  ind.with.agg <- aggregate(x = ind.with.y.r, fact = 5, fun = sum, na.rm = T)
  ind.with.agg <- as.matrix(ind.with.agg)
  # image.plot(ind.with.agg)

  ind.with[, , z] <- ind.with.agg
}

# Save
dir.create(dirname(ind.tmp))
saveRDS(ind.with, ind.tmp)
