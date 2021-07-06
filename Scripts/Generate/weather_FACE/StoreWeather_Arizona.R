library(readODS)
library(humidity)
library(zoo)
rm(list = ls())

observed.file <- "../../../Data/Primary/FACE/Kimball2017/Biomass Yield Area Phenology Management Weather Soil Moisture.ods"
out.weather <- "./Saves/weather_Arizona.RDS"

weather = read_ods(path = observed.file, sheet = "Weather_daily")
weather$W_DATE = as.Date(weather$W_DATE)

# Correct NAs
weather$RAIN[is.na(weather$RAIN)] = 0
weather$TMIN = na.approx(weather$TMIN, na.rm = F, rule = 2)
weather$TMAX = na.approx(weather$TMAX, na.rm = F, rule = 2)
weather$WIND = na.approx(weather$WIND, na.rm = F, rule = 2)
weather$SRAD = na.approx(weather$SRAD, na.rm = F, rule = 2)
weather$TDEW = na.approx(weather$TDEW, na.rm = F, rule = 2)

# Convert vapour pressure  
plot(weather$W_DATE, weather$RAIN, type = "l")
plot(weather$W_DATE, weather$TMIN, type = "l")
plot(weather$W_DATE, weather$TMAX, type = "l")

weather$TAVG = (weather$TMIN + weather$TMAX) / 2
weather$VP = WVP1(weather$TDEW, isK = F) * 1e-1
weather$SRAD = weather$SRAD * 1e6 / (24 * 60 * 60)
weather$WIND = weather$WIND * 1e3 / (24 * 60 * 60)

dir.create(dirname(out.weather))
saveRDS(weather, out.weather)
