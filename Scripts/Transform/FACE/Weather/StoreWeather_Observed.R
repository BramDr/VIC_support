library(readODS)
rm(list = ls())

soil.file <- "../../../../Data/Primary/FACE/Kimball2017/Biomass Yield Area Phenology Management Weather Soil Moisture.ods"
out.weather <- "./Saves_observed/weather_observed.RDS"

weather = read_ods(path = soil.file, sheet = "Weather_daily")
weather$W_DATE = as.Date(weather$W_DATE)

plot(weather$W_DATE, weather$RAIN, type = "l")
plot(weather$W_DATE, weather$TMIN, type = "l")
plot(weather$W_DATE, weather$TMAX, type = "l")

dir.create(dirname(out.weather))
saveRDS(weather, out.weather)
