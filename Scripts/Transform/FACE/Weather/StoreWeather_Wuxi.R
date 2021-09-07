library(openxlsx)
library(humidity)
library(zoo)
rm(list = ls())

observed.file <- "../../../../Data/Primary/FACE/Hasegawa2017/Wuxi_FACE_R/Wuxi_Onsite_weather_01_03_with_CO2_A_E_corrected_130502.xlsx"
out.weather <- "./Saves/weather_Wuxi.RDS"

weather = read.xlsx(observed.file, sheet = "Climate01_03")
weather = weather[2:nrow(weather),]
weather$W_DATE = as.Date(paste0(weather$Year, "-", weather$Day), format = "%Y-%j")

# Correct NAs
weather$Pr[is.na(weather$Pr)] = 0
weather$mean.air.temp = na.approx(weather$mean.air.temp, na.rm = F, rule = 2)
weather$wind.speed = na.approx(weather$wind.speed, na.rm = F, rule = 2)
weather$solar.rad = na.approx(weather$solar.rad, na.rm = F, rule = 2)
weather$VP_sat = na.approx(weather$VP_sat, na.rm = F, rule = 2)
weather$RH = na.approx(weather$RH, na.rm = F, rule = 2)

# Convert vapour pressure
plot(weather$W_DATE, weather$mean.air.temp, type = "l")
plot(weather$W_DATE, weather$RH, type = "l")
plot(weather$W_DATE, weather$Pr, type = "l")

weather$solar.rad = as.numeric(weather$solar.rad) * 1e3 / (24 * 60 * 60)
weather$VP = WVP2(as.numeric(weather$RH), as.numeric(weather$VP_sat)) * 1e-3

dir.create(dirname(out.weather))
saveRDS(weather, out.weather)
