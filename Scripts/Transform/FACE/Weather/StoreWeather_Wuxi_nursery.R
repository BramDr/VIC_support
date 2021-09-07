library(openxlsx)
library(humidity)
rm(list = ls())

observed.file <- "../../../../Data/Primary/FACE/Hasegawa2017/Wuxi_FACE_R/Wuxi_Nursery_data.xlsx"
out.weather <- "./Saves/weather_Wuxi_nursery.RDS"

treatment = data.frame(year = rep(2001:2003, each = 6),
                       nutrient = rep(rep(c("LN", "MN", "HN"), each = 2), 3),
                       co2 = c("A", "E"))
treatment$treatment = paste0(substr(treatment$year, 3, 4), treatment$nutrient, treatment$co2)
treatment = treatment[!(treatment$year == 2001 & treatment$nutrient == "HN"),]

i = 1
for(i in 1:nrow(treatment)) {
  pattern = paste0(treatment$year[i])
  
  weather = read.xlsx(observed.file, sheet = pattern)
  weather = weather[2:nrow(weather),]
  weather$date = as.Date(weather$date, origin = "1899-12-30")
  
  weather$solar.rad = as.numeric(weather$solar.rad) * 1e6 / (24 * 60 * 60)
  
  weather$treatment = treatment$treatment[i]
  
  if(i == 1){
    weather.all = weather
  } else {
    weather.all = rbind(weather.all, weather)
  }
}

dir.create(dirname(out.weather))
saveRDS(weather.all, out.weather)
