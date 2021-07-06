library(openxlsx)
library(humidity)
rm(list = ls())

observed.a.file <- "../../../../Data/Primary/FACE/Yang2006/Shizukuishi_FACE_98_00/Shizukuishi_Nursery_data_ambient.xlsx"
observed.e.file <- "../../../../Data/Primary/FACE/Yang2006/Shizukuishi_FACE_98_00/Shizukuishi_Nursery_data_elevated.xlsx"
out.weather <- "./Saves/weather_Shizukuishi_nursery.RDS"

treatment = data.frame(year = rep(1998:2000, each = 6),
                       nutrient = rep(rep(c("LN", "MN", "HN"), each = 2), 3),
                       co2 = c("A", "E"))
treatment$treatment = paste0(substr(treatment$year, 3, 4), treatment$nutrient, treatment$co2)
treatment = treatment[!(treatment$year == 1998 & treatment$nutrient == "LN"),]

i = 1
for(i in 1:nrow(treatment)) {
  observed.file = observed.a.file
  pattern = paste0(treatment$year[i], "_", treatment$co2[i])
  
  if(treatment$co2[i] == "E"){
    observed.file = observed.e.file
    pattern = paste0(treatment$year[i], treatment$co2[i])
    if(treatment$year[i] == 2000){
      pattern = paste0(treatment$year[i], treatment$co2[i], "-CO2")
    }
  }
  
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
