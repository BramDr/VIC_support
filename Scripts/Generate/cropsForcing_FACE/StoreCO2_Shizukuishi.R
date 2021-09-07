library(openxlsx)
rm(list = ls())

observed.file <- "../../../../Data/Primary/FACE/Yang2006/Shizukuishi_FACE_98_00/Shizukuishi_Onsite_weather_98_00_with_CO2_A_E.xlsx"
out.co2 <- "./Saves/co2_Shizukuishi.RDS"

treatment = data.frame(year = rep(1998:2000, each = 6),
                       nutrient = rep(rep(c("LN", "MN", "HN"), each = 2), 3),
                       co2 = c("A", "E"))
treatment$treatment = paste0(substr(treatment$year, 3, 4), treatment$nutrient, treatment$co2)
treatment = treatment[!(treatment$year == 1998 & treatment$nutrient == "LN"),]

co2 = read.xlsx(observed.file, sheet = "Climate98_00")

co2.df = data.frame(treatment = treatment$treatment, 
                    co2 = NA, 
                    stringsAsFactors = F)

year = treatment$year[1]
for(year in treatment$year){
  co2.a.year = as.numeric(co2$CO2_ambient[co2$Year == year])
  co2.a.year = mean(co2.a.year, na.rm = T)
  co2.e.year = as.numeric(co2$CO2_elevated[co2$Year == year])
  co2.e.year = mean(co2.e.year, na.rm = T)
  
  co2.df$co2[treatment$year == year & treatment$co2 == "A"] = co2.a.year
  co2.df$co2[treatment$year == year & treatment$co2 == "E"] = co2.e.year
}

dir.create(dirname(out.co2))
saveRDS(co2.df, out.co2)
