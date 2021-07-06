library(openxlsx)
rm(list = ls())

observed.file <- "../../../../Data/Primary/FACE/Hasegawa2017/Wuxi_FACE_R/Wuxi_Onsite_weather_01_03_with_CO2_A_E_corrected_130502.xlsx"
out.co2 <- "./Saves/co2_Wuxi.RDS"

treatment = data.frame(year = rep(2001:2003, each = 6),
                       nutrient = rep(rep(c("LN", "MN", "HN"), each = 2), 3),
                       co2 = c("A", "E"))
treatment$treatment = paste0(substr(treatment$year, 3, 4), treatment$nutrient, treatment$co2)
treatment = treatment[!(treatment$year == 2001 & treatment$nutrient == "HN"),]

co2 = read.xlsx(observed.file, sheet = "Climate01_03")

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
