library(readODS)
rm(list = ls())

out.irrigation <- "./Saves/irrigation_Shizukuishi.RDS"

treatment = data.frame(year = rep(1998:2000, each = 6),
                       nutrient = rep(rep(c("LN", "MN", "HN"), each = 2), 3),
                       co2 = c("A", "E"))
treatment$treatment = paste0(substr(treatment$year, 3, 4), treatment$nutrient, treatment$co2)
treatment = treatment[!(treatment$year == 1998 & treatment$nutrient == "LN"),]

irrigation = data.frame(treatment = treatment$treatment, date = "0000-01-01", irr = 0)
dir.create(dirname(out.irrigation))
saveRDS(irrigation, out.irrigation)
