library(readODS)
rm(list = ls())

out.irrigation <- "./Saves/irrigation_Wuxi.RDS"

treatment = data.frame(year = rep(2001:2003, each = 6),
                       nutrient = rep(rep(c("LN", "MN", "HN"), each = 2), 3),
                       co2 = c("A", "E"))
treatment$treatment = paste0(substr(treatment$year, 3, 4), treatment$nutrient, treatment$co2)
treatment = treatment[!(treatment$year == 2001 & treatment$nutrient == "HN"),]

irrigation = data.frame(treatment = treatment$treatment, date = "0000-01-01", irr = 0)
dir.create(dirname(out.irrigation))
saveRDS(irrigation, out.irrigation)
