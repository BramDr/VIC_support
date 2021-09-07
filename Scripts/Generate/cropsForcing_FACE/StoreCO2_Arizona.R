library(readODS)
rm(list = ls())

observed.file <- "../../../../Data/Primary/FACE/Kimball2017/Biomass Yield Area Phenology Management Weather Soil Moisture.ods"
out.co2 <- "./Saves/co2_Arizona.RDS"

co2 = read_ods(path = observed.file, sheet = "Env_modifications")
co2 = co2[1:20,1:4]
co2$treatment = co2$TRNO
co2$co2 = co2$EMCO2
co2$co2[co2$ECCO2 == "A"] = co2$co2[co2$ECCO2 == "A"] + 370
co2 = co2[,c("treatment", "co2")]

dir.create(dirname(out.co2))
saveRDS(co2, out.co2)
