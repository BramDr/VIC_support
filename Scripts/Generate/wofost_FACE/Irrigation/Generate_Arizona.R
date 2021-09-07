library(readODS)
rm(list = ls())

observed.file <- "../../../../Data/Primary/FACE/Kimball2017/Biomass Yield Area Phenology Management Weather Soil Moisture.ods"
out.irrigation <- "./Saves/irrigation_Arizona.RDS"

irrigation = read_ods(path = observed.file, sheet = "Irrigations")

irrigation$idate = as.Date(irrigation$idate)

irrigation = data.frame(treatment = irrigation$TRNO, date = irrigation$idate, irr = irrigation$IRVAL)
dir.create(dirname(out.irrigation))
saveRDS(irrigation, out.irrigation)
