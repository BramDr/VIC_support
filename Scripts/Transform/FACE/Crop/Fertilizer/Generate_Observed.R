library(readODS)
rm(list = ls())

observed.file <- "../../../../../Data/Primary/FACE/Kimball2017/Biomass Yield Area Phenology Management Weather Soil Moisture.ods"
out.fertilizer <- "./Saves/fertilizer_observed.RDS"

fertilizer = read_ods(path = observed.file, sheet = "Fertilizations")

fertilizer$FEDATE = as.Date(fertilizer$FEDATE)

fertilizer = data.frame(treatment = fertilizer$TRNO, date = fertilizer$FEDATE, N = fertilizer$FEAMN, P = fertilizer$FEAMP)
dir.create(dirname(out.fertilizer))
saveRDS(fertilizer, out.fertilizer)
