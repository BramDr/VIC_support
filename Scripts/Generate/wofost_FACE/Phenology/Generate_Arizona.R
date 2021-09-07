library(readODS)
rm(list = ls())

observed.file <- "../../../../Data/Primary/FACE/Kimball2017/Biomass Yield Area Phenology Management Weather Soil Moisture.ods"
out.phenology <- "./Saves/phenology_Arizona.RDS"

plant = read_ods(path = observed.file, sheet = "Plantings")
emergence = read_ods(path = observed.file, sheet = "Env_modifications")
observed = read_ods(path = observed.file, sheet = "Obs_summary_Avg_over_Reps")

plant$PDATE = as.Date(plant$PDATE)
observed$ADAT = as.Date(observed$ADAT)
observed$MDAT = as.Date(observed$MDAT)
emergence$emdate = as.Date(emergence$emdate)
emergence = aggregate(formula = emdate ~ TRNO, data = emergence, FUN = min)

phenology = data.frame(treatment = plant$TRNO, 
                       plant = plant$PDATE, 
                       emergence = emergence$emdate,
                       anthesis = observed$ADAT, 
                       maturity = observed$MDAT)
dir.create(dirname(out.phenology))
saveRDS(phenology, out.phenology)
