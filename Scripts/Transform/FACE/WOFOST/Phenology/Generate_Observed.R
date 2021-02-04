library(readODS)
rm(list = ls())

observed.file <- "../../../../../Data/Primary/FACE/Kimball2017/Biomass Yield Area Phenology Management Weather Soil Moisture.ods"
out.phenology <- "./Saves/phenology_observed.RDS"

plant = read_ods(path = observed.file, sheet = "Plantings")
observed = read_ods(path = observed.file, sheet = "Obs_summary_Avg_over_Reps")
repetitions = read_ods(path = observed.file, sheet = "Obs_daily_by_Reps")

plant$PDATE = as.Date(plant$PDATE)
observed$ADAT = as.Date(observed$ADAT)
observed$MDAT = as.Date(observed$MDAT)
repetitions$DATE = as.Date(repetitions$DATE)

emergence = aggregate(formula = GSTHD ~ DATE + TRNO, data = repetitions, FUN = mean)
min.emergence = aggregate(formula = GSTHD ~ TRNO, data = emergence, FUN = min)
emergence = merge(emergence, min.emergence)

phenology = data.frame(treatment = plant$TRNO, 
                       plant = plant$PDATE, 
                       emergence = emergence$DATE,
                       anthesis = observed$ADAT, 
                       maturity = observed$MDAT)
dir.create(dirname(out.phenology))
saveRDS(phenology, out.phenology)
