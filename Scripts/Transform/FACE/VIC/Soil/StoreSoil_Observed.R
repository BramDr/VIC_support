library(readODS)
rm(list = ls())

soil.file <- "../../../../Data/Primary/FACE/Kimball2017/Biomass Yield Area Phenology Management Weather Soil Moisture.ods"
out.clay <- "./Saves_observed/clay_observed.RDS"
out.sand <- "./Saves_observed/sand_observed.RDS"
out.silt <- "./Saves_observed/silt_observed.RDS"
out.bulk <- "./Saves_observed/bulk_observed.RDS"
out.ocar <- "./Saves_observed/ocar_observed.RDS"
out.ksat <- "./Saves_observed/ksat_observed.RDS"
out.poro <- "./Saves_observed/poro_observed.RDS"

soil = read_ods(path = soil.file, sheet = "Soil_layers")
soil = soil[1:14,1:14]
layers = nrow(soil)

clay = as.numeric(soil[,"SLCLY"])
silt = as.numeric(soil[,"SLSIL"])
sand = 1 - clay - silt
bulk = as.numeric(soil[,"slbdm"]) # kg/dm3
ocar = as.numeric(soil[,"sloc"]) * 1e1 # g[C]/100g[soil] to g[C]/kg[soil]
ksat = as.numeric(soil[,"SKSAT"]) * 60 * 60 * 24 * 1e-3 # µm/s to mm/d
poro = as.numeric(soil[,"slsat"]) # cm3/cm3

dir.create(dirname(out.clay))
dir.create(dirname(out.sand))
dir.create(dirname(out.silt))
dir.create(dirname(out.bulk))
dir.create(dirname(out.ocar))
dir.create(dirname(out.ksat))
dir.create(dirname(out.poro))

saveRDS(clay, out.clay)
saveRDS(sand, out.sand)
saveRDS(silt, out.silt)
saveRDS(bulk, out.bulk)
saveRDS(ocar, out.ocar)
saveRDS(ksat, out.ksat)
saveRDS(poro, out.poro)
