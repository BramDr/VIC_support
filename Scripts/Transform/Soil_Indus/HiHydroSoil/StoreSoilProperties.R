rm(list = ls())
library(fields)
library(raster)

ksat.files = c("./Saves/Ksat_M_250m_TOPSOIL.RDS", "./Saves/Ksat_M_250m_SUBSOIL.RDS")
wfc.files = c("./Saves/WCpF2_M_250m_TOPSOIL.RDS", "./Saves/WCpF2_M_250m_SUBSOIL.RDS")
wpwp.files = c("./Saves/WCpF4.2_M_250m_TOPSOIL.RDS", "./Saves/WCpF4.2_M_250m_SUBSOIL.RDS")
wcr.files = c("./Saves/WCpF3_M_250m_TOPSOIL.RDS", "./Saves/WCpF3_M_250m_SUBSOIL.RDS")
max.moist.files = c("./Saves/WCsat_M_250m_TOPSOIL.RDS", "./Saves/WCsat_M_250m_SUBSOIL.RDS")
res.moist.files = c("./Saves/WCres_M_250m_TOPSOIL.RDS", "./Saves/WCres_M_250m_SUBSOIL.RDS")
ksat.out = "/home/bram/Data/Transformed/Soil/HiHydroSoil/ksat_5min_Indus.RDS"
wfc.out = "/home/bram/Data/Transformed/Soil/HiHydroSoil/wfc_5min_Indus.RDS"
wpwp.out = "/home/bram/Data/Transformed/Soil/HiHydroSoil/wpwp_5min_Indus.RDS"
wcr.out = "/home/bram/Data/Transformed/Soil/HiHydroSoil/wcr_5min_Indus.RDS"
max.moist.out = "/home/bram/Data/Transformed/Soil/HiHydroSoil/max_moist_5min_Indus.RDS"
res.moist.out = "/home/bram/Data/Transformed/Soil/HiHydroSoil/res_moist_5min_Indus.RDS"

# Load
template = readRDS(ksat.files[1])

ksat = array(NA, dim = c(dim(template), 3))
wfc = array(NA, dim = c(dim(template), 3))
wpwp = array(NA, dim = c(dim(template), 3))
wcr = array(NA, dim = c(dim(template), 3))
max.moist = array(NA, dim = c(dim(template), 3))
res.moist = array(NA, dim = c(dim(template), 3))

ksat[,,1] = readRDS(ksat.files[1]) * 1e1
ksat[,,2:3] = readRDS(ksat.files[2]) * 1e1
image.plot(ksat[,,1])
image.plot(ksat[,,2])

max.moist[,,1] = readRDS(max.moist.files[1])
max.moist[,,2:3] = readRDS(max.moist.files[2])
image.plot(max.moist[,,1])
image.plot(max.moist[,,2])

wfc[,,1] = readRDS(wfc.files[1])
wfc[,,2:3] = readRDS(wfc.files[2])
image.plot(wfc[,,1])
image.plot(wfc[,,2])

wpwp[,,1] = readRDS(wpwp.files[1])
wpwp[,,2:3] = readRDS(wpwp.files[2])
image.plot(wpwp[,,1])
image.plot(wpwp[,,2])

wcr[,,1] = readRDS(wcr.files[1])
wcr[,,2:3] = readRDS(wcr.files[2])
image.plot(wcr[,,1])
image.plot(wcr[,,2])

res.moist[,,1] = readRDS(res.moist.files[1])
res.moist[,,2:3] = readRDS(res.moist.files[2])
image.plot(res.moist[,,1])
image.plot(res.moist[,,2])

# Correct fractions
sel = !is.na(wpwp) & !is.na(res.moist) & wpwp < res.moist
wpwp[sel] = res.moist[sel] + 1e-6
sel = !is.na(wcr) & !is.na(wpwp) & wcr < wpwp
wcr[sel] = wpwp[sel] + 1e-6
sel = !is.na(wfc) & !is.na(wcr) & wfc < wcr
wfc[sel] = wcr[sel] + 1e-6

wpwp = wpwp / max.moist
wcr = wcr / max.moist
wfc = wfc / max.moist

dir.create(dirname(ksat.out))
dir.create(dirname(wfc.out))
dir.create(dirname(wpwp.out))
dir.create(dirname(wcr.out))
dir.create(dirname(max.moist.out))
dir.create(dirname(res.moist.out))

saveRDS(ksat, ksat.out)
saveRDS(wfc, wfc.out)
saveRDS(wpwp, wpwp.out)
saveRDS(wcr, wcr.out)
saveRDS(max.moist, max.moist.out)
saveRDS(res.moist, res.moist.out)

