library(openxlsx)
rm(list = ls())

soil.file <- "../../../../../Data/Primary/FACE/Hasegawa2017/Wuxi_FACE_R/Crop_soil_Wuxi_01_03_elevated_corrected_130502.xlsx"
global.file <- "../../../../../Data/Transformed/Soil/ISRIC/ph_30min_global.RDS"
soil.out <- "./Saves/soil_Wuxi.RDS"

point <- c(31.616667, 120.466667) # lat-lon
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

calc.saxton <- function(clay, silt, sand) {
  maps <- list()
  
  maps[["a"]] <- exp(-4.396 + clay * -0.0715 + sand^2 * -4.880e-4 + sand^2 * clay * -4.285e-5) * 100.0
  maps[["b"]] <- -3.140 + (clay^2) * -0.00222 + (sand^2) * clay * -3.484e-5
  maps[["vs"]] <- 0.332 + sand * -7.251e-4 + log(clay, base = 10) * 0.1276
  maps[["vs"]][maps[["vs"]] <= 0] <- NA
  maps[["ys"]] <- 100.0 * (-0.108 + maps[["vs"]] * 0.341)
  maps[["ks"]] <- 2.778e-6 * exp(12.012 + sand * -0.0755 + ((-3.8950 + sand * 0.03671 + clay * -0.1103 + (clay^2) * 8.7546e-4) / maps[["vs"]]))
  
  return(maps)
}
agg.layers = function(data){
  out.data = rep(data, 3)
  return(out.data)
}

global = readRDS(global.file)
global = t(global[nrow(global):1,,1])
x = which.min(abs(lons - point[2]))
y = which.min(abs(lats - point[1]))
ph = c(global[x,y], NA, NA)

soil = read.xlsx(soil.file, sheet = "Soil")
soil = soil[3,]
soil = as.data.frame(matrix(as.numeric(soil), ncol = ncol(soil)))
colnames(soil) = c("BULK", "", "", "TC", "TN", "CNRAT", "SAND", "", "", "SILT", "CLAY")

saxton = calc.saxton(soil$CLAY, soil$SILT, soil$SAND)

quartz <- agg.layers(soil$SAND) / 100
expt <- agg.layers(2 * -saxton$b + 3)
ksat <- agg.layers(saxton$ks * 1000 * 60 * 60 * 24) # um/s to mm/day
resid.moist <- agg.layers((1000000 / saxton$a)^(1 / saxton$b))
Wpwp <- agg.layers((1500 / saxton$a)^(1 / saxton$b))
Wcr <- agg.layers((100 / saxton$a)^(1 / saxton$b))
Wfc <- agg.layers((33 / saxton$a)^(1 / saxton$b))
max.moist <- agg.layers(saxton$vs)
bubble <- agg.layers(saxton$ys / 0.0980665)
bulk.dens <- agg.layers(soil$BULK * 1e3) # g/cm3 to kg/m3
ocar = agg.layers(soil$TC) # g[C]/kg[soil]

# Correct fractions
sel = !is.na(Wpwp) & !is.na(max.moist) & Wpwp > max.moist
Wpwp[sel] = max.moist[sel]
sel = !is.na(Wcr) & !is.na(max.moist) & Wcr > max.moist
Wcr[sel] = max.moist[sel]
sel = !is.na(Wfc) & !is.na(max.moist) & Wfc > max.moist
Wfc[sel] = max.moist[sel]

sel = !is.na(Wpwp) & !is.na(resid.moist) & Wpwp < resid.moist
Wpwp[sel] = resid.moist[sel] + 1e-6
sel = !is.na(Wcr) & !is.na(Wpwp) & Wcr < Wpwp
Wcr[sel] = Wpwp[sel] + 1e-6
sel = !is.na(Wfc) & !is.na(Wcr) & Wfc < Wcr
Wfc[sel] = Wcr[sel] + 1e-6

Wpwp <- Wpwp / max.moist
Wcr <- Wcr / max.moist
Wfc <- Wfc / max.moist

soil.list = list(quartz = quartz, 
                 expt = expt, 
                 ksat = ksat, 
                 resid.moist = resid.moist, 
                 Wpwp = Wpwp, 
                 Wcr = Wcr, 
                 Wfc = Wfc, 
                 max.moist = max.moist, 
                 bubble = bubble, 
                 bulk.dens = bulk.dens, 
                 ocar = ocar,
                 ph = ph)

dir.create(dirname(soil.out))
saveRDS(soil.list, soil.out)
