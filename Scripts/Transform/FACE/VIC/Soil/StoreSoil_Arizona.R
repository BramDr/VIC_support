library(readODS)
rm(list = ls())

soil.file <- "../../../../../Data/Primary/FACE/Kimball2017/Biomass Yield Area Phenology Management Weather Soil Moisture.ods"
global.ph.file <- "../../../../../Data/Transformed/Soil/ISRIC/ph_30min_global.RDS"
global.ocar.file <- "../../../../../Data/Transformed/Soil/ISRIC/ocar_30min_global.RDS"
soil.out <- "./Saves/soil_Arizona.RDS"

point <- c(33.0628, -111.9826) # lat-lon
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
  out.data = rep(NA, 3)
  out.data[1] = (data[1] * 5 + data[2] * 10 + data[3] * 15) / 30
  out.data[2] = (data[4] * 20 + data[5] * 10 + data[6] * 10 + data[7] * 20 + data[8] * 20 + data[9] * 10 + data[10] * 10) / 100
  out.data[3] = out.data[2]
  return(out.data)
}

global.ph = readRDS(global.ph.file)
global.ph = t(global.ph[nrow(global.ph):1,,1])
x = which.min(abs(lons - point[2]))
y = which.min(abs(lats - point[1]))
ph = c(global.ph[x,y], NA, NA)

global.ocar = readRDS(global.ocar.file)
global.ocar = t(global.ocar[nrow(global.ocar):1,,1])
x = which.min(abs(lons - point[2]))
y = which.min(abs(lats - point[1]))
ocar = c(global.ocar[x,y], NA, NA)
  
soil = read_ods(path = soil.file, sheet = "Soil_layers")
soil = soil[1:14,1:14]
layers = nrow(soil)

soil$SLSAN = 1 - soil$SLCLY - soil$SLSIL
saxton = calc.saxton(soil$SLCLY * 100, soil$SLSIL * 100, soil$SLSAN * 100)

quartz <- agg.layers(soil$SLSAN)
expt <- agg.layers(2 * -saxton$b + 3)
#ksat <- agg.layers(saxton$ks * 1e3 * 60 * 60 * 24) # m/s to mm/day
ksat <- agg.layers(soil$SKSAT * 1e-3 * 60 * 60 * 24) # um/s to mm/day
resid.moist <- agg.layers((1000000 / saxton$a)^(1 / saxton$b))
Wpwp <- agg.layers((1500 / saxton$a)^(1 / saxton$b))
Wcr.s <- agg.layers((100 / saxton$a)^(1 / saxton$b))
Wcr <- agg.layers(as.numeric(soil$slll))
Wfc.s <- agg.layers((33 / saxton$a)^(1 / saxton$b))
Wfc <- agg.layers(as.numeric(soil$sldul))
max.moist.s <- agg.layers(saxton$vs)
max.moist <- agg.layers(soil$slsat)
bubble <- agg.layers(saxton$ys / 0.0980665)
bulk.dens <- agg.layers(soil$slbdm * 1e3) # g/cm3 to kg/m3
#ocar = agg.layers(soil$sloc * 1e1) # g[C]/100g[soil] to g[C]/kg[soil]

Wpwp = Wpwp - (Wcr.s - Wcr)

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
