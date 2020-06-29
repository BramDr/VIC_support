library(fields)
library(raster)
library(ncdf4)
rm(list = ls())

soil.file = "../../../../Data/Primary/GGCMI/Phase3/soil/HWSD_soil_data_on_cropland_v1.01.nc"

quartz.out = "../../../../Data/Transformed/Soil/GGCMI/quartz_30min_global.RDS"
expt.out = "../../../../Data/Transformed/Soil/GGCMI/expt_30min_global.RDS"
ksat.out = "../../../../Data/Transformed/Soil/GGCMI/ksat_30min_global.RDS"
bulk.dens.out = "../../../../Data/Transformed/Soil/GGCMI/bulk_dens_30min_global.RDS"
Wcr.out = "../../../../Data/Transformed/Soil/GGCMI/Wcr_30min_global.RDS"
Wpwp.out = "../../../../Data/Transformed/Soil/GGCMI/Wpwp_30min_global.RDS"
bubble.out = "../../../../Data/Transformed/Soil/GGCMI/bubble_30min_global.RDS"
soil.dens.out = "../../../../Data/Transformed/Soil/GGCMI/soil_dens_30min_global.RDS"
resid.moist.out = "../../../../Data/Transformed/Soil/GGCMI/resid_moist_30min_global.RDS"

nc = nc_open(soil.file)
clay = ncvar_get(nc, "clay")
sand = ncvar_get(nc, "sand")
silt = ncvar_get(nc, "silt")
bulk = ncvar_get(nc, "bulk_density")
nc_close(nc)

calc.cosby.multivariate = function(clay, silt, sand) {
  maps = list()
  
  maps[["b"]] = 3.10 + clay * 0.157 + sand * -0.003
  maps[["ys"]] = 10 ^ (1.54 + sand * -0.0095 + silt * 0.0063)
  maps[["ks"]] = 10 ^ (-0.60 + sand * 0.0126 + clay * -0.0064)
  maps[["vs"]] = 50.5 + sand * -0.142 + clay * -0.037
  
  return(maps)
}
calc.cosby.univariate = function(clay, silt, sand) {
  maps = list()
  
  maps[["b"]] = 2.91 + clay * 0.159
  maps[["ys"]] = 10 ^ (1.88 + sand * -0.0131)
  maps[["ks"]] = 10 ^ (-0.884 + sand * 0.0153)
  maps[["vs"]] = 48.9 + sand * -0.126
  
  return(maps)
}
calc.saxton = function(clay, silt, sand) {
  maps = list()
  
  maps[["a"]] = exp(-4.396 + clay * -0.0715 + sand ^ 2 * -4.880e-4 + sand ^ 2 * clay * -4.285e-5) * 100.0
  maps[["b"]] = -3.140 + (clay ^ 2) * -0.00222 + (sand ^ 2) * clay * -3.484e-5
  maps[["vs"]] = 0.332 + sand * -7.251e-4 + log(clay, base = 10) * 0.1276
  maps[["vs"]][maps[["vs"]] <= 0] = NA
  maps[["ys"]] = 100.0 * (-0.108 + maps[["vs"]] * 0.341)
  maps[["ks"]] = 2.778e-6 * exp(12.012 + sand * -0.0755 + ((-3.8950 + sand * 0.03671 + clay * -0.1103 + (clay ^ 2) * 8.7546e-4) / maps[["vs"]]))
  
  return(maps)
}

cosby.m = calc.cosby.multivariate(clay, silt, sand)
cosby.u = calc.cosby.univariate(clay, silt, sand)
saxton = calc.saxton(clay, silt, sand)

trans.map = function(map){
  map.t = array(NA, dim = c(dim(map)))
  for(z in 1:dim(map)[3]){
    map.t[,,z] = map[,dim(map)[2]:1,z]
  }
  return(map.t)
}
agg.layers = function(maps) {
  agg.map = array(NA, dim = c(720, 360, 3))
  agg.map[,,1] = maps
  agg.map[,,2] = maps
  agg.map[,,3] = agg.map[,,2]
  #agg.map[agg.map == 0] = NA
  return(trans.map(agg.map))
}
plot.layers = function(map) {
  for(z in 1:dim(map)[3]){
    image.plot(map[,,z], main = paste0("l = ", z))
  }
}

quartz = sand / 100
quartz = agg.layers(quartz)
#plot.layers(quartz)

expt = 2 * -saxton$b + 3
expt = agg.layers(expt)
#plot.layers(expt)

ksat = saxton$ks * 1000 * 60 * 60 * 24
ksat = agg.layers(ksat)
#plot.layers(ksat)

resid.moist = (1000000 / saxton$a) ^ (1 / saxton$b)
Wpwp = (1500 / saxton$a) ^ (1 / saxton$b) / saxton$vs
Wfc = (33 / saxton$a) ^ (1 / saxton$b) / saxton$vs
Wcr = Wpwp + 0.7 * (Wfc - Wpwp)

Wpwp[Wpwp > 1 | Wpwp < 0] = NA
Wpwp = agg.layers(Wpwp)
#plot.layers(Wpwp)

resid.moist[resid.moist > 1 | resid.moist < 0] = NA
resid.moist = agg.layers(resid.moist)
resid.moist[!is.na(resid.moist) & !is.na(Wpwp) & resid.moist > Wpwp] = Wpwp[!is.na(resid.moist) & !is.na(Wpwp) & resid.moist > Wpwp] - 1e-6
#plot.layers(resid.moist)

Wcr[Wcr > 1 | Wcr < 0] = NA
Wcr = agg.layers(Wcr)
Wcr[!is.na(Wcr) & !is.na(Wpwp) & Wcr < Wpwp] = Wpwp[!is.na(Wcr) & !is.na(Wpwp) & Wcr < Wpwp] + 1e-6
#plot.layers(Wcr)

bulk.dens = agg.layers(bulk * 1e3)
#plot.layers(bulk.dens)

bubble = saxton$ys / 0.0980665
bubble[bubble < 0] = 0
bubble = agg.layers(bubble)
#plot.layers(bubble)

soil.dens = (bulk * 1e3) / (-saxton$vs + 1)
soil.dens = agg.layers(soil.dens)
#plot.layers(soil.dens)

dir.create(dirname(quartz.out))
dir.create(dirname(expt.out))
dir.create(dirname(ksat.out))
dir.create(dirname(Wcr.out))
dir.create(dirname(Wpwp.out))
dir.create(dirname(bulk.dens.out))
dir.create(dirname(bubble.out))
dir.create(dirname(soil.dens.out))
dir.create(dirname(resid.moist.out))

saveRDS(quartz, quartz.out)
saveRDS(expt, expt.out)
saveRDS(ksat, ksat.out)
saveRDS(Wcr, Wcr.out)
saveRDS(Wpwp, Wpwp.out)
saveRDS(bulk.dens, bulk.dens.out)
saveRDS(bubble, bubble.out)
saveRDS(soil.dens, soil.dens.out)
saveRDS(resid.moist, resid.moist.out)

