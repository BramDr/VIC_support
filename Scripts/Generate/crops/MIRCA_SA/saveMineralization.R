library(fields)
library(raster)
library(ncdf4)
rm(list = ls())

ocar.file = "../../../../Data/Transformed/Soil/ISRIC/ocar_30min_global.RDS"
tnit.file = "../../../../Data/Transformed/Soil/ISRIC/tnit_30min_global.RDS"
cnrat.file = "../../../../Data/Transformed/Soil/ISRIC/cnrat_30min_global.RDS"
clay.file = "../../../../Data/Transformed/Soil/ISRIC/clay_30min_global.RDS"
ph.file = "../../../../Data/Transformed/Soil/ISRIC/ph_30min_global.RDS"
temp.file = "../../../../Data/Transformed/WFDEI/Tair_ymonmean_WFDEI_1979-2016.nc"
tfactor.file = "./Saves/tfactor_30min_global.RDS"
crop.file = "./Saves/crop_mapping.csv"
plant.file = "./Saves/plantDay_30min_global.RDS"
harvest.file = "./Saves/harvestDay_30min_global.RDS"
min.n.out = "./Saves/mineralizationN_30min_global.RDS"
min.p.out = "./Saves/mineralizationP_30min_global.RDS"
min.k.out = "./Saves/mineralizationK_30min_global.RDS"
rec.n.out = "./Saves/recoveryN_30min_global.RDS"
rec.p.out = "./Saves/recoveryP_30min_global.RDS"
rec.k.out = "./Saves/recoveryK_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
plant = readRDS(plant.file)
harvest = readRDS(harvest.file)
Tfactor = readRDS(tfactor.file)

clay = readRDS(clay.file)
ocar = readRDS(ocar.file)
tnit = readRDS(tnit.file)
cnrat = readRDS(cnrat.file)
ph = readRDS(ph.file)

nc = nc_open(temp.file)
temp = ncvar_get(nc, "Tair")
nc_close(nc)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

clay[clay <= 0] = NA
ocar[ocar <= 0] = NA
tnit[tnit <= 0] = NA
cnrat[cnrat <= 0] = NA
ph[ph <= 0] = NA

clay = t(clay[dim(clay)[1]:1,,1])
ocar = t(ocar[dim(ocar)[1]:1,,1])
tnit = t(tnit[dim(tnit)[1]:1,,1])
cnrat = t(cnrat[dim(cnrat)[1]:1,,1])
ph = t(ph[dim(ph)[1]:1,,1])
onit = ocar / cnrat

calcQUEFTS90 = function(ocar, ph){
  Nf = 0.25*(ph-3)
  aN = 6.8
  SN = aN * Nf * ocar
  return(SN)
}
calcQUEFTS93 = function(ocar, ph, temp, clay){
  Nf = 2 ^ ((temp - 9) / 9) / log(15*clay,10)
  aN = 4.5
  SN = aN * Nf * ocar
  return(SN)
}
calcQUEFTS04 = function(ocar, ph, temp){
  Nf = 0.25 * (ph-3)
  Nf[ph > 7] = 1
  Nf[ph < 4.7] = 0.4
  aN = 2 * 2^((temp - 9) / 9)
  SN = aN * Nf * ocar
  return(SN)
}

calcQUEFTS90.alt = function(onit, ph){
  Nf = 0.25*(ph-3)
  aN = 68
  SN = aN * Nf * onit
  return(SN)
}
calcQUEFTS93.alt = function(onit, ph, temp, clay){
  Nf = 2 ^ ((temp - 9) / 9) / log(15*clay,10)
  aN = 45
  SN = aN * Nf * onit
  return(SN)
}
calcQUEFTS04.alt = function(onit, ph, temp){
  Nf = 0.25 * (ph-3)
  Nf[ph > 7] = 1
  Nf[ph < 4.7] = 0.4
  aN = 20 * 2^((temp - 9) / 9)
  SN = aN * Nf * onit
  return(SN)
}

# Calculate
mineral.n = array(NA, dim = c(length(lons), length(lats), nrow(crops)))
mineral.p = array(NA, dim = c(length(lons), length(lats), nrow(crops)))
mineral.k = array(NA, dim = c(length(lons), length(lats), nrow(crops)))
recovery.n = array(NA, dim = c(length(lons), length(lats), nrow(crops)))
recovery.p = array(NA, dim = c(length(lons), length(lats), nrow(crops)))
recovery.k = array(NA, dim = c(length(lons), length(lats), nrow(crops)))

i = 6
for(i in 1:nrow(crops)) {
  print(crops$name[i])
  
  x = 140
  y = 250
  temp.crop = array(NA, dim = c(dim(plant)[1:2]))
  for(x in 1:dim(plant)[1]) {
    for(y in 1:dim(plant)[2]) {
      if(is.na(plant[x,y,i,6])){
        next
      }
      
      start.month = as.Date(plant[x,y,i,6], origin = "0000-12-31")
      start.month = as.numeric(format.Date(start.month, "%m"))
      end.month = as.Date(harvest[x,y,i,6], origin = "0000-12-31")
      end.month = as.numeric(format.Date(end.month, "%m"))
      
      if(start.month < end.month){
        temp.sel = temp[x,y,start.month:end.month]
      } else {
        temp.sel = temp[x,y,c(start.month:12, 1:end.month)]
      }
      
      temp.sel = mean(temp.sel)
      
      temp.crop[x,y] = temp.sel
    }
  }
  temp.crop = temp.crop - 273.15 + Tfactor
  #image.plot(temp.crop)
  
  # QUEFTS.90=calcQUEFTS90(ocar,ph)
  # image.plot(QUEFTS.90 / 120)
  # QUEFTS.93=calcQUEFTS93(ocar,ph,temp.mean,clay)
  # image.plot(QUEFTS.93 / 120)
  # QUEFTS.04=calcQUEFTS04(ocar,ph,temp.mean)
  # image.plot(QUEFTS.04 / 120, zlim = c(0,3))
  
  # QUEFTS.90=calcQUEFTS90.alt(onit,ph)
  # image.plot(QUEFTS.90 / 120)
  # QUEFTS.93=calcQUEFTS93.alt(onit,ph,temp.mean,clay)
  # image.plot(QUEFTS.93 / 120)
  # QUEFTS.04=calcQUEFTS04.alt(onit,ph,temp.mean)
  # image.plot(QUEFTS.04 / 120, zlim = c(0,3))
  
  min.n=calcQUEFTS04(ocar,ph,temp.crop) * 3
  image.plot(min.n / 360, zlim = c(0,3), main = paste0(crops$water[i], " ", crops$name[i], " ", crops$season[i]))
  
  min.p = min.k = array(999999.9, dim = dim(min.n))
  rec.n = array(1 / 360, dim = dim(min.n))
  rec.p = rec.k = array(1, dim = dim(min.n))
  
  mineral.n[,,i] = min.n
  mineral.p[,,i] = min.p
  mineral.k[,,i] = min.k
  recovery.n[,,i] = rec.n
  recovery.p[,,i] = rec.p
  recovery.k[,,i] = rec.k
}

# Save
dir.create(dirname(min.n.out))
saveRDS(mineral.n, min.n.out)
dir.create(dirname(min.p.out))
saveRDS(mineral.p, min.p.out)
dir.create(dirname(min.k.out))
saveRDS(mineral.k, min.k.out)
dir.create(dirname(rec.n.out))
saveRDS(recovery.n, rec.n.out)
dir.create(dirname(rec.p.out))
saveRDS(recovery.p, rec.p.out)
dir.create(dirname(rec.k.out))
saveRDS(recovery.k, rec.k.out)

