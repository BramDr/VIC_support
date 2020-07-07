library(fields)
library(ncdf4)
rm(list = ls())

# Input
tnit.file = "../../../../Data/Transformed/Soil/ISRIC/tnit_30min_global.RDS"
bulk.file = "../../../../Data/Transformed/Soil/ISRIC/bulk_30min_global.RDS"
temp.file = "../../../../Data/Transformed/VIC/fluxes_VICWURsaxtonvarying.nat.ymonmean.1981-01.nc"
moist.file = "../../../../Data/Transformed/VIC/fluxes_VICWURsaxtonvarying.nat.ymonmean.1981-01.nc"
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

tnit = readRDS(tnit.file)
tnit[tnit<0] = NA

bulk = readRDS(bulk.file)
bulk[bulk<=0] = NA

plant = readRDS(plant.file)
harvest = readRDS(harvest.file)

nc = nc_open(temp.file)
temp = ncvar_get(nc, "OUT_SOIL_TEMP", start = c(1,1,1,1), count = c(-1,-1,1,-1), collapse_degen = F)
nc_close(nc)
temp.mean = apply(X = temp, MARGIN = c(1,2), FUN = mean)
image.plot(temp.mean)

nc = nc_open(moist.file)
moist = ncvar_get(nc, "OUT_SOIL_EFF_SAT", start = c(1,1,1,1), count = c(-1,-1,1,-1), collapse_degen = F)
nc_close(nc)
moist.mean = apply(X = moist, MARGIN = c(1,2), FUN = mean)
image.plot(moist.mean)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

calcFactor = function(temp, moist){
  return(temp * 
          (0.04021601 - 
             5.00505434 * moist ^ 3 + 
             4.26937932 * moist ^ 2 + 
             0.71890122 * moist))
}

# Calculate
mineral.n = array(NA, dim = c(length(lons), length(lats), nrow(crops)))
mineral.p = array(NA, dim = c(length(lons), length(lats), nrow(crops)))
mineral.k = array(NA, dim = c(length(lons), length(lats), nrow(crops)))
recovery.n = array(NA, dim = c(length(lons), length(lats), nrow(crops)))
recovery.p = array(NA, dim = c(length(lons), length(lats), nrow(crops)))
recovery.k = array(NA, dim = c(length(lons), length(lats), nrow(crops)))

i = 1
for(i in 1:nrow(crops)) {
  print(crops$name[i])
    
  x = 140
  y = 250
  temp.crop = array(NA, dim = dim(plant)[1:2])
  moist.crop = array(NA, dim = dim(plant)[1:2])
  for(x in 1:dim(plant)[1]) {
    for(y in 1:dim(plant)[2]) {
      if(is.na(plant[x,y,i])){
        next
      }
      
      start.month = as.Date(plant[x,y,i], origin = "0000-12-31")
      start.month = as.numeric(format.Date(start.month, "%m"))
      end.month = as.Date(harvest[x,y,i], origin = "0000-12-31")
      end.month = as.numeric(format.Date(end.month, "%m"))
      
      if(start.month < end.month){
        temp.sel = mean(temp[x,y,,start.month:end.month])
        moist.sel = mean(moist[x,y,,start.month:end.month])
      } else {
        temp.sel = mean(temp[x,y,,c(end.month:12, 1:start.month)])
        moist.sel = mean(moist[x,y,,c(end.month:12, 1:start.month)])
      }
      
      temp.crop[x,y] = temp.sel
      moist.crop[x,y] = moist.sel
    }
  }
  image.plot(temp.crop)
  image.plot(moist.crop)
  
  factor = 1 - exp(-0.001 * calcFactor(temp.crop, moist.crop))
  factor[factor < 0] = 0
  #image.plot(factor)

  tnit.40cm = apply(X = tnit[,,1:2], MARGIN = c(1,2), FUN = mean, na.rm = T)
  bulk.40cm = apply(X = bulk[,,1:2], MARGIN = c(1,2), FUN = mean, na.rm = T)

  tnit.adj = t(tnit.40cm[dim(tnit.40cm)[1]:1,])
  #image.plot(tnit.adj)
  bulk.adj = t(bulk.40cm[dim(bulk.40cm)[1]:1,])
  #image.plot(bulk.adj)

  min.n = tnit.adj / 1000 * bulk.adj * 1000 * 10000 * factor * 0.3 # g kg-1 -> kg ha-1 (0.3 m deep)
  image.plot(min.n, zlim = c(0,500))

  min.p = min.k = array(999999.9, dim = dim(min.n))
  rec.n = array(1 / 365, dim = dim(min.n))
  rec.p = rec.k = array(1, dim = dim(min.n))
  
  # image.plot(moist.mean - moist.crop)
  # image.plot(temp.mean - temp.crop)
  # image.plot(factor - (1 - exp(-0.001 * calcFactor(temp.mean, moist.mean))))
  
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

