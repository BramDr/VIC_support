library(fields)
library(ncdf4)
rm(list = ls())

# Input
tnit.file = "../../../../Data/Transformed/Soil/ISRIC/tnit_30min_global.RDS"
bulk.file = "../../../../Data/Transformed/Soil/ISRIC/bulk_30min_global.RDS"
temp.file = "../../../../Data/Transformed/VIC/fluxes_VICWURsaxtonvarying.nat.1979-01.nc"
moist.file = "../../../../Data/Transformed/VIC/fluxes_VICWURsaxtonvarying.nat.1979-01.nc"
min.n.out = "./Saves/mineralizationN_30min_global.RDS"
min.p.out = "./Saves/mineralizationP_30min_global.RDS"
min.k.out = "./Saves/mineralizationK_30min_global.RDS"
rec.n.out = "./Saves/recoveryN_30min_global.RDS"
rec.p.out = "./Saves/recoveryP_30min_global.RDS"
rec.k.out = "./Saves/recoveryK_30min_global.RDS"

# Load
tnit = readRDS(tnit.file)
tnit[tnit<0] = NA

bulk = readRDS(bulk.file)
bulk[bulk<=0] = NA

nc = nc_open(temp.file)
temp = ncvar_get(nc, "OUT_SOIL_TEMP", start = c(1,1,1,24), count = c(-1,-1,2,-1))
nc_close(nc)
temp = apply(X = temp, MARGIN = c(1,2), FUN = mean)
image.plot(temp)

nc = nc_open(moist.file)
moist = ncvar_get(nc, "OUT_SOIL_EFF_SAT", start = c(1,1,1,24), count = c(-1,-1,2,-1))
nc_close(nc)
moist = apply(X = moist, MARGIN = c(1,2), FUN = mean)
image.plot(moist)

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
factor = 1 - exp(-0.001 * calcFactor(temp, moist))
factor[factor < 0] = 0
image.plot(factor)

tnit.1m = apply(X = tnit[,,1:5], MARGIN = c(1,2), FUN = mean, na.rm = T)
bulk.1m = apply(X = bulk[,,1:5], MARGIN = c(1,2), FUN = mean, na.rm = T)

tnit.adj = t(tnit.1m[dim(tnit.1m)[1]:1,])
image.plot(tnit.adj)
bulk.adj = t(bulk.1m[dim(bulk.1m)[1]:1,])
image.plot(bulk.adj)

min.n = tnit.adj / 1000 * bulk.adj * 1000 * 10000 * factor # g kg-1 -> kg ha-1
image.plot(min.n, zlim = c(0,500))

min.p = min.k = array(999999.9, dim = dim(min.n))
rec.n = array(1 / 365, dim = dim(min.n))
rec.p = rec.k = array(1, dim = dim(min.n))

# Save
dir.create(dirname(min.n.out))
saveRDS(min.n, min.n.out)
dir.create(dirname(min.p.out))
saveRDS(min.p, min.p.out)
dir.create(dirname(min.k.out))
saveRDS(min.k, min.k.out)
dir.create(dirname(rec.n.out))
saveRDS(rec.n, rec.n.out)
dir.create(dirname(rec.p.out))
saveRDS(rec.p, rec.p.out)
dir.create(dirname(rec.k.out))
saveRDS(rec.k, rec.k.out)

