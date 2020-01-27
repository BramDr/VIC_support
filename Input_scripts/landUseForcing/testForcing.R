library(ncdf4)
library(fields)
rm(list = ls())

param.file = "/home/bram/Data/VIC/parameters/Wageningen/VIC_params_Wageningen.nc"
out.file = "/home/bram/Data/VIC/forcing/Wageningen/coverage_monthly_Wageningen/coverage_monthly_Wageningen_"
years = 1979:2016

nc = nc_open(param.file)
Cv = ncvar_get(nc, nc$var$Cv)
Nveg = ncvar_get(nc, nc$var$Nveg)
nc_close(nc)

Cv.t1 = c(0.01,0,0,0,0,0.02,0.161,0,0,0.218,0.590,0.001)
Cv.t2 = c(0.05,0,0,0,0,0.05,0.05,0,0,0.05,0.05,0.75)
Cv.t3 = c(0,0,0,0,0,0,0,0,0,0,0,0)

Cv.f = array(NA, dim = c(nc$dim$veg_class$len, 12))
Cv.f[,1] = Cv
Cv.f[,2:5] = Cv.t1
Cv.f[,6:10] = Cv.t2
Cv.f[,10:12] = Cv.t3

dim.lat = nc$dim$lat
dim.lon = nc$dim$lon
dim.veg = nc$dim$veg
for(year in years){
  time = as.Date(paste0(year, "-", 1:12, "-01"))
  dim.time = ncdim_def(name = "time",
                       units = "days since 1970-01-01",
                       vals = as.numeric(time),
                       calendar = "standard")
  var = ncvar_def(name = "Cv",
                  units = "-",
                  dim = list(dim.lon, dim.lat, dim.veg, dim.time),
                  missval = -1)
    
  out.file.f = paste0(out.file, year, ".nc")
  
  dir.create(dirname(out.file.f), showWarnings = F)
  nc = nc_create(out.file.f, vars = list(var))
  ncvar_put(nc, nc$var$Cv, Cv.f)
  nc_close(nc)
}