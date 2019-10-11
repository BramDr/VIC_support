library(ncdf4)
library(fields)
rm(list = ls())

# Input
flux.file = "../../../Data/VIC/Output/global/fluxes_hydrolake_test_global.1979-01-01.nc"

nc = nc_open(flux.file)
lake.frac = ncvar_get(nc, "OUT_LAKE_ICE_FRACT")
lake.temp = ncvar_get(nc, "OUT_LAKE_ICE_TEMP")
lake.height = ncvar_get(nc, "OUT_LAKE_ICE_HEIGHT");
lake.depth = ncvar_get(nc, "OUT_LAKE_DEPTH");
lake.area = ncvar_get(nc, "OUT_LAKE_SURF_AREA");

lake.bf.in = ncvar_get(nc, "OUT_LAKE_BF_IN_V");
lake.bf.out = ncvar_get(nc, "OUT_LAKE_BF_OUT_V");
lake.run.in = ncvar_get(nc, "OUT_LAKE_RO_IN_V");
lake.run.out = ncvar_get(nc, "OUT_LAKE_CHAN_OUT_V");
lake.prec = ncvar_get(nc, "OUT_LAKE_PREC_V");
lake.evap = ncvar_get(nc, "OUT_LAKE_EVAP_V");
lake.rech = ncvar_get(nc, "OUT_LAKE_RCHRG_V");

lake.vol = ncvar_get(nc, "OUT_LAKE_VOLUME");
lake.dvol = ncvar_get(nc, "OUT_LAKE_DSTOR_V");

time = as.Date(nc$dim$time$vals, origin = "0000-12-30")
nc_close(nc)

lake.err = lake.dvol + lake.prec + lake.bf.in + lake.run.in - lake.bf.out - lake.run.out - lake.evap - lake.rech

par(mfrow=c(4,1))
for(x in 1:nc$dim$lon$len){
  for(y in 1:nc$dim$lat$len){
    if(is.na(lake.frac[x,y,1])){
      next
    }
    
    lfrac = lake.frac[x,y,]
    ltemp = lake.temp[x,y,]
    lheight = lake.height[x,y,]
    ldepth = lake.depth[x,y,]
    larea = lake.area[x,y,]
    
    err = sum(ldepth > 0.5 & lfrac == 1 & ltemp == 0)
    print(err)
    
    if(err > 0){
      # plot(time, lfrac, type = "l")
      # plot(time, ltemp, type = "l", lty = 2)
      # plot(time, lheight, type = "l", lty = 3)
      # plot(time, ldepth, type = "l", lty = 4)
      plot(time, larea, type = "l")
      plot(time, ldepth, type = "l")
    }
  }
}
par(mfrow=c(1,1))
