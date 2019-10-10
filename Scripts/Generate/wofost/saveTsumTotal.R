library(ncdf4)
library(fields)
rm(list = ls())

# Input
temp.file = "../../../Data/Transformed/WFDEI/tas_daily_WFDEI_1979.nc"
cc.file = "../../../Data/Primary/MIRCA2000/Growing periods listed/cropping_calendars_30min.txt"

# Load
cc <- read.table(file = cc.file, header = TRUE, stringsAsFactors = F)
cc$rowname <- 1:nrow(cc)

nc = nc_open(temp.file)
temp = ncvar_get(nc, nc$var$tas)
lats = nc$dim$lat$vals
lons = nc$dim$lon$vals
time = as.Date(nc$dim$time$vals / 24, origin = "1979-01-01")
time = as.Date(format.Date(time, "%Y-%m-%d"))
nc_close(nc)

# Setup
cc.sel = cc[cc$crop %in% c(28),]
maize.tsum = c(900, 1000)
maize.tbase = data.frame(temperature = c(0,6,30,35), increase = c(0,0,24,24))

get.tincrease = function(temp, sensitivity){
  ti = c()
  
  for(t in temp){
    
    for(i in 1:nrow(sensitivity)){
      
      done = F
      if(t < sensitivity$temperature[i]){
        if(i == 1){
          ti = c(ti, sensitivity$increase[1])
          
          done = T
          break
        } else {
          di = sensitivity$increase[i] - sensitivity$increase[i - 1]
          dt = sensitivity$temperature[i] - sensitivity$temperature[i - 1]
          ctemp = t - sensitivity$temperature[i - 1]
          
          ti = c(ti, sensitivity$increase[i - 1] + di * ctemp / dt)
          
          done = T
          break
        }
      }
    }
    
    if(!done){
      ti = c(ti, sensitivity$increase[i])
    }
  }
  
  return(ti)
}

# Calculate
## TEMPERATURE
#image.plot(apply(temp, c(1,2), sum))

## GROWING DAYS
sday = array(NA, dim = c(dim(temp)[1:2], max(cc.sel$subcrop)))
eday = array(NA, dim = c(dim(temp)[1:2], max(cc.sel$subcrop)))
gdays = array(NA, dim = c(dim(temp)[1:2], max(cc.sel$subcrop)))
for(i in 1:nrow(cc.sel)){
  x = which(lons == cc.sel$lon[i])
  y = which(lats == cc.sel$lat[i])
  
  if(cc.sel$start[i] < cc.sel$end[i]) {
    start = as.Date(paste0("1979-", cc.sel$start[i], "-15"))
    end = as.Date(paste0("1979-", cc.sel$end[i], "-20"))
    
    grow.time = seq.Date(start, end, by = "day")
  } else {
    start = as.Date(paste0("1979-", cc.sel$start[i], "-15"))
    end = as.Date(paste0("1980-", cc.sel$end[i], "-20"))
    
    grow.time = seq.Date(start, end, by = "day")
    grow.time = as.Date(paste0("1979-", format.Date(grow.time, "%m-%d")))
  }
  z = which(time %in% grow.time)
  
  sday[x,y,cc.sel$subcrop[i]] = as.numeric(format.Date(start, "%m"))
  eday[x,y,cc.sel$subcrop[i]] = as.numeric(format.Date(end, "%m"))
  gdays[x,y,cc.sel$subcrop[i]] = length(z)
}
image.plot(sday[,,1])
image.plot(eday[,,1])
image.plot(gdays[,,1])

## TSUMS
nodata = array(0, dim = dim(temp)[1:2])
tsum = array(NA, dim = c(dim(temp)[1:2], max(cc.sel$subcrop)))
for(i in 1:nrow(cc.sel)){
  x = which(lons == cc.sel$lon[i])
  y = which(lats == cc.sel$lat[i])
  
  if(cc.sel$start[i] < cc.sel$end[i]) {
    start = as.Date(paste0("1979-", cc.sel$start[i], "-15"))
    end = as.Date(paste0("1979-", cc.sel$end[i], "-20"))
    
    grow.time = seq.Date(start, end, by = "day")
  } else {
    start = as.Date(paste0("1979-", cc.sel$start[i], "-15"))
    end = as.Date(paste0("1980-", cc.sel$end[i], "-20"))
    
    grow.time = seq.Date(start, end, by = "day")
    grow.time = as.Date(paste0("1979-", format.Date(grow.time, "%m-%d")))
  }
  z = which(time %in% grow.time)
  
  t = temp[x,y,z]
  
  if(length(na.omit(t)) == 0){
    nodata[x,y] = 1
    next
  }
  
  ti = get.tincrease(t, maize.tbase)
  
  tsum[x,y,cc.sel$subcrop[i]] = sum(ti)
}
image.plot(nodata)
image.plot(tsum[,,1], zlim = c(0,5000))
image.plot(tsum[,,1] <= 1000)
