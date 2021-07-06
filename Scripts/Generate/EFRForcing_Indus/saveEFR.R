library(fields)
library(ncdf4)
library(zoo)
rm(list = ls())

# Input
efr.dir <- "../../../Data/Transformed/VIC/Indus/"
smooth.out <- "./Saves/smooth_5min_Indus.RDS"
efr.out <- "./Saves/efr_5min_Indus.RDS"

# Load
efr.files = list.files(efr.dir, pattern = "*naturalized*", full.names = T)

efr.file = efr.files[1]
for(efr.file in efr.files) {
  print(basename(efr.file))
  
  patterns = strsplit(basename(efr.file), "_")[[1]]
  model = patterns[3]
  scenario = patterns[4]
  syear = patterns[5]
  eyear = patterns[6]
  
  period.pattern = paste0(model, "_", scenario, "_", syear, "_", eyear)
  years = (as.numeric(syear) - 2):(as.numeric(eyear))
  
  efr.out.tmp <- gsub(x = efr.out, pattern = "_5min", replacement = paste0("_", period.pattern, "_5min"))
  smooth.out.tmp <- gsub(x = smooth.out, pattern = "_5min", replacement = paste0("_", period.pattern, "_5min"))
  if(file.exists(efr.out.tmp) && file.exists(smooth.out.tmp)){
    next
  }
  
  nc <- nc_open(efr.file)
  time <- as.Date(nc$dim$time$vals, "0000-12-30")
  doys <- as.numeric(format.Date(time, "%j"))
  lons = nc$dim$lon$vals
  lats = nc$dim$lat$vals
  discharge <- ncvar_get(nc, "OUT_DISCHARGE")
  nc_close(nc)

  # Calculate
  discharge.efr = array(NA, dim = dim(discharge))
  discharge.smooth = array(NA, dim = dim(discharge))
  x = 100
  y = 100
  for(x in 1:dim(discharge)[1]){
    print(x)
    for(y in 1:dim(discharge)[2]){
      if(is.na(discharge[x,y,1])){
        next
      }
      
      discharge.tmp = rep(discharge[x,y,], 3)
      smooth = rollapply(discharge.tmp, width = 30, FUN = median, probs = 0.5, fill = NA)
      smooth = smooth[(366+1):(366+366)]
      
      discharge.smooth[x,y,] = smooth
      
      sel.low = smooth <= 0.4 * mean(smooth)
      sel.high = smooth > 0.8 * mean(smooth)
      sel.mid = smooth > 0.4 * mean(smooth) & smooth <= 0.8 * mean(smooth)
      
      efr = smooth
      efr[sel.low] = smooth[sel.low] * 0.6
      efr[sel.high] = smooth[sel.high] * 0.3
      efr[sel.mid] = smooth[sel.mid] * 0.45
      
      discharge.efr[x,y,] = efr
      
      #plot(time, discharge[x,y,], type = "l")
      #lines(time, smooth, col = "red")
      #lines(time, efr, col = "blue")
    }
  }
  
  dir.create(dirname(efr.out.tmp))
  saveRDS(discharge.efr, efr.out.tmp)
  dir.create(dirname(smooth.out.tmp))
  saveRDS(discharge.smooth, smooth.out.tmp)
}
