library(fields)
library(ncdf4)
rm(list = ls())

# Input
dam.file <- "../../../Data/Primary/GRanD/GRanDv1.3.csv"
area.file <- "../../../Data/Transformed/Routing/area_5min_Indus.RDS"
accumulation.file <- "../../../Data/Transformed/Routing/accumulation_5min_Indus.RDS"
dam.out <- "../../../Data/Transformed/Dams/GRanDcorrected_Indus.csv"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
area <- readRDS(area.file)
accumulation <- readRDS(accumulation.file)

dams <- read.csv(dam.file, stringsAsFactors = F)
dams$LONG_DD <- as.numeric(dams$LONG_DD)
dams$LAT_DD <- as.numeric(dams$LAT_DD)
dams[dams == -99] <- NA

# Calculate
dams$X <- NA
dams$Y <- NA
dams$AREA_FRAC <- NA
dams$CATCH_FRAC <- NA
dams$MODEL_LONG_DD <- NA
dams$MODEL_LAT_DD <- NA
dams$MODEL_X <- NA
dams$MODEL_Y <- NA
dams$MODEL_AREA_FRAC <- NA
dams$MODEL_CATCH_FRAC <- NA

i = 4792
for (i in 1:nrow(dams)) {
  x.diff = abs(out.lons - dams$LONG_DD[i])
  y.diff = abs(out.lats - dams$LAT_DD[i])
  
  if(min(x.diff) > resolution / 2 || min(y.diff) > resolution / 2){
    next
  }
  
  x <- which.min(abs(out.lons - dams$LONG_DD[i]))
  y <- which.min(abs(out.lats - dams$LAT_DD[i]))
  
  dams$X[i] = x
  dams$Y[i] = y

  frac.a <- dams$CATCH_SKM[i] / (area[x, y] * 1e-6)
  frac.a <- min(frac.a, 1)
  
  frac.c <- dams$CATCH_SKM[i] / (accumulation[x, y] * 1e-6)
  frac.c <- max(frac.c, 1 / frac.c)
  
  dams$AREA_FRAC[i] <- frac.a
  dams$CATCH_FRAC[i] <- frac.c

  if (frac.a < 1) {
    dams$MODEL_LONG_DD[i] <- out.lons[x]
    dams$MODEL_LAT_DD[i] <- out.lats[y]
    dams$MODEL_X[i] <- x
    dams$MODEL_Y[i] <- y
    dams$MODEL_AREA_FRAC[i] <- frac.a
    dams$MODEL_CATCH_FRAC[i] <- frac.c
    next
  }
  
  bx <- x
  by <- y
  ba <- Inf
  max.index = 1
  for(max.index in 1:3){
    n.index = -max.index:max.index
    
    if(frac.c > 0.75 && frac.c < 1.25){
      break;
    }
    
    bx <- x
    by <- y
    ba <- Inf
    
    for (xni in n.index) {
      for (yni in n.index) {
        xn <- x + xni
        yn <- y + yni
  
        if (xn <= 0 || xn > dim(accumulation)[1] ||
          yn <= 0 || yn > dim(accumulation)[2]) {
          next
        }
  
        if (is.na(accumulation[xn, yn])) {
          next
        }
  
        # if neighbouring accumulation is better than the current accumulation,
        # save neighbouring location
        if (abs(dams$CATCH_SKM[i] - accumulation[xn, yn] * 1e-6) < ba) {
          bx <- xn
          by <- yn
          ba <- abs(dams$CATCH_SKM[i] - accumulation[xn, yn] * 1e-6)
        }
      }
    }
  
    frac.a <- dams$CATCH_SKM[i] / (area[bx, by] * 1e-6)
    frac.a <- min(frac.a, 1)
  
    frac.c <- dams$CATCH_SKM[i] / (accumulation[bx, by] * 1e-6)
    frac.c <- max(frac.c, 1 / frac.c) 
  }
  
  dams$MODEL_LONG_DD[i] <- out.lons[bx]
  dams$MODEL_LAT_DD[i] <- out.lats[by]
  dams$MODEL_X[i] <- bx
  dams$MODEL_Y[i] <- by
  dams$MODEL_CATCH_FRAC[i] <- frac.c
  dams$MODEL_AREA_FRAC[i] <- frac.a
  
  print(paste0("Dam from ", dams$CATCH_FRAC[i], " to ", dams$MODEL_CATCH_FRAC[i]))
}

dams = dams[!is.na(dams$MODEL_AREA_FRAC),]

dams.view = dams
dams.view$PLOT_X = dams.view$X / dim(accumulation)[1]
dams.view$PLOT_Y = dams.view$Y / dim(accumulation)[2]
dams.view$PLOT_MODEL_X = dams.view$MODEL_X / dim(accumulation)[1]
dams.view$PLOT_MODEL_Y = dams.view$MODEL_Y / dim(accumulation)[2]

image.plot(accumulation > 0)
points(dams.view$PLOT_X, dams.view$PLOT_Y, col = "white")
points(dams.view$PLOT_MODEL_X, dams.view$PLOT_MODEL_Y, col = "purple")

# Save
dir.create(dirname(dam.out))
write.csv(x = dams, file = dam.out, row.names = F)
