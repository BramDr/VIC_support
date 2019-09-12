library(fields)
library(ncdf4)

rm(list = ls())

# Input
Cc.monthly.file = "Saves/CcMonthly_30min_global.RDS"
Cv.fixed.file = "Input/VIC_params_global.nc"
Cv.monthly.out = "Saves/CvMonthly_30min_global.RDS"

# Load
nc = nc_open(Cv.fixed.file)
Cv.fixed = ncvar_get(nc, nc$var$Cv)
Cv.fixed = Cv.fixed[,,c(1:10,12)]
nc_close(nc)

Cc.monthly = readRDS(Cc.monthly.file)

# Setup
nveg = 11
nmonths = 12

Cc.crop.max = apply(X = Cc.monthly, MARGIN = c(1,2,4), FUN = sum)
Cc.crop.max = apply(X = Cc.crop.max, MARGIN = c(1,2), FUN = mean)
Cc.veg.max = 1 - Cc.crop.max

Cv.veg.cur = apply(X = Cv.fixed, MARGIN = c(1,2), FUN = sum)
Cv.veg.diff = Cc.veg.max - Cv.veg.cur

image.plot(Cv.veg.diff)

# Caclulate
Cv.monthly = array(0, dim = c(720, 360, nveg, nmonths))
for(x in 1:dim(Cv.monthly)[1]){
  for(y in 1:dim(Cv.monthly)[2]){
    
    # Outside mask
    if(is.na(Cv.veg.diff[x,y])){
      
    }
    
    # No differences
    else if(Cv.veg.diff[x,y] == 0){
      Cv.adj = Cv.fixed[x,y,]
      Cv.monthly[x,y,,] = rep(Cv.adj, 12)
    }
    
    # Crop area is larger than before
    else if(Cv.veg.diff[x,y] < 0){
      Cv.adj = Cv.fixed[x,y,] * (Cc.veg.max[x,y] / Cv.veg.cur[x,y])
      Cv.monthly[x,y,,] = rep(Cv.adj, 12)
    }
    
    # Crop area is smaller than before
    else if(Cv.veg.diff[x,y] > 0 && Cv.veg.cur[x,y] > 0){
      Cv.adj = Cv.fixed[x,y,] * (Cc.veg.max[x,y] / Cv.veg.cur[x,y])
      Cv.monthly[x,y,,] = rep(Cv.adj, 12)
    }
    
    # Crop area had disappeared, but whole cell is now empty
    else if(Cv.veg.cur[x,y] <= 0){
      Cv.neighbouring = Cv.fixed[(x - 1):(x + 1), (y - 1):(y + 1),]
      Cv.neighbouring = apply(X = Cv.neighbouring, MARGIN = c(3), FUN = mean, na.rm = T)
      Cv.adj = Cv.neighbouring / sum(Cv.neighbouring)
      Cv.monthly[x,y,,] = rep(Cv.adj, 12)
    }
    
    # Error?
    else {
      print("huh?")
    }
  }
}
Cv.veg.max = apply(X = Cv.monthly, MARGIN = c(1,2,4), FUN = sum)
Cv.veg.max = apply(X = Cv.veg.max, MARGIN = c(1,2), FUN = mean)
image.plot(Cv.veg.max)
image.plot(Cc.veg.max)
image.plot(Cv.veg.max - Cc.veg.max)

# Save
dir.create(dirname(Cv.monthly.out))
saveRDS(Cv.monthly, Cv.monthly.out)
