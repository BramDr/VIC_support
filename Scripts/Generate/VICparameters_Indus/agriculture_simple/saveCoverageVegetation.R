rm(list = ls())
library(fields)
library(ncdf4)

# Input
vegetation.file <- "../../../../Data/VIC/Parameters/Indus_5min/vegetation_params_Modis_Indus.nc"
crop.grid.file = "./Saves/coverageMIRCA_grid_monthly_5min_Indus.RDS"
vegetation.crop.idx = 12
coverage.out = "./Saves/coverage_vegetation_monthly_5min_Indus.RDS"

# Load
nc = nc_open(vegetation.file)
vegetation.orig.cv = ncvar_get(nc, "Cv")
nc_close(nc)

crop.grid.cv = readRDS(crop.grid.file)

# Calculate
vegetation.orig.cv.sum = apply(X = vegetation.orig.cv, MARGIN = c(1,2), FUN = sum)
image.plot(vegetation.orig.cv.sum)

vegetation.cv.adj = vegetation.orig.cv
vegetation.cv.adj[,,vegetation.crop.idx] = vegetation.cv.adj[,,vegetation.crop.idx] - crop.grid.cv
vegetation.cv.adj[vegetation.cv.adj < 0] = 0
vegetation.cv.adj.sum = apply(X = vegetation.cv.adj, MARGIN = c(1,2), FUN = sum)
image.plot(vegetation.cv.adj.sum)

vegetation.cv.scaled = vegetation.cv.adj
for(v in 1:dim(vegetation.cv.adj)[3]){
  vegetation.cv.scaled.sel = vegetation.cv.scaled[,,v]
  
  vegetation.cv.scaled.sel = vegetation.cv.adj[,,v] / vegetation.cv.adj.sum * (1 - crop.grid.cv)
  vegetation.cv.scaled.sel[vegetation.cv.adj.sum == 0] = NA
  
  vegetation.cv.scaled[,,v] = vegetation.cv.scaled.sel
}
vegetation.cv.scaled.sum = apply(X = vegetation.cv.scaled, MARGIN = c(1,2), FUN = sum)
image.plot(vegetation.cv.scaled.sum + crop.grid.cv)

vegetation.cv.filled = vegetation.cv.scaled
fill.err = array(0, dim = dim(vegetation.cv.scaled)[1:2])
for(x in 1:dim(vegetation.cv.scaled)[1]){
  for(y in 1:dim(vegetation.cv.scaled)[2]){
    if(!is.na(vegetation.cv.scaled.sum[x,y]) || is.na(vegetation.cv.adj.sum[x,y])){
      next
    }
    
    veg = rep(0, dim(vegetation.cv.scaled)[3])
    count = 0
    for(nxi in c(-2,0,2)){
      for(nyi in c(-2,0,2)){
        nx = x + nxi
        ny = y + nyi
        
        if(nx <= 0 || ny <= 0 || 
           nx >= dim(vegetation.cv.scaled.sum)[1] || ny >= dim(vegetation.cv.scaled.sum)[2]){
          next
        }
        
        if(is.na(vegetation.cv.scaled.sum[nx, ny])){
          next
        }
        veg = veg + vegetation.cv.scaled[nx,ny,]
        count = count + 1
      }
    }
    
    if(count == 0){
      print("NO NEIGHBOURS")
      fill.err[x,y] = 1
      next
    }
    
    veg = veg / count
    vegetation.cv.filled[x,y,] =  veg / sum(veg) * (1 - crop.grid.cv[x,y])
  }
}
vegetation.cv.filled.sum = apply(X = vegetation.cv.filled, MARGIN = c(1,2), FUN = sum)
image.plot(vegetation.cv.filled.sum + crop.grid.cv)

vegetation.cv = array(NA, dim = c(dim(vegetation.cv.filled)[1:3], 12))
for(z in 1:dim(vegetation.cv)[4]){
  vegetation.cv[,,,z] = vegetation.cv.filled
}

# Save
dir.create(dirname(coverage.out))
saveRDS(vegetation.cv, coverage.out)
