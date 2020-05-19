library(fields)
library(ncdf4)
rm(list = ls())

# Input
function.script = "../../Support/mapFunctions.R"
crop.file = "./Saves/crop_mapping_MIRCA.csv"
vegetation.file = "../../../Data/Primary/VIC/VIC_params_global.nc"
Cc.dir = "./Saves"
Cc.total.file = "./Saves/Cc_total_MIRCA_30min_global.RDS"
Cc.other.file = "./Saves/Cc_other_MIRCA_30min_global.RDS"
Cc.bare.file = "./Saves/Cc_bare_MIRCA_30min_global.RDS"
out.file = "./Saves/coverage_monthly_MIRCA_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = FALSE)
Cc.files = list.files(path = Cc.dir, pattern = "Cc_.*_MIRCA", full.names = TRUE)
Cc.total = readRDS(Cc.total.file)
Cc.other = readRDS(Cc.other.file)
Cc.bare = readRDS(Cc.bare.file)

nc = nc_open(vegetation.file)
Cv = ncvar_get(nc, nc$var$Cv)
nc_close(nc)
Cv[is.na(Cv)] = 0

# Setup
vic.id.u = unique(crops$vic.id)

get.surrounding.mean = function(data, x, y, max.itir = 20) {
  for(i in 1:max.itir) {
    x.min = x - i
    x.max = x + i
    y.min = y - i
    y.max = y + i
    
    if(x.min < 1) {
      x.min = 1
    }
    if(y.min < 1) {
      y.min = 1
    }
    if(x.max > 720) {
      x.max = 720
    }
    if(y.max > 360) {
      y.max = 360
    }
    
    surrounding = apply(X = data[x.min:x.max, y.min:y.max, ], MARGIN = c(3), FUN = mean, na.rm = T)
    if(sum(surrounding) == 0){
      next
    }
    return(surrounding)
  }
  
  print("Could not find surrounding");
  return(NA)
}

# Calculate
## Calculate monthly crop coverage
## Crop cover is varying monthly, but cropland coverage is stable
## Uncultivated cropland will be added to bare soil component
Cc = array(0, dim = c(dim(Cv)[1], dim(Cv)[2], length(vic.id.u) + 1, 12))
for(i in 1:nrow(crops)) {
  print(crops$mirca.name[i])
  
  idx = which(vic.id.u == crops$vic.id[i])
  
  Cc.file = grep(x = Cc.files, pattern = paste0("_", i, "_"), value = T)
  Cc.c = readRDS(Cc.file)
  Cc[,,idx,] = Cc[,,idx,] + Cc.c
}
Cc[,,length(vic.id.u) + 1,] = Cc.other

Cc.sum = apply(X = Cc, MARGIN = c(1,2,4), FUN = sum)
Cc.max = apply(X = Cc.sum, MARGIN = c(1,2), FUN = max)
image.plot(Cc.max)

## Calculate monthly veg coverage
## Veg coverage is stable every month but is rescaled to fit crop fractions
## If no natural vegetation was available, add bare soil
Cv.max = apply(X = Cv[,,c(1:10,12)], MARGIN = c(1,2), FUN = sum)
Cv.m = array(0, dim = c(dim(Cv)[1], dim(Cv)[2], dim(Cv)[3] - 1, 12))
for(x in 1:dim(Cv)[1]){
  for(y in 1:dim(Cv)[2]) {
    if(Cv.max[x,y] == 0 && Cv[x,y,11] == 0 && Cc.max[x,y] == 0){
      next
    }
    
    crop.f = Cc.max[x,y]
    veg.f = Cv.max[x,y]
    
    if(veg.f <= 0.00001) {
      if(crop.f == 1){
        next
      }
      #sur = get.surrounding.mean(data = Cv[,,c(1:10,12)], x = x, y = y)
      for(i in 1:12) {
        Cv.m[x,y,dim(Cv.m)[3],i] = (1 - crop.f)
      }
    } else {
      rescale.f = (1 - crop.f) / veg.f
      for(i in 1:12) {
        Cv.m[x,y,,i] = Cv[x,y,c(1:10,12)] * rescale.f
      }
    }
  }
}
Cv.sum = apply(X = Cv.m, MARGIN = c(1,2,4), FUN = sum)
Cv.max = apply(X = Cv.sum, MARGIN = c(1,2), FUN = max)
image.plot(Cv.max)
image.plot(Cc.max)
image.plot(Cv.max + Cc.max)

## Create final coverage map
Cv.final = array(data = 0, dim = c(dim(Cv)[1], dim(Cv)[2], dim(Cv.m)[3] + dim(Cc)[3], 12))
Cv.final[,,1:10,] = Cv.m[,,1:10,]
Cv.final[,,11:(11 + dim(Cc)[3] - 1),] = Cc[,,,]
Cv.final[,,dim(Cv.final)[3],] = Cv.m[,,11,] + Cc.bare

Cv.final.sum = apply(X = Cv.final, MARGIN = c(1,2), FUN = sum)
image.plot(Cv.final.sum)

# Save
dir.create(dirname(out.file))
saveRDS(object = Cv.final, file = out.file)
