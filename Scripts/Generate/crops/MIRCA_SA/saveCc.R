library(fields)
library(ncdf4)
rm(list = ls())

# Input
scc.file = "../../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
crop.file = "./Saves/crop_mapping.csv"
cc.out = "./Saves/cc_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
scc = read.csv(scc.file, stringsAsFactors = F, header = T)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

scc$x = sapply(X = scc$lon, FUN = function(x){which(lons == x)})
scc$y = sapply(X = scc$lat, FUN = function(x){which(lats == x)})

set.cc.map = function(x, columns, idx) {
  x <- as.numeric(x)
  #print(x[columns == "rowname"])
  
  start = x[columns == "start"]
  end = x[columns == "end"]
  x.idx = x[columns == "x"]
  y.idx = x[columns == "y"]
  
  if (start < end) {
    period <- start:end
  } else {
    period <- c(start:12, 1:end)
  }
  
  cc.map[x.idx,y.idx,idx,] <<- 0
  cc.map[x.idx,y.idx,idx,period] <<- 1
  return(0)
}

# Save
cc.map = array(NA, dim = c(length(lons), length(lats), nrow(crops), 12))
for(i in 1:nrow(crops)) {
  print(crops$name[i])
  
  scc.c = scc[scc$crop == crops$mirca[i] & scc$subcrop == crops$season[i], ]
  scc.c$rowname = 1:nrow(scc.c)
  
  apply(X = scc.c, MARGIN = 1, FUN = set.cc.map, columns = colnames(scc.c), idx = i)
  #image.plot(cc.map[,,i,1])
}

cc.sum = apply(X = cc.map, MARGIN = c(1,2,3), FUN = sum)
for(i in 1:nrow(crops)){
  image.plot(cc.sum[,,i], main = i)
}

# Save
dir.create(dirname(cc.out))
saveRDS(cc.map, cc.out)
