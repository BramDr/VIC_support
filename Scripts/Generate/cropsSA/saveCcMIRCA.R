library(fields)
rm(list = ls())

# Input
scc.file = "../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
crop.file = "./Saves/crop_mapping_MIRCA.csv"
Cc.out = "./Saves/Cc_MIRCA_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
scc = read.csv(scc.file, stringsAsFactors = F, header = T)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

scc$x = scc$column
scc$y = 360 - scc$row

set.Cc.map = function(x, columns) {
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
  
  Cc.map[x.idx,y.idx,period] <<- 1
  return(0)
}

# Save
for(i in 1:nrow(crops)) {
  print(crops$mirca.name[i])
  
  Cc.crop.out = gsub(x = Cc.out, pattern = "Cc_", replacement = paste0("Cc_", i, "_"))
  print(basename(Cc.crop.out))
  
  scc.c = scc[scc$crop == crops$mirca.id[i] & scc$subcrop == crops$season[i], ]
  scc.c$rowname = 1:nrow(scc.c)
  
  Cc.map = array(0, dim = c(length(lons), length(lats), 12))
  apply(X = scc.c, MARGIN = 1, FUN = set.Cc.map, columns = colnames(scc.c))
  #image.plot(Cc.map[,,1])
  
  dir.create(dirname(Cc.crop.out))
  saveRDS(Cc.map, Cc.crop.out)
}



