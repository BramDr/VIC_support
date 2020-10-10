library(fields)
rm(list = ls())

# Input
scc.file = "../../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
crop.file = "./Saves/crop_mapping.csv"
plant.out = "./Saves/plantDay_30min_global.RDS"
harvest.out = "./Saves/harvestDay_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
scc = read.csv(scc.file, stringsAsFactors = F, header = T)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)
noptions = 16

scc$x = sapply(X = scc$lon, FUN = function(x){which(lons == x)})
scc$y = sapply(X = scc$lat, FUN = function(x){which(lats == x)})

set.plant.harvest = function(x, columns, idx) {
  x <- as.numeric(x)
  #print(x[columns == "rowname"])
  
  start = x[columns == "start"]
  end = x[columns == "end"]
  x.idx = x[columns == "x"]
  y.idx = x[columns == "y"]
  
  if(start == end){
    print("SAME")
  }
  
  start.alt = start + 1
  end.alt = end + 1
  if(start.alt > 12) {
    start.alt = start.alt - 12
  }
  if(end.alt > 12) {
    end.alt = end.alt - 12
  }
  
  start.doy.e = as.numeric(format.Date(as.Date(paste0("0001-",start,"-05")), "%j"))
  end.doy.e = as.numeric(format.Date(as.Date(paste0("0001-",end,"-05")), "%j"))
  start.doy.m = as.numeric(format.Date(as.Date(paste0("0001-",start,"-15")), "%j"))
  end.doy.m = as.numeric(format.Date(as.Date(paste0("0001-",end,"-15")), "%j"))
  start.doy.l = as.numeric(format.Date(as.Date(paste0("0001-",start,"-25")), "%j"))
  end.doy.l = as.numeric(format.Date(as.Date(paste0("0001-",end,"-25")), "%j"))
  start.doy.vl = as.numeric(format.Date(as.Date(paste0("0001-",start.alt,"-04")), "%j"))
  end.doy.vl = as.numeric(format.Date(as.Date(paste0("0001-",end.alt,"-04")), "%j"))
  
  z = 1
  for(start.doy in c(start.doy.e, start.doy.m, start.doy.l, start.doy.vl)){
    for(end.doy in c(end.doy.e, end.doy.m, end.doy.l, end.doy.vl)){
      plant_day[x.idx,y.idx,idx,z] <<- start.doy
      harvest_day[x.idx,y.idx,idx,z] <<- end.doy
      z = z + 1
    }
  }
  return(0)
}

# Calculate
plant_day = array(NA, dim = c(length(lons), length(lats), nrow(crops), noptions))
harvest_day = array(NA, dim = c(length(lons), length(lats), nrow(crops), noptions))
for(i in 1:nrow(crops)){
  print(crops$name[i])
  
  scc.c = scc[scc$crop == crops$mirca[i] & scc$subcrop == crops$season[i], ]
  scc.c$rowname = 1:nrow(scc.c)
  
  if(i == 1){
    scc.c$end[scc.c$lon > 95 & scc.c$end == 4] = 5
  }
  
  apply(X = scc.c, MARGIN = 1, FUN = set.plant.harvest, columns = colnames(scc.c), idx = i)
}
image.plot(plant_day[,,1,1])
image.plot(plant_day[,,1,9])
image.plot(harvest_day[,,1,1])
image.plot(harvest_day[,,1,3])

# Save
dir.create(dirname(plant.out))
saveRDS(plant_day, plant.out)
dir.create(dirname(harvest.out))
saveRDS(harvest_day, harvest.out)

