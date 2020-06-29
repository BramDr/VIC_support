library(fields)
rm(list = ls())

# Input
scc.file = "../../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
option.dir = "./In"
crop.file = "./Saves/crop_mapping.csv"
plant.out = "./Saves/plantDay_30min_global.RDS"
harvest.out = "./Saves/harvestDay_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
scc = read.csv(scc.file, stringsAsFactors = F, header = T)

option.files = list.files(option.dir, pattern = "actual", full.names = T)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

scc$x = sapply(X = scc$lon, FUN = function(x){which(lons == x)})
scc$y = sapply(X = scc$lat, FUN = function(x){which(lats == x)})

option.map = array(3, dim = c(length(lons), length(lats), nrow(crops)))
for(i in 1:nrow(crops)){
  option.file = grep(x = option.files, pattern = paste0(crops$name[i], "_", crops$water[i], "_", crops$season[i]), value = T)
  
  if(length(option.file) == 0){
    print("FILE MISSING")
    next
  }
  
  option.map[,,i] = readRDS(option.file)
}
option.map[option.map == 0] = 3
image.plot(option.map[,,5])

start.doy.e = 4
end.doy.e = 4
start.doy.m = 14
end.doy.m = 14
start.doy.l = 24
end.doy.l = 24
start.doy.options = c()
end.doy.options = c()
for(start.doy in c(start.doy.e, start.doy.m, start.doy.l)){
  for(end.doy in c(end.doy.e, end.doy.m, end.doy.l)){
    start.doy.options = c(start.doy.options, start.doy)
    end.doy.options = c(end.doy.options, end.doy)
  }
}

set.plant.harvest = function(x, columns, idx) {
  x <- as.numeric(x)
  #print(x[columns == "rowname"])
  
  start = x[columns == "start"]
  end = x[columns == "end"]
  x.idx = x[columns == "x"]
  y.idx = x[columns == "y"]
  
  option = option.map[x.idx, y.idx, idx]
  
  start.doy = as.numeric(format.Date(as.Date(paste0("0001-",start,"-01")), "%j"))
  end.doy = as.numeric(format.Date(as.Date(paste0("0001-",end,"-01")), "%j"))
  
  plant_day[x.idx,y.idx,idx] <<- start.doy + start.doy.options[option]
  harvest_day[x.idx,y.idx,idx] <<- end.doy + end.doy.options[option]
  return(0)
}

# Calculate
plant_day = array(NA, dim = c(length(lons), length(lats), nrow(crops)))
harvest_day = array(NA, dim = c(length(lons), length(lats), nrow(crops)))

i = 5
for(i in 1:nrow(crops)){
  print(crops$name[i])
  
  scc.c = scc[scc$crop == crops$mirca[i] & scc$subcrop == crops$season[i], ]
  scc.c$rowname = 1:nrow(scc.c)
  
  apply(X = scc.c, MARGIN = 1, FUN = set.plant.harvest, columns = colnames(scc.c), idx = i)
}
image.plot(plant_day[,,5])
image.plot(harvest_day[,,5])

# Save
dir.create(dirname(plant.out))
saveRDS(plant_day, plant.out)
dir.create(dirname(harvest.out))
saveRDS(harvest_day, harvest.out)

