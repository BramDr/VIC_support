library(fields)
rm(list = ls())

# Input
crop.file <- "./Saves/crop_mapping.RDS"
scc.file <- "./Saves/subcropCalendar_5min_Indus.RDS"
plant.out <- "./Saves/plantday_crops_5min_Indus.RDS"
harvest.out <- "./Saves/harvestday_crops_5min_Indus.RDS"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
crops <- readRDS(crop.file)
scc <- readRDS(scc.file)

set.plant.harvest <- function(x, columns, idx) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  start <- x[columns == "start"]
  end <- x[columns == "end"]
  lat <- x[columns == "lat"]
  lon <- x[columns == "long"]
  
  x.idx <- which.min(abs(out.lons - lon))
  y.idx <- which.min(abs(out.lats - lat))

  start.doy <- as.numeric(format.Date(as.Date(paste0("0001-", start, "-05")), "%j"))
  end.doy <- as.numeric(format.Date(as.Date(paste0("0001-", end, "-25")), "%j"))

  plant_day[x.idx, y.idx, idx] <<- start.doy
  harvest_day[x.idx, y.idx, idx] <<- end.doy

  return(0)
}

# Calculate
plant_day <- array(NA, dim = c(length(out.lons), length(out.lats), nrow(crops)))
harvest_day <- array(NA, dim = c(length(out.lons), length(out.lats), nrow(crops)))

i <- 1
for (i in 1:nrow(crops)) {
  print(crops$name[i])

  if(crops$season[i] == "max"){
    scc.max = aggregate(formula = subcrop ~ Cell_ID + crop, data = scc[scc$crop == crops$mirca[i],], FUN = max)
    colnames(scc.max) = c("Cell_ID", "crop", "maxseason")
    scc.tmp = merge(scc, scc.max)
    
    scc.c = scc.tmp[scc.tmp$crop == crops$mirca[i] & scc.tmp$subcrop == scc.tmp$maxseason,]
  } else {
    scc.c <- scc[scc$crop == crops$mirca[i] & scc$subcrop == crops$season[i], ]
  }
  scc.c$rowname <- 1:nrow(scc.c)

  apply(X = scc.c, MARGIN = 1, FUN = set.plant.harvest, columns = colnames(scc.c), idx = i)
}
image.plot(plant_day[, , 2])
image.plot(harvest_day[, , 2])

# Save
dir.create(dirname(plant.out))
saveRDS(plant_day, plant.out)
dir.create(dirname(harvest.out))
saveRDS(harvest_day, harvest.out)
