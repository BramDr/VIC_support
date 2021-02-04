library(fields)
rm(list = ls())

# Input
scc.file <- "../../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
crop.file <- "./Saves/crop_mapping.csv"
plant.out <- "./Saves/plantDay_30min_global.RDS"
harvest.out <- "./Saves/harvestDay_30min_global.RDS"

# Load
crops <- read.csv(crop.file, stringsAsFactors = F)
scc <- read.csv(scc.file, stringsAsFactors = F, header = T)

# Setup
lons <- seq(-179.75, 179.75, by = 0.5)
lats <- seq(-89.75, 89.75, by = 0.5)
noptions <- 81

scc$x <- sapply(X = scc$lon, FUN = function(x) {
  which(lons == x)
})
scc$y <- sapply(X = scc$lat, FUN = function(x) {
  which(lats == x)
})

set.plant.harvest <- function(x, columns, idx) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  start <- x[columns == "start"]
  end <- x[columns == "end"]
  x.idx <- x[columns == "x"]
  y.idx <- x[columns == "y"]

  start.next <- start + 1
  end.next <- end + 1
  start.prev <- start - 1
  end.prev <- end - 1
  if (start.next > 12) {
    start.next <- start.next - 12
  }
  if (end.next > 12) {
    end.next <- end.next - 12
  }
  if (start.prev < 1) {
    start.prev <- start.prev + 12
  }
  if (end.prev < 1) {
    end.prev <- end.prev + 12
  }

  start.doy.prev.e <- as.numeric(format.Date(as.Date(paste0("0001-", start.prev, "-05")), "%j"))
  end.doy.prev.e <- as.numeric(format.Date(as.Date(paste0("0001-", end.prev, "-05")), "%j"))
  start.doy.prev.m <- as.numeric(format.Date(as.Date(paste0("0001-", start.prev, "-15")), "%j"))
  end.doy.prev.m <- as.numeric(format.Date(as.Date(paste0("0001-", end.prev, "-15")), "%j"))
  start.doy.prev.l <- as.numeric(format.Date(as.Date(paste0("0001-", start.prev, "-25")), "%j"))
  end.doy.prev.l <- as.numeric(format.Date(as.Date(paste0("0001-", end.prev, "-25")), "%j"))
  
  start.doy.e <- as.numeric(format.Date(as.Date(paste0("0001-", start, "-05")), "%j"))
  end.doy.e <- as.numeric(format.Date(as.Date(paste0("0001-", end, "-05")), "%j"))
  start.doy.m <- as.numeric(format.Date(as.Date(paste0("0001-", start, "-15")), "%j"))
  end.doy.m <- as.numeric(format.Date(as.Date(paste0("0001-", end, "-15")), "%j"))
  start.doy.l <- as.numeric(format.Date(as.Date(paste0("0001-", start, "-25")), "%j"))
  end.doy.l <- as.numeric(format.Date(as.Date(paste0("0001-", end, "-25")), "%j"))

  start.doy.next.e <- as.numeric(format.Date(as.Date(paste0("0001-", start.next, "-05")), "%j"))
  end.doy.next.e <- as.numeric(format.Date(as.Date(paste0("0001-", end.next, "-05")), "%j"))
  start.doy.next.m <- as.numeric(format.Date(as.Date(paste0("0001-", start.next, "-15")), "%j"))
  end.doy.next.m <- as.numeric(format.Date(as.Date(paste0("0001-", end.next, "-15")), "%j"))
  start.doy.next.l <- as.numeric(format.Date(as.Date(paste0("0001-", start.next, "-25")), "%j"))
  end.doy.next.l <- as.numeric(format.Date(as.Date(paste0("0001-", end.next, "-25")), "%j"))

  start.doys <- c(start.doy.prev.e, start.doy.prev.m, start.doy.prev.l, 
                  start.doy.e, start.doy.m, start.doy.l,
                  start.doy.next.e, start.doy.next.m, start.doy.next.l)
  end.doys <- c(end.doy.prev.e, end.doy.prev.m, end.doy.prev.l, 
                  end.doy.e, end.doy.m, end.doy.l,
                  end.doy.next.e, end.doy.next.m, end.doy.next.l)
  
  if(end > start){
    dir = 1
  } else {
    dir = -1
  }
  
  z <- 1
  for (start.doy in start.doys) {
    for (end.doy in end.doys) {
      if(dir == 1) {
        if(end.doy <= start.doy) {
          #print(paste0("months ", start, "-", end, ": corr = ", start.doy.e, "-", end.doy.l, " ; wrong = ", start.doy, "-", end.doy))
          z <- z + 1
          next
        }
      } else if (dir == -1) {
        if(start.doy <= end.doy) {
          #print(paste0("months ", start, "-", end, ": corr = ", start.doy.e, "-", end.doy.l, " ; wrong = ", start.doy, "-", end.doy))
          z <- z + 1
          next
        }
      }
    
    
      plant_day[x.idx, y.idx, idx, z] <<- start.doy
      harvest_day[x.idx, y.idx, idx, z] <<- end.doy
      z <- z + 1
    }
  }
  return(0)
}

# Calculate
plant_day <- array(NA, dim = c(length(lons), length(lats), nrow(crops), noptions))
harvest_day <- array(NA, dim = c(length(lons), length(lats), nrow(crops), noptions))
i = 1
for (i in 1:nrow(crops)) {
  print(crops$name[i])

  scc.c <- scc[scc$crop == crops$mirca[i] & scc$subcrop == crops$season[i], ]
  scc.c$rowname <- 1:nrow(scc.c)

  apply(X = scc.c, MARGIN = 1, FUN = set.plant.harvest, columns = colnames(scc.c), idx = i)
}
image.plot(plant_day[, , 1, 1])
image.plot(plant_day[, , 1, 3])
image.plot(harvest_day[, , 1, 1])
image.plot(harvest_day[, , 1, 3])

# Save
dir.create(dirname(plant.out))
saveRDS(plant_day, plant.out)
dir.create(dirname(harvest.out))
saveRDS(harvest_day, harvest.out)
