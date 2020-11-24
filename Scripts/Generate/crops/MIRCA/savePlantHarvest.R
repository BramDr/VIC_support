library(fields)
rm(list = ls())

# Input
scc.file <- "../../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
option.dir <- "../../../../Data/Transformed/Crops"
crop.file <- "./Saves/crop_mapping.csv"
plant.out <- "./Saves/plantDay_30min_global.RDS"
harvest.out <- "./Saves/harvestDay_30min_global.RDS"

# Load
crops <- read.csv(crop.file, stringsAsFactors = F)
scc <- read.csv(scc.file, stringsAsFactors = F, header = T)

option.files <- list.files(option.dir, pattern = "actual", full.names = T)

# Setup
lons <- seq(-179.75, 179.75, by = 0.5)
lats <- seq(-89.75, 89.75, by = 0.5)

scc$x <- sapply(X = scc$lon, FUN = function(x) {
  which(lons == x)
})
scc$y <- sapply(X = scc$lat, FUN = function(x) {
  which(lats == x)
})

option.map <- array(3, dim = c(length(lons), length(lats), nrow(crops)))
for (i in 1:nrow(crops)) {
  option.file <- grep(x = option.files, pattern = paste0(crops$name[i], "_", crops$water[i], "_", crops$season[i]), value = T)

  if (length(option.file) == 0) {
    print("FILE MISSING")
    next
  }

  option.map[, , i] <- readRDS(option.file)
}
option.map[option.map == 0] <- 3
image.plot(option.map[, , 5])

set.plant.harvest <- function(x, columns, idx) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  start <- x[columns == "start"]
  end <- x[columns == "end"]
  x.idx <- x[columns == "x"]
  y.idx <- x[columns == "y"]

  start.alt <- start + 1
  end.alt <- end + 1
  if (start.alt > 12) {
    start.alt <- start.alt - 12
  }
  if (end.alt > 12) {
    end.alt <- end.alt - 12
  }

  option <- option.map[x.idx, y.idx, idx]

  start.doy.e <- as.numeric(format.Date(as.Date(paste0("0001-", start, "-05")), "%j"))
  end.doy.e <- as.numeric(format.Date(as.Date(paste0("0001-", end, "-05")), "%j"))
  start.doy.m <- as.numeric(format.Date(as.Date(paste0("0001-", start, "-15")), "%j"))
  end.doy.m <- as.numeric(format.Date(as.Date(paste0("0001-", end, "-15")), "%j"))
  start.doy.l <- as.numeric(format.Date(as.Date(paste0("0001-", start, "-25")), "%j"))
  end.doy.l <- as.numeric(format.Date(as.Date(paste0("0001-", end, "-25")), "%j"))
  start.doy.vl <- as.numeric(format.Date(as.Date(paste0("0001-", start.alt, "-04")), "%j"))
  end.doy.vl <- as.numeric(format.Date(as.Date(paste0("0001-", end.alt, "-04")), "%j"))

  start.doy.comb <- c(start.doy.e, start.doy.m, start.doy.l, start.doy.vl)
  end.doy.comb <- c(end.doy.e, end.doy.m, end.doy.l, end.doy.vl)

  start.doys <- rep(start.doy.comb, each = length(end.doy.comb))
  end.doys <- rep(end.doy.comb, times = length(start.doy.comb))

  plant_day[x.idx, y.idx, idx] <<- start.doys[option]
  harvest_day[x.idx, y.idx, idx] <<- end.doys[option]

  return(0)
}

# Calculate
plant_day <- array(NA, dim = c(length(lons), length(lats), nrow(crops)))
harvest_day <- array(NA, dim = c(length(lons), length(lats), nrow(crops)))

i <- 5
for (i in 1:nrow(crops)) {
  print(crops$name[i])

  scc.c <- scc[scc$crop == crops$mirca[i] & scc$subcrop == crops$season[i], ]
  scc.c$rowname <- 1:nrow(scc.c)

  if (i == 1) {
    scc.c$end[scc.c$lon > 95 & scc.c$end == 4] <- 5
  }

  apply(X = scc.c, MARGIN = 1, FUN = set.plant.harvest, columns = colnames(scc.c), idx = i)
}
image.plot(plant_day[, , 5])
image.plot(harvest_day[, , 5])

# Save
dir.create(dirname(plant.out))
saveRDS(plant_day, plant.out)
dir.create(dirname(harvest.out))
saveRDS(harvest_day, harvest.out)
