library(fields)
rm(list = ls())

# Input
scc.file <- "../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
crop.file <- "./Saves/crop_mapping_single.csv"
plant.out <- "./Saves/plantDay_MIRCA_30min_global.RDS"
harvest.out <- "./Saves/harvestDay_MIRCA_30min_global.RDS"

# Load
crops <- read.csv(crop.file, stringsAsFactors = F)
scc <- read.csv(scc.file, stringsAsFactors = F, header = T)

# Setup
lons <- seq(-179.75, 179.75, by = 0.5)
lats <- seq(-89.75, 89.75, by = 0.5)

scc$x <- scc$column
scc$y <- 360 - scc$row

set.plant.harvest <- function(x, columns, idx) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  start <- x[columns == "start"]
  end <- x[columns == "end"]
  x.idx <- x[columns == "x"]
  y.idx <- x[columns == "y"]

  start.doy <- as.numeric(format.Date(as.Date(paste0("0001-", start, "-01")), "%j"))
  end.doy <- as.numeric(format.Date(as.Date(paste0("0001-", end, "-28")), "%j"))

  plant_day[x.idx, y.idx, idx] <<- start.doy
  harvest_day[x.idx, y.idx, idx] <<- end.doy
  return(0)
}

# Calculate
plant_day <- array(NA, dim = c(length(lons), length(lats), nrow(crops)))
harvest_day <- array(NA, dim = c(length(lons), length(lats), nrow(crops)))
for (i in 1:nrow(crops)) {
  print(crops$mirca.name[i])

  scc.c <- scc[scc$crop == crops$mirca.id[i] & scc$subcrop == 1, ]
  scc.c$rowname <- 1:nrow(scc.c)

  apply(X = scc.c, MARGIN = 1, FUN = set.plant.harvest, columns = colnames(scc.c), idx = i)
}
image.plot(plant_day[, , 1])
image.plot(harvest_day[, , 1])

# Save
dir.create(dirname(plant.out))
saveRDS(plant_day, plant.out)
dir.create(dirname(harvest.out))
saveRDS(harvest_day, harvest.out)
