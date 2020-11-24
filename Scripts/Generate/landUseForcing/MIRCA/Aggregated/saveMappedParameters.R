library(fields)
library(ncdf4)
rm(list = ls())

# Input
cc.file <- "../../../../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
Cv.file <- "Saves/cellParameters_Cv.csv"
fcanopy.file <- "Saves/cellParameters_fcanopy.csv"
paddy.out <- "Saves/parametersPaddy_30min_global.RDS"
irr.out <- "Saves/parametersIrrigated_30min_global.RDS"
rain.out <- "Saves/parametersRainfed_30min_global.RDS"

# Load
load.values <- function(inname, name) {
  tmp.Cv.file <- gsub(x = Cv.file, pattern = "cellParameters", replacement = paste0("cellParameters", inname))
  assign(x = paste0("Cv.", name), value = read.csv(tmp.Cv.file, header = TRUE, stringsAsFactors = F), envir = .GlobalEnv)

  tmp.fcanopy.file <- gsub(x = fcanopy.file, pattern = "cellParameters", replacement = paste0("cellParameters", inname))
  assign(x = paste0("fcanopy.", name), value = read.csv(tmp.fcanopy.file, header = TRUE, stringsAsFactors = F), envir = .GlobalEnv)
}

cc <- read.csv(cc.file, header = TRUE, stringsAsFactors = F)

load.values("Paddy", "paddy")
load.values("Irrigated", "irr")
load.values("Rainfed", "rain")

# Setup
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
lons <- seq(from = -179.75, to = 179.75, by = 0.5)

loc <- aggregate(x = cc[, c("lon", "lat", "row", "column")], by = list(cc$cell_ID), FUN = mean)

create.map <- function(values) {
  # print(colnames(values)[2])

  map <- array(NA, dim = c(720, 360))

  values <- merge(values, loc, by.x = 1, by.y = 1)

  for (j in 1:nrow(values)) {
    x <- which(lons == values$lon[j])
    y <- which(lats == values$lat[j])
    map[x, y] <- values[j, 2]
  }

  return(map)
}
map.values <- function(name) {
  print(name)

  Cv.sel <- get(paste0("Cv.", name))
  fcanopy.sel <- get(paste0("fcanopy.", name))

  maps <- list()
  maps[["Cv"]] <- create.map(Cv.sel[, c("Group.1", "fc.cell")])

  map.m <- array(NA, dim = c(720, 360, 12))
  for (m in 1:12) {
    map.m[, , m] <- create.map(fcanopy.sel[, c("Group.1", paste0("fc.tile.", m))])
  }
  maps[["fcanopy"]] <- map.m

  return(maps)
}

maps.rain <- map.values("rain")
maps.irr <- map.values("irr")
maps.paddy <- map.values("paddy")

# Save
dir.create(dirname(paddy.out))
saveRDS(maps.paddy, paddy.out)
dir.create(dirname(irr.out))
saveRDS(maps.irr, irr.out)
dir.create(dirname(rain.out))
saveRDS(maps.rain, rain.out)
