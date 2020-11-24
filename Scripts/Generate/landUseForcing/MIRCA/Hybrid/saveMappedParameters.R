library(fields)
library(ncdf4)
rm(list = ls())

# Input
crop.file <- "./Saves/crop_mapping.csv"
cc.file <- "../../../../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
Cv.file <- "Saves/cellParameters_Cv.csv"
fcanopy.file <- "Saves/cellParameters_fcanopy.csv"
param.out <- "Saves/parameters_30min_global.RDS"
split <- data.frame(
  name = c(
    "wheatRainfed", "wheatIrrigated",
    "maizeRainfed", "maizeIrrigated",
    "riceRainfed", "riceIrrigated",
    "soybeanRainfed", "soybeanIrrigated"
  ),
  id = c(
    27, 1,
    28, 2,
    29, 3,
    34, 8
  ),
  stringsAsFactors = F
)

# Load
load.values <- function(inname, name) {
  tmp.Cv.file <- gsub(x = Cv.file, pattern = "cellParameters", replacement = paste0("cellParameters", inname))
  assign(x = paste0("Cv.", name), value = read.csv(tmp.Cv.file, header = TRUE, stringsAsFactors = F), envir = .GlobalEnv)

  tmp.fcanopy.file <- gsub(x = fcanopy.file, pattern = "cellParameters", replacement = paste0("cellParameters", inname))
  assign(x = paste0("fcanopy.", name), value = read.csv(tmp.fcanopy.file, header = TRUE, stringsAsFactors = F), envir = .GlobalEnv)
}

crops <- read.csv(crop.file, stringsAsFactors = F)
cc <- read.csv(cc.file, header = TRUE, stringsAsFactors = F)

load.values("Irrigated", "irr")
Cv.irr$Group.1 <- fcanopy.irr$Group.1
load.values("Rainfed", "rain")
Cv.rain$Group.1 <- fcanopy.rain$Group.1

for (i in 1:nrow(split)) {
  load.values(
    paste0(
      toupper(substring(text = split$name[i], first = 1, last = 1)),
      substring(text = split$name[i], first = 2, last = nchar(split$name[i]))
    ),
    split$name[i]
  )

  Cv.split <- get(x = paste0("Cv.", split$name[i]))
  fcanopy.split <- get(x = paste0("fcanopy.", split$name[i]))
  Cv.split$Group.1 <- fcanopy.split$Group.1
  assign(x = paste0("Cv.", split$name[i]), value = Cv.split)
}

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

for (i in 1:nrow(split)) {
  maps.split <- map.values(split$name[i])
  assign(x = paste0("maps.", split$name[i]), value = maps.split)
}

# Save
parameter.out.tmp <- gsub(x = param.out, pattern = "parameters_", replacement = paste0("parameters", "Irrigated", "_"))
dir.create(dirname(parameter.out.tmp))
saveRDS(maps.irr, parameter.out.tmp)
parameter.out.tmp <- gsub(x = param.out, pattern = "parameters_", replacement = paste0("parameters", "Rainfed", "_"))
dir.create(dirname(parameter.out.tmp))
saveRDS(maps.rain, parameter.out.tmp)

for (i in 1:nrow(split)) {
  maps.split <- get(x = paste0("maps.", split$name[i]))
  parameter.out.tmp <- gsub(
    x = param.out, pattern = "parameters_",
    replacement = paste0(
      "parameters",
      paste0(
        toupper(substring(text = split$name[i], first = 1, last = 1)),
        substring(text = split$name[i], first = 2, last = nchar(split$name[i]))
      ),
      "_"
    )
  )
  dir.create(dirname(parameter.out.tmp))
  saveRDS(maps.split, parameter.out.tmp)
}
