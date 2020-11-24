library(fields)
library(ncdf4)
rm(list = ls())

# Input
crop.file <- "./Saves/crop_mapping.csv"
cc.file <- "../../../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
Cv.file <- "Saves/cellParametersUniform_Cv.csv"
fixed.file <- "Saves/cellParametersUniform_fixed.csv"
fcanopy.file <- "Saves/cellParametersUniform_fcanopy.csv"
albedo.file <- "Saves/cellParametersUniform_albedo.csv"
LAI.file <- "Saves/cellParametersUniform_LAI.csv"
veg_rough.file <- "Saves/cellParametersUniform_vegRough.csv"
displacement.file <- "Saves/cellParametersUniform_displacement.csv"
param.out <- "Saves/parametersUniform_30min_global.RDS"
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
  tmp.Cv.file <- gsub(x = Cv.file, pattern = "cellParametersUniform", replacement = paste0("cellParametersUniform", inname))
  assign(x = paste0("Cv.", name), value = read.csv(tmp.Cv.file, header = TRUE, stringsAsFactors = F), envir = .GlobalEnv)

  tmp.fixed.file <- gsub(x = fixed.file, pattern = "cellParametersUniform", replacement = paste0("cellParametersUniform", inname))
  assign(x = paste0("fixed.", name), value = read.csv(tmp.fixed.file, header = TRUE, stringsAsFactors = F), envir = .GlobalEnv)

  tmp.albedo.file <- gsub(x = albedo.file, pattern = "cellParametersUniform", replacement = paste0("cellParametersUniform", inname))
  assign(x = paste0("albedo.", name), value = read.csv(tmp.albedo.file, header = TRUE, stringsAsFactors = F), envir = .GlobalEnv)

  tmp.LAI.file <- gsub(x = LAI.file, pattern = "cellParametersUniform", replacement = paste0("cellParametersUniform", inname))
  assign(x = paste0("LAI.", name), value = read.csv(tmp.LAI.file, header = TRUE, stringsAsFactors = F), envir = .GlobalEnv)

  tmp.veg_rough.file <- gsub(x = veg_rough.file, pattern = "cellParametersUniform", replacement = paste0("cellParametersUniform", inname))
  assign(x = paste0("veg_rough.", name), value = read.csv(tmp.veg_rough.file, header = TRUE, stringsAsFactors = F), envir = .GlobalEnv)

  tmp.displacement.file <- gsub(x = displacement.file, pattern = "cellParametersUniform", replacement = paste0("cellParametersUniform", inname))
  assign(x = paste0("displacement.", name), value = read.csv(tmp.displacement.file, header = TRUE, stringsAsFactors = F), envir = .GlobalEnv)

  tmp.fcanopy.file <- gsub(x = fcanopy.file, pattern = "cellParametersUniform", replacement = paste0("cellParametersUniform", inname))
  assign(x = paste0("fcanopy.", name), value = read.csv(tmp.fcanopy.file, header = TRUE, stringsAsFactors = F), envir = .GlobalEnv)
}

crops <- read.csv(crop.file, stringsAsFactors = F)
cc <- read.csv(cc.file, header = TRUE, stringsAsFactors = F)

load.values("Irrigated", "irr")
load.values("Rainfed", "rain")

for (i in 1:nrow(split)) {
  load.values(
    paste0(
      toupper(substring(text = split$name[i], first = 1, last = 1)),
      substring(text = split$name[i], first = 2, last = nchar(split$name[i]))
    ),
    split$name[i]
  )
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
  fixed.sel <- get(paste0("fixed.", name))
  albedo.sel <- get(paste0("albedo.", name))
  LAI.sel <- get(paste0("LAI.", name))
  displacement.sel <- get(paste0("displacement.", name))
  veg_rough.sel <- get(paste0("veg_rough.", name))

  maps <- list()
  maps[["Cv"]] <- create.map(Cv.sel[, c("Group.1", "fc.cell")])

  group.idx <- which(colnames(fixed.sel) == "Group.1")
  for (i in 2:ncol(fixed.sel)) {
    maps[[colnames(fixed.sel)[i]]] <- create.map(fixed.sel[, c(group.idx, i)])
  }

  map.m <- array(NA, dim = c(720, 360, 12))
  for (m in 1:12) {
    map.m[, , m] <- create.map(albedo.sel[, c("Group.1", paste0("albedo.", m))])
  }
  maps[["albedo"]] <- map.m

  map.m <- array(NA, dim = c(720, 360, 12))
  for (m in 1:12) {
    map.m[, , m] <- create.map(LAI.sel[, c("Group.1", paste0("LAI.", m))])
  }
  maps[["LAI"]] <- map.m

  map.m <- array(NA, dim = c(720, 360, 12))
  for (m in 1:12) {
    map.m[, , m] <- create.map(displacement.sel[, c("Group.1", paste0("displacement.", m))])
  }
  maps[["displacement"]] <- map.m

  map.m <- array(NA, dim = c(720, 360, 12))
  for (m in 1:12) {
    map.m[, , m] <- create.map(veg_rough.sel[, c("Group.1", paste0("veg_rough.", m))])
  }
  maps[["veg_rough"]] <- map.m

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
parameter.out.tmp <- gsub(x = param.out, pattern = "parametersUniform_", replacement = paste0("parametersUniform", "Irrigated", "_"))
dir.create(dirname(parameter.out.tmp))
saveRDS(maps.irr, parameter.out.tmp)
parameter.out.tmp <- gsub(x = param.out, pattern = "parametersUniform_", replacement = paste0("parametersUniform", "Rainfed", "_"))
dir.create(dirname(parameter.out.tmp))
saveRDS(maps.rain, parameter.out.tmp)

for (i in 1:nrow(split)) {
  maps.split <- get(x = paste0("maps.", split$name[i]))
  parameter.out.tmp <- gsub(
    x = param.out, pattern = "parametersUniform_",
    replacement = paste0(
      "parametersUniform",
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
