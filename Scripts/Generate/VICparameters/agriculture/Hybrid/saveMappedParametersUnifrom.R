library(fields)
library(ncdf4)
rm(list = ls())

# Input
cc.file <- "../../../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
Cv.file <- "Saves/cellParametersUniform_Cv.csv"
fixed.file <- "Saves/cellParametersUniform_fixed.csv"
fcanopy.file <- "Saves/cellParametersUniform_fcanopy.csv"
albedo.file <- "Saves/cellParametersUniform_albedo.csv"
LAI.file <- "Saves/cellParametersUniform_LAI.csv"
veg_rough.file <- "Saves/cellParametersUniform_vegRough.csv"
displacement.file <- "Saves/cellParametersUniform_displacement.csv"
param.out <- "Saves/parametersUniform_30min_global.RDS"
split = data.frame(name = c("wheatRainfed","wheatIrrigated"),
                   id = c(27, 1),
                   stringsAsFactors = F)

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

cc <- read.csv(cc.file, header = TRUE, stringsAsFactors = F)

load.values("Paddy", "paddy")
load.values("Irrigated", "irr")
load.values("Rainfed", "rain")

for(i in 1:nrow(split)){
  load.values(paste0(toupper(substring(text = split$name[i], first = 1, last = 1)), 
                     substring(text = split$name[i], first = 2, last = nchar(split$name[i]))),
              split$name[i])
}

# Setup
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
lons <- seq(from = -179.75, to = 179.75, by = 0.5)

loc.paddy <- aggregate(
  x = cc[cc$crop == 3 & ! cc$crop %in% split$id, c("lon", "lat", "row", "column")],
  by = list(cc$cell_ID[cc$crop == 3 & ! cc$crop %in% split$id]), FUN = mean
)
loc.irr <- aggregate(
  x = cc[cc$crop %in% c(1:2, 4:26) & ! cc$crop %in% split$id, c("lon", "lat", "row", "column")],
  by = list(cc$cell_ID[cc$crop %in% c(1:2, 4:26) & ! cc$crop %in% split$id]), FUN = mean
)
loc.rain <- aggregate(
  x = cc[cc$crop %in% c(27:52) & ! cc$crop %in% split$id, c("lon", "lat", "row", "column")],
  by = list(cc$cell_ID[cc$crop %in% c(27:52) & ! cc$crop %in% split$id]), FUN = mean
)

for(i in 1:nrow(split)){
  loc.split <- aggregate(
    x = cc[cc$crop %in% split$id[i], c("lon", "lat", "row", "column")],
    by = list(cc$cell_ID[cc$crop %in% split$id[i]]), FUN = mean
  )
  assign(x = paste0("loc.", split$name[i]), value = loc.split)
}

create.map <- function(values, lons.v, lats.v) {
  map <- array(NA, dim = c(720, 360))

  for (j in 1:length(values)) {
    x <- which(lons == lons.v[j])
    y <- which(lats == lats.v[j])
    map[x, y] <- values[j]
  }

  return(map)
}
map.values <- function(name) {
  print(name)
  
  loc.sel <- get(paste0("loc.", name))
  Cv.sel <- get(paste0("Cv.", name))
  fcanopy.sel <- get(paste0("fcanopy.", name))
  fixed.sel <- get(paste0("fixed.", name))
  albedo.sel <- get(paste0("albedo.", name))
  LAI.sel <- get(paste0("LAI.", name))
  displacement.sel <- get(paste0("displacement.", name))
  veg_rough.sel <- get(paste0("veg_rough.", name))

  maps <- list()
  maps[["Cv"]] <- create.map(Cv.sel[, 1], loc.sel$lon, loc.sel$lat)

  for (i in 2:ncol(fixed.sel)) {
    maps[[colnames(fixed.sel)[i]]] <- create.map(fixed.sel[, i], loc.sel$lon, loc.sel$lat)
  }

  map.m <- array(NA, dim = c(720, 360, 12))
  for (m in 1:12) {
    map.m[, , m] <- create.map(albedo.sel[, paste0("albedo.", m)], loc.sel$lon, loc.sel$lat)
  }
  maps[["albedo"]] <- map.m

  map.m <- array(NA, dim = c(720, 360, 12))
  for (m in 1:12) {
    map.m[, , m] <- create.map(LAI.sel[, paste0("LAI.", m)], loc.sel$lon, loc.sel$lat)
  }
  maps[["LAI"]] <- map.m

  map.m <- array(NA, dim = c(720, 360, 12))
  for (m in 1:12) {
    map.m[, , m] <- create.map(displacement.sel[, paste0("displacement.", m)], loc.sel$lon, loc.sel$lat)
  }
  maps[["displacement"]] <- map.m

  map.m <- array(NA, dim = c(720, 360, 12))
  for (m in 1:12) {
    map.m[, , m] <- create.map(veg_rough.sel[, paste0("veg_rough.", m)], loc.sel$lon, loc.sel$lat)
  }
  maps[["veg_rough"]] <- map.m

  map.m <- array(NA, dim = c(720, 360, 12))
  for (m in 1:12) {
    map.m[, , m] <- create.map(fcanopy.sel[, paste0("fc.tile.", m)], loc.sel$lon, loc.sel$lat)
  }
  maps[["fcanopy"]] <- map.m

  return(maps)
}

maps.rain <- map.values("rain")
maps.irr <- map.values("irr")
maps.paddy <- map.values("paddy")

for(i in 1:nrow(split)){
  maps.split <- map.values(split$name[i])
  assign(x = paste0("maps.", split$name[i]), value = maps.split)
}

# Save
parameter.out.tmp = gsub(x = param.out, pattern = "parametersUniform_", replacement = paste0("parametersUniform", "Paddy", "_"))
dir.create(dirname(parameter.out.tmp))
saveRDS(maps.paddy, parameter.out.tmp)
parameter.out.tmp = gsub(x = param.out, pattern = "parametersUniform_", replacement = paste0("parametersUniform", "Irrigated", "_"))
dir.create(dirname(parameter.out.tmp))
saveRDS(maps.irr, parameter.out.tmp)
parameter.out.tmp = gsub(x = param.out, pattern = "parametersUniform_", replacement = paste0("parametersUniform", "Rainfed", "_"))
dir.create(dirname(parameter.out.tmp))
saveRDS(maps.rain, parameter.out.tmp)

for(i in 1:nrow(split)){
  maps.split = get(x = paste0("maps.", split$name[i]))
  parameter.out.tmp = gsub(x = param.out, pattern = "parametersUniform_", 
                           replacement = paste0("parametersUniform", 
                                                paste0(toupper(substring(text = split$name[i], first = 1, last = 1)), 
                                                       substring(text = split$name[i], first = 2, last = nchar(split$name[i]))), 
                                                "_"))
  dir.create(dirname(parameter.out.tmp))
  saveRDS(maps.split, parameter.out.tmp)
}

