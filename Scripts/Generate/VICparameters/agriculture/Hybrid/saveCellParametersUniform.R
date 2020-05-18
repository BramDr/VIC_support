library(fields)
library(plyr)
rm(list = ls())

# Input
cc.file <- "../../../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
fixed.file <- "../../../../../Data/Transformed/LandUse/subcropCalendar_fixed_30min_global.csv"
albedo.file <- "../../../../../Data/Transformed/LandUse/subcropCalendar_albedo_30min_global.csv"
LAI.file <- "../../../../../Data/Transformed/LandUse/subcropCalendar_LAI_30min_global.csv"
veg_rough.file <- "../../../../../Data/Transformed/LandUse/subcropCalendar_vegRough_30min_global.csv"
displacement.file <- "../../../../../Data/Transformed/LandUse/subcropCalendar_displacement_30min_global.csv"
fc.cell.file <- "../../../../../Data/Transformed/LandUse/subcropCalendar_coverageCell_30min_global.csv"
fc.tile.file <- "Saves/subcropCalendar_coverageTile_30min_global.csv"
fc.tile.monthly.file <- "Saves/subcropCalendar_coverageTileMonthly_30min_global.csv"
avgf.file <- "Saves/subcropCalendar_fractionTile_30min_global.csv"
Cv.out <- "Saves/cellParametersUniform_Cv.csv"
fixed.out <- "Saves/cellParametersUniform_fixed.csv"
fcanopy.out <- "Saves/cellParametersUniform_fcanopy.csv"
albedo.out <- "Saves/cellParametersUniform_albedo.csv"
LAI.out <- "Saves/cellParametersUniform_LAI.csv"
veg_rough.out <- "Saves/cellParametersUniform_vegRough.csv"
displacement.out <- "Saves/cellParametersUniform_displacement.csv"
split = data.frame(name = c("wheatRainfed","wheatIrrigated"),
                   id = c(27, 1),
                   stringsAsFactors = F)

# Load
cc <- read.csv(file = cc.file, header = TRUE, stringsAsFactors = F)
fixed <- read.csv(file = fixed.file, header = TRUE, stringsAsFactors = F)
albedo <- read.csv(file = albedo.file, header = TRUE, stringsAsFactors = F)
LAI <- read.csv(file = LAI.file, header = TRUE, stringsAsFactors = F)
veg_rough <- read.csv(file = veg_rough.file, header = TRUE, stringsAsFactors = F)
displacement <- read.csv(file = displacement.file, header = TRUE, stringsAsFactors = F)
fc.cell <- read.csv(file = fc.cell.file, header = TRUE, stringsAsFactors = F)
fc.tile <- read.csv(file = fc.tile.file, header = TRUE, stringsAsFactors = F)
fc.tile.monthly <- read.csv(file = fc.tile.monthly.file, header = TRUE, stringsAsFactors = F)
avgf <- read.csv(file = avgf.file, header = TRUE, stringsAsFactors = F)

# Setup
calc.values <- function(sel, name) {
  print(name)
  
  cv.sel <- aggregate(x = fc.cell[sel, paste0("fc.cell.", 1:12)], by = list(cc$cell_ID[sel]), FUN = sum)
  cv.sel <- apply(X = cv.sel[, paste0("fc.cell.", 1:12)], MARGIN = 1, FUN = max)

  fc.sel <- aggregate(x = fc.tile[sel, paste0("fc.tile.", 1:12)], by = list(cc$cell_ID[sel]), FUN = sum)

  LAI.sel <- aggregate(x = LAI[sel, paste0("LAI.", 1:12)], by = list(cc$cell_ID[sel]), FUN = sum)

  albedo.sel <- aggregate(x = albedo[sel, paste0("albedo.", 1:12)], by = list(cc$cell_ID[sel]), FUN = sum)

  displacement.sel <- aggregate(x = displacement[sel, paste0("displacement.", 1:12)], by = list(cc$cell_ID[sel]), FUN = sum)

  veg_rough.sel <- aggregate(x = veg_rough[sel, paste0("veg_rough.", 1:12)], by = list(cc$cell_ID[sel]), FUN = sum)

  fixed.sel <- aggregate(x = fixed[sel, ], by = list(cc$cell_ID[sel]), FUN = sum)
  fixed.sel$overstory <- fixed.sel$overstory > 0.8

  fc.adj <- fc.sel * 0 + 1
  for (i in 1:ncol(fc.adj)) {
    fc.adj[, i] <- fc.adj[, i] * fixed.sel$fcanopy2
  }

  assign(x = paste0("Cv.", name), value = cv.sel, envir = .GlobalEnv)
  assign(x = paste0("fcanopy.", name), value = fc.adj, envir = .GlobalEnv)
  assign(x = paste0("LAI.", name), value = LAI.sel, envir = .GlobalEnv)
  assign(x = paste0("albedo.", name), value = albedo.sel, envir = .GlobalEnv)
  assign(x = paste0("displacement.", name), value = displacement.sel, envir = .GlobalEnv)
  assign(x = paste0("veg_rough.", name), value = veg_rough.sel, envir = .GlobalEnv)
  assign(x = paste0("fixed.", name), value = fixed.sel, envir = .GlobalEnv)
}
save.values <- function(name, outname) {
  tmp.Cv.out <- gsub(x = Cv.out, pattern = "cellParametersUniform", replacement = paste0("cellParametersUniform", outname))
  dir.create(dirname(tmp.Cv.out))
  write.csv(get(x = paste0("Cv.", name)), tmp.Cv.out, row.names = F)

  tmp.fixed.out <- gsub(x = fixed.out, pattern = "cellParametersUniform", replacement = paste0("cellParametersUniform", outname))
  dir.create(dirname(tmp.fixed.out))
  write.csv(get(x = paste0("fixed.", name)), tmp.fixed.out, row.names = F)

  tmp.fcanopy.out <- gsub(x = fcanopy.out, pattern = "cellParametersUniform", replacement = paste0("cellParametersUniform", outname))
  dir.create(dirname(tmp.fcanopy.out))
  write.csv(get(x = paste0("fcanopy.", name)), tmp.fcanopy.out, row.names = F)

  tmp.albedo.out <- gsub(x = albedo.out, pattern = "cellParametersUniform", replacement = paste0("cellParametersUniform", outname))
  dir.create(dirname(tmp.albedo.out))
  write.csv(get(x = paste0("albedo.", name)), tmp.albedo.out, row.names = F)

  tmp.LAI.out <- gsub(x = LAI.out, pattern = "cellParametersUniform", replacement = paste0("cellParametersUniform", outname))
  dir.create(dirname(tmp.LAI.out))
  write.csv(get(x = paste0("LAI.", name)), tmp.LAI.out, row.names = F)

  tmp.veg_rough.out <- gsub(x = veg_rough.out, pattern = "cellParametersUniform", replacement = paste0("cellParametersUniform", outname))
  dir.create(dirname(tmp.veg_rough.out))
  write.csv(get(x = paste0("veg_rough.", name)), tmp.veg_rough.out, row.names = F)

  tmp.displacement.out <- gsub(x = displacement.out, pattern = "cellParametersUniform", replacement = paste0("cellParametersUniform", outname))
  dir.create(dirname(tmp.displacement.out))
  write.csv(get(x = paste0("displacement.", name)), tmp.displacement.out, row.names = F)
}

for (m in 1:12) {
  LAI[, paste0("LAI.", m)] <- fc.tile.monthly[, paste0("fc.tile.monthly.", m)] * LAI[, paste0("LAI.", m)]
  albedo[, paste0("albedo.", m)] <- fc.tile.monthly[, paste0("fc.tile.monthly.", m)] * albedo[, paste0("albedo.", m)]

  displacement[, paste0("displacement.", m)] <- fc.tile.monthly[, paste0("fc.tile.monthly.", m)] * displacement[, paste0("displacement.", m)]
  veg_rough[, paste0("veg_rough.", m)] <- fc.tile.monthly[, paste0("fc.tile.monthly.", m)] * veg_rough[, paste0("veg_rough.", m)]
}

fixed[, "root_depth.1"] <- avgf * fixed[, "root_depth.1"]
fixed[, "root_depth.2"] <- avgf * fixed[, "root_depth.2"]
fixed[, "root_depth.3"] <- avgf * fixed[, "root_depth.3"]
fixed[, "root_frac.1"] <- avgf * fixed[, "root_frac.1"]
fixed[, "root_frac.2"] <- avgf * fixed[, "root_frac.2"]
fixed[, "root_frac.3"] <- avgf * fixed[, "root_frac.3"]
fixed[, "RGL"] <- avgf * fixed[, "RGL"]
fixed[, "rarc"] <- avgf * fixed[, "rarc"]
fixed[, "rad_atten"] <- avgf * fixed[, "rad_atten"]
fixed[, "overstory"] <- avgf * fixed[, "overstory"]
fixed[, "trunk_ratio"] <- avgf * fixed[, "trunk_ratio"]
fixed[, "wind_atten"] <- avgf * fixed[, "wind_atten"]
fixed[, "wind_h"] <- avgf * fixed[, "wind_h"]
fixed[, "rmin"] <- avgf * fixed[, "rmin"]
fixed[, "fcanopy2"] <- avgf * fixed[, "fcanopy2"]

# Calculate
sel.paddy <- cc$crop == 3 & ! cc$crop %in% split$id
sel.irr <- cc$crop %in% c(1:2, 4:26) & ! cc$crop %in% split$id
sel.rain <- cc$crop %in% c(27:52) & ! cc$crop %in% split$id

calc.values(sel = sel.paddy, name = "paddy")
calc.values(sel = sel.irr, name = "irr")
calc.values(sel = sel.rain, name = "rain")

for(i in 1:nrow(split)){
  sel.split <- cc$crop %in% split$id[i]
  calc.values(sel = sel.split, name = split$name[i])
}

## Adjustments
displacement.max <- apply(X = displacement.paddy[, paste0("displacement.", 1:12)], MARGIN = 1, FUN = max)
fixed.paddy[, "wind_h"] <- displacement.max / 0.67 + 1
displacement.max <- apply(X = displacement.irr[, paste0("displacement.", 1:12)], MARGIN = 1, FUN = max)
fixed.irr[, "wind_h"] <- displacement.max / 0.67 + 1
displacement.max <- apply(X = displacement.rain[, paste0("displacement.", 1:12)], MARGIN = 1, FUN = max)
fixed.rain[, "wind_h"] <- displacement.max / 0.67 + 1

for(i in 1:nrow(split)){
  displacement.split = get(x = paste0("displacement.", split$name[i]))
  fixed.split = get(x = paste0("fixed.", split$name[i]))
  displacement.max <- apply(X = displacement.split[, paste0("displacement.", 1:12)], MARGIN = 1, FUN = max)
  fixed.split[, "wind_h"] <- displacement.max / 0.67 + 1
  assign(x = paste0("fixed.", split$name[i]), fixed.split)
}

fcanopy.paddy[fcanopy.paddy > 1] <- 1
fcanopy.irr[fcanopy.irr > 1] <- 1
fcanopy.rain[fcanopy.rain > 1] <- 1

for(i in 1:nrow(split)){
  fcanopy.split = get(x = paste0("fcanopy.", split$name[i]))
  fcanopy.split[fcanopy.split > 1] <- 1
  assign(x = paste0("fcanopy.", split$name[i]), fcanopy.split)
}

albedo.paddy <- albedo.paddy + 0.2 * (1 - fcanopy.paddy)
albedo.irr <- albedo.irr + 0.2 * (1 - fcanopy.irr)
albedo.rain <- albedo.rain + 0.2 * (1 - fcanopy.rain)

for(i in 1:nrow(split)){
  albedo.split = get(x = paste0("albedo.", split$name[i]))
  fcanopy.split = get(x = paste0("fcanopy.", split$name[i]))
  albedo.split <- albedo.split + 0.2 * (1 - fcanopy.split)
  assign(x = paste0("albedo.", split$name[i]), albedo.split)
}

fcanopy.paddy[fcanopy.paddy < 0.00011] <- 0.00011
fcanopy.irr[fcanopy.irr < 0.00011] <- 0.00011
fcanopy.rain[fcanopy.rain < 0.00011] <- 0.00011

for(i in 1:nrow(split)){
  fcanopy.split = get(x = paste0("fcanopy.", split$name[i]))
  fcanopy.split[fcanopy.split < 0.00011] <- 0.00011
  assign(x = paste0("fcanopy.", split$name[i]), fcanopy.split)
}

# Save
save.values(name = "paddy", outname = "Paddy")
save.values(name = "irr", outname = "Irrigated")
save.values(name = "rain", outname = "Rainfed")

for(i in 1:nrow(split)){
  save.values(name = split$name[i], 
              outname = paste0(toupper(substring(text = split$name[i], first = 1, last = 1)), 
                               substring(text = split$name[i], first = 2, last = nchar(split$name[i]))))
}

