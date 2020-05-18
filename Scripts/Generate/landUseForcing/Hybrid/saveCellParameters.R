library(fields)
library(plyr)
rm(list = ls())

# Input
cc.file <- "../../../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
fixed.file <- "../../../../../Data/Transformed/LandUse/subcropCalendar_fixed_30min_global.csv"
fc.cell.file <- "../../../../../Data/Transformed/LandUse/subcropCalendar_coverageCell_30min_global.csv"
fc.tile.file <- "Saves/subcropCalendar_coverageTile_30min_global.csv"
fc.tile.monthly.file <- "Saves/subcropCalendar_coverageTileMonthly_30min_global.csv"
avgf.file <- "Saves/subcropCalendar_fractionTile_30min_global.csv"
Cv.out <- "Saves/cellParameters_Cv.csv"
fcanopy.out <- "Saves/cellParameters_fcanopy.csv"
split = data.frame(name = c("wheatRainfed","wheatIrrigated"),
                   id = c(27, 1),
                   stringsAsFactors = F)

# Load
cc <- read.csv(file = cc.file, header = TRUE, stringsAsFactors = F)
fixed <- read.csv(file = fixed.file, header = TRUE, stringsAsFactors = F)
fc.cell <- read.csv(file = fc.cell.file, header = TRUE, stringsAsFactors = F)
fc.tile <- read.csv(file = fc.tile.file, header = TRUE, stringsAsFactors = F)
avgf <- read.csv(file = avgf.file, header = TRUE, stringsAsFactors = F)

# Setup
calc.values <- function(sel, name) {
  print(name)
  
  cv.sel <- aggregate(x = fc.cell[sel, paste0("fc.cell.", 1:12)], by = list(cc$cell_ID[sel]), FUN = sum)
  cv.sel <- apply(X = cv.sel[, paste0("fc.cell.", 1:12)], MARGIN = 1, FUN = max)

  fc.sel <- aggregate(x = fc.tile[sel, paste0("fc.tile.", 1:12)], by = list(cc$cell_ID[sel]), FUN = sum)

  fixed.sel <- aggregate(x = fixed[sel, ], by = list(cc$cell_ID[sel]), FUN = sum)

  fc.adj <- fc.sel
  for (i in 1:ncol(fc.sel)) {
    fc.adj[, i] <- fc.sel[, i] * fixed.sel$fcanopy2
  }

  assign(x = paste0("Cv.", name), value = cv.sel, envir = .GlobalEnv)
  assign(x = paste0("fcanopy.", name), value = fc.adj, envir = .GlobalEnv)
}
save.values <- function(name, outname) {
  tmp.Cv.out <- gsub(x = Cv.out, pattern = "cellParameters", replacement = paste0("cellParameters", outname))
  dir.create(dirname(tmp.Cv.out))
  write.csv(get(x = paste0("Cv.", name)), tmp.Cv.out, row.names = F)

  tmp.fcanopy.out <- gsub(x = fcanopy.out, pattern = "cellParameters", replacement = paste0("cellParameters", outname))
  dir.create(dirname(tmp.fcanopy.out))
  write.csv(get(x = paste0("fcanopy.", name)), tmp.fcanopy.out, row.names = F)
}

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
fcanopy.paddy[fcanopy.paddy > 1] <- 1
fcanopy.irr[fcanopy.irr > 1] <- 1
fcanopy.rain[fcanopy.rain > 1] <- 1

for(i in 1:nrow(split)){
  fcanopy.split = get(x = paste0("fcanopy.", split$name[i]))
  fcanopy.split[fcanopy.split > 1] <- 1
  assign(x = paste0("fcanopy.", split$name[i]), fcanopy.split)
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

