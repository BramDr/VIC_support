library(fields)
library(plyr)
rm(list = ls())

# Input
crop.file <- "./Saves/crop_mapping.csv"
cc.file <- "../../../../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
fc.cell.file <- "../../../../../../Data/Transformed/LandUse/subcropCalendar_coverageCell_30min_global.csv"
fc.tile.file <- "Saves/subcropCalendar_coverageTile_30min_global.csv"
fc.tile.monthly.file <- "Saves/subcropCalendar_coverageTileMonthly_30min_global.csv"
avgf.file <- "Saves/subcropCalendar_fractionTile_30min_global.csv"
Cv.out <- "Saves/cellParameters_Cv.csv"
fcanopy.out <- "Saves/cellParameters_fcanopy.csv"
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
crops <- read.csv(crop.file, stringsAsFactors = F)
cc <- read.csv(file = cc.file, header = TRUE, stringsAsFactors = F)
fc.cell <- read.csv(file = fc.cell.file, header = TRUE, stringsAsFactors = F)
fc.tile <- read.csv(file = fc.tile.file, header = TRUE, stringsAsFactors = F)
avgf <- read.csv(file = avgf.file, header = TRUE, stringsAsFactors = F)

# Setup
calc.values <- function(sel, name) {
  print(name)

  cv.sel1 <- aggregate(x = fc.cell[sel, paste0("fc.cell.", 1:12)], by = list(cc$cell_ID[sel]), FUN = sum)
  cv.sel2 <- apply(X = cv.sel1[, paste0("fc.cell.", 1:12)], MARGIN = 1, FUN = max)
  cv.sel <- cv.sel1[, c("Group.1", "fc.cell.1")]
  cv.sel[, 2] <- cv.sel2
  colnames(cv.sel) <- c("Group.1", "fc.cell")

  fc.sel <- aggregate(x = fc.tile[sel, paste0("fc.tile.", 1:12)], by = list(cc$cell_ID[sel]), FUN = sum)

  assign(x = paste0("Cv.", name), value = cv.sel, envir = .GlobalEnv)
  assign(x = paste0("fcanopy.", name), value = fc.sel, envir = .GlobalEnv)
}
save.values <- function(name, outname) {
  tmp.Cv.out <- gsub(x = Cv.out, pattern = "cellParameters", replacement = paste0("cellParameters", outname))
  dir.create(dirname(tmp.Cv.out))
  write.csv(get(x = paste0("Cv.", name)), tmp.Cv.out, row.names = F)

  tmp.fcanopy.out <- gsub(x = fcanopy.out, pattern = "cellParameters", replacement = paste0("cellParameters", outname))
  dir.create(dirname(tmp.fcanopy.out))
  write.csv(get(x = paste0("fcanopy.", name)), tmp.fcanopy.out, row.names = F)
}

# Calculate
sel.irr <- cc$crop %in% c(1:2, 4:26) & !cc$crop %in% split$id
sel.rain <- cc$crop %in% c(27:52) & !cc$crop %in% split$id

calc.values(sel = sel.irr, name = "irr")
calc.values(sel = sel.rain, name = "rain")

for (i in 1:nrow(split)) {
  seasons <- crops$season[crops$mirca == split$id[i]]
  sel.split <- cc$crop %in% split$id[i] & cc$subcrop %in% seasons
  calc.values(sel = sel.split, name = split$name[i])
}

## Adjustments
fcanopy.sel <- fcanopy.irr > 1
fcanopy.sel[, 1] <- FALSE
fcanopy.irr[fcanopy.sel] <- 1
fcanopy.sel <- fcanopy.rain > 1
fcanopy.sel[, 1] <- FALSE
fcanopy.rain[fcanopy.sel] <- 1

for (i in 1:nrow(split)) {
  fcanopy.split <- get(x = paste0("fcanopy.", split$name[i]))
  fcanopy.sel <- fcanopy.split > 1
  fcanopy.sel[, 1] <- FALSE
  fcanopy.split[fcanopy.sel] <- 1
  assign(x = paste0("fcanopy.", split$name[i]), fcanopy.split)
}

fcanopy.sel <- fcanopy.irr < 0
fcanopy.sel[, 1] <- FALSE
fcanopy.irr[fcanopy.sel] <- 0
fcanopy.sel <- fcanopy.rain < 0
fcanopy.sel[, 1] <- FALSE
fcanopy.rain[fcanopy.sel] <- 0

for (i in 1:nrow(split)) {
  fcanopy.split <- get(x = paste0("fcanopy.", split$name[i]))
  fcanopy.sel <- fcanopy.split < 0
  fcanopy.sel[, 1] <- FALSE
  fcanopy.split[fcanopy.sel] <- 0
  assign(x = paste0("fcanopy.", split$name[i]), fcanopy.split)
}

# Save
save.values(name = "irr", outname = "Irrigated")
save.values(name = "rain", outname = "Rainfed")

for (i in 1:nrow(split)) {
  save.values(
    name = split$name[i],
    outname = paste0(
      toupper(substring(text = split$name[i], first = 1, last = 1)),
      substring(text = split$name[i], first = 2, last = nchar(split$name[i]))
    )
  )
}
