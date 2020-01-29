library(fields)
library(plyr)
rm(list = ls())

# Input
cc.file <- "../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
area.file <- "../../../Data/Primary/MIRCA2000/Cell area grid/cell_area_ha_30mn.asc"
fc.cell.out <- "Saves/croppingCalendars_coverageCell.csv"
fc.tile.out <- "Saves/croppingCalendars_coverageTile.csv"
fc.crop.out <- "Saves/croppingCalendars_coverageCrop.csv"

# Load
cc <- read.csv(file = cc.file, stringsAsFactors = F)
area <- as.matrix(read.table(file = area.file, header = FALSE, sep = " ", skip = 6, stringsAsFactors = F))

# Setup
add.area.cell <- function(x, columns) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  out <- area[x[columns == "row"], x[columns == "column"]]

  return(out)
}
add.fc.cell <- function(x, columns) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  maxValue <- x[columns == "cellarea"]
  out <- x[columns %in% paste0("area.", 1:12)] / maxValue

  return(out)
}
add.fc.tile <- function(x, columns) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  maxValue <- x[columns == "tilearea"]
  out <- x[columns %in% paste0("area.", 1:12)] / maxValue

  return(out)
}
add.fc.crop <- function(x, columns) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  maxValues <- x[columns %in% paste0("croparea.", 1:12)]
  out <- rep(0, 12)
  out2 <- x[columns %in% paste0("area.", 1:12)] / maxValues
  out[maxValues > 0] <- out2[maxValues > 0]

  return(out)
}

# Calculate
## Calculate tile area
cc.cell.paddy <- aggregate(
  formula = cbind(area.1, area.2, area.3, area.4, area.5, area.6, area.7, area.8, area.9, area.10, area.11, area.12) ~ cell_ID + row + column,
  data = cc[cc$crop == 3, ], FUN = sum
)
cc.cell.paddy[, paste0("paddyarea.", 1:12)] <- cc.cell.paddy[, paste0("area.", 1:12)]
cc.cell.paddy$paddytile <- apply(X = cc.cell.paddy[, paste0("paddyarea.", 1:12)], MARGIN = 1, FUN = max)

cc.cell.irr <- aggregate(
  formula = cbind(area.1, area.2, area.3, area.4, area.5, area.6, area.7, area.8, area.9, area.10, area.11, area.12) ~ cell_ID + row + column,
  data = cc[cc$crop %in% c(1:2, 4:26), ], FUN = sum
)
cc.cell.irr[, paste0("irrarea.", 1:12)] <- cc.cell.irr[, paste0("area.", 1:12)]
cc.cell.irr$irrtile <- apply(X = cc.cell.irr[, paste0("irrarea.", 1:12)], MARGIN = 1, FUN = max)

cc.cell.rain <- aggregate(
  formula = cbind(area.1, area.2, area.3, area.4, area.5, area.6, area.7, area.8, area.9, area.10, area.11, area.12) ~ cell_ID + row + column,
  data = cc[cc$crop %in% c(27:52), ], FUN = sum
)
cc.cell.rain[, paste0("rainarea.", 1:12)] <- cc.cell.rain[, paste0("area.", 1:12)]
cc.cell.rain$raintile <- apply(X = cc.cell.rain[, paste0("rainarea.", 1:12)], MARGIN = 1, FUN = max)

## Calculate cell area
cellarea <- apply(X = cc, MARGIN = 1, FUN = add.area.cell, columns = colnames(cc))

## Merge
cc.merge <- cc
cc.merge <- join(cc.merge, cc.cell.paddy[, c("cell_ID", "paddytile", paste0("paddyarea.", 1:12))])
cc.merge <- join(cc.merge, cc.cell.irr[, c("cell_ID", "irrtile", paste0("irrarea.", 1:12))])
cc.merge <- join(cc.merge, cc.cell.rain[, c("cell_ID", "raintile", paste0("rainarea.", 1:12))])

cc.merge$cellarea <- NA
cc.merge$tilearea <- NA
cc.merge[, paste0("croparea.", 1:12)] <- NA

cc.merge$cellarea <- cellarea

cc.merge[cc.merge$crop == 3, paste0("croparea.", 1:12)] <-
  cc.merge[cc.merge$crop == 3, paste0("paddyarea.", 1:12)]
cc.merge$tilearea[cc.merge$crop == 3] <- cc.merge$paddytile[cc.merge$crop == 3]

cc.merge[cc.merge$crop %in% c(1:2, 4:26), paste0("croparea.", 1:12)] <-
  cc.merge[cc.merge$crop %in% c(1:2, 4:26), paste0("irrarea.", 1:12)]
cc.merge$tilearea[cc.merge$crop %in% c(1:2, 4:26)] <- cc.merge$irrtile[cc.merge$crop %in% c(1:2, 4:26)]

cc.merge[cc.merge$crop %in% c(27:52), paste0("croparea.", 1:12)] <-
  cc.merge[cc.merge$crop %in% c(27:52), paste0("rainarea.", 1:12)]
cc.merge$tilearea[cc.merge$crop %in% c(27:52)] <- cc.merge$raintile[cc.merge$crop %in% c(27:52)]

## Calculate coverage
fc.cell <- apply(X = cc.merge, MARGIN = 1, FUN = add.fc.cell, columns = colnames(cc.merge))
fc.cell <- as.data.frame(t(fc.cell))
colnames(fc.cell) <- paste0("fc.cell.", 1:12)
fc.cell$maxfc.cell <- apply(X = fc.cell, MARGIN = 1, FUN = max)
fc.cell$meanfc.cell <- apply(X = fc.cell[, 1:(ncol(fc.cell) - 1)], MARGIN = 1, FUN = mean)
max(fc.cell$maxfc.cell)
min(fc.cell$maxfc.cell)

fc.tile <- apply(X = cc.merge, MARGIN = 1, FUN = add.fc.tile, columns = colnames(cc.merge))
fc.tile <- as.data.frame(t(fc.tile))
colnames(fc.tile) <- paste0("fc.tile.", 1:12)
fc.tile$maxfc.tile <- apply(X = fc.tile, MARGIN = 1, FUN = max)
fc.tile$meanfc.tile <- apply(X = fc.tile[, 1:(ncol(fc.tile) - 1)], MARGIN = 1, FUN = mean)
max(fc.tile$maxfc.tile)
min(fc.tile$maxfc.tile)

fc.crop <- apply(X = cc.merge, MARGIN = 1, FUN = add.fc.crop, columns = colnames(cc.merge))
fc.crop <- as.data.frame(t(fc.crop))
colnames(fc.crop) <- paste0("fc.crop.", 1:12)
fc.crop$maxfc.crop <- apply(X = fc.crop, MARGIN = 1, FUN = max)
fc.crop$meanfc.crop <- apply(X = fc.crop[, 1:(ncol(fc.crop) - 1)], MARGIN = 1, FUN = mean)
max(fc.crop$maxfc.crop)
min(fc.crop$maxfc.crop)

# Save
dir.create(dirname(fc.cell.out))
write.csv(fc.cell, fc.cell.out, row.names = F)
dir.create(dirname(fc.tile.out))
write.csv(fc.tile, fc.tile.out, row.names = F)
dir.create(dirname(fc.crop.out))
write.csv(fc.crop, fc.crop.out, row.names = F)
