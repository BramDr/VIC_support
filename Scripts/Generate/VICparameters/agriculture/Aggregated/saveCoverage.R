library(fields)
library(plyr)
rm(list = ls())

# Input
cc.file <- "../../../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
coverage.file <- "../../../../../Data/Transformed/LandUse/subcropCalendar_coverageCrop_30min_global.csv"
area.file <- "../../../../../Data/Primary/MIRCA2000/Cell area grid/cell_area_ha_30mn.asc"
fc.tile.out <- "Saves/subcropCalendar_coverageTile_30min_global.csv"
fc.tile.monthly.out <- "Saves/subcropCalendar_coverageTileMonthly_30min_global.csv"

# Load
cc <- read.csv(file = cc.file, stringsAsFactors = F)
coverage <- read.csv(file = coverage.file, stringsAsFactors = F)
area <- as.matrix(read.table(file = area.file, header = FALSE, sep = " ", skip = 6, stringsAsFactors = F))

# Setup
add.fc.tile <- function(x, columns) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  maxValue <- x[columns == "tilearea"]
  out <- x[columns %in% paste0("area.", 1:12)] / maxValue

  return(out)
}
add.fc.tile.monthly <- function(x, columns) {
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
cc.cell.paddy$maxpaddyarea <- apply(X = cc.cell.paddy[, paste0("paddyarea.", 1:12)], MARGIN = 1, FUN = max)

cc.cell.irr <- aggregate(
  formula = cbind(area.1, area.2, area.3, area.4, area.5, area.6, area.7, area.8, area.9, area.10, area.11, area.12) ~ cell_ID + row + column,
  data = cc[cc$crop %in% c(1:2, 4:26), ], FUN = sum
)
cc.cell.irr[, paste0("irrarea.", 1:12)] <- cc.cell.irr[, paste0("area.", 1:12)]
cc.cell.irr$maxirrarea <- apply(X = cc.cell.irr[, paste0("irrarea.", 1:12)], MARGIN = 1, FUN = max)

cc.cell.rain <- aggregate(
  formula = cbind(area.1, area.2, area.3, area.4, area.5, area.6, area.7, area.8, area.9, area.10, area.11, area.12) ~ cell_ID + row + column,
  data = cc[cc$crop %in% c(27:52), ], FUN = sum
)
cc.cell.rain[, paste0("rainarea.", 1:12)] <- cc.cell.rain[, paste0("area.", 1:12)]
cc.cell.rain$maxrainarea <- apply(X = cc.cell.rain[, paste0("rainarea.", 1:12)], MARGIN = 1, FUN = max)

## Merge
cc.merge <- cc
cc.merge <- join(cc.merge, cc.cell.paddy[, c("cell_ID", "maxpaddyarea", paste0("paddyarea.", 1:12))])
cc.merge <- join(cc.merge, cc.cell.irr[, c("cell_ID", "maxirrarea", paste0("irrarea.", 1:12))])
cc.merge <- join(cc.merge, cc.cell.rain[, c("cell_ID", "maxrainarea", paste0("rainarea.", 1:12))])
cc.merge$croparea <- NA
cc.merge$tilearea <- NA

cc.merge[cc.merge$crop == 3, paste0("croparea.", 1:12)] <-
  cc.merge[cc.merge$crop == 3, paste0("paddyarea.", 1:12)]
cc.merge$tilearea[cc.merge$crop == 3] <- cc.merge$maxpaddyarea[cc.merge$crop == 3]

cc.merge[cc.merge$crop %in% c(1:2, 4:26), paste0("croparea.", 1:12)] <-
  cc.merge[cc.merge$crop %in% c(1:2, 4:26), paste0("irrarea.", 1:12)]
cc.merge$tilearea[cc.merge$crop %in% c(1:2, 4:26)] <- cc.merge$maxirrarea[cc.merge$crop %in% c(1:2, 4:26)]

cc.merge[cc.merge$crop %in% c(27:52), paste0("croparea.", 1:12)] <-
  cc.merge[cc.merge$crop %in% c(27:52), paste0("rainarea.", 1:12)]
cc.merge$tilearea[cc.merge$crop %in% c(27:52)] <- cc.merge$maxrainarea[cc.merge$crop %in% c(27:52)]

## Calculate coverage
fc.tile <- apply(X = cc.merge, MARGIN = 1, FUN = add.fc.tile, columns = colnames(cc.merge))
fc.tile <- as.data.frame(t(fc.tile))
colnames(fc.tile) <- paste0("fc.tile.", 1:12)
fc.tile$maxfc.tile <- apply(X = fc.tile, MARGIN = 1, FUN = max)
fc.tile$meanfc.tile <- apply(X = fc.tile[, 1:(ncol(fc.tile) - 1)], MARGIN = 1, FUN = mean)
max(fc.tile$maxfc.tile)
min(fc.tile$maxfc.tile)

fc.tile.monthly <- apply(X = cc.merge, MARGIN = 1, FUN = add.fc.tile.monthly, columns = colnames(cc.merge))
fc.tile.monthly <- as.data.frame(t(fc.tile.monthly))
colnames(fc.tile.monthly) <- paste0("fc.tile.monthly.", 1:12)
fc.tile.monthly$maxfc.tile.monthly <- apply(X = fc.tile.monthly, MARGIN = 1, FUN = max)
fc.tile.monthly$meanfc.tile.monthly <- apply(X = fc.tile.monthly[, 1:(ncol(fc.tile.monthly) - 1)], MARGIN = 1, FUN = mean)
max(fc.tile.monthly$maxfc.tile.monthly)
min(fc.tile.monthly$maxfc.tile.monthly)

# Save
dir.create(dirname(fc.tile.out))
write.csv(fc.tile, fc.tile.out, row.names = F)
dir.create(dirname(fc.tile.monthly.out))
write.csv(fc.tile.monthly, fc.tile.monthly.out, row.names = F)
