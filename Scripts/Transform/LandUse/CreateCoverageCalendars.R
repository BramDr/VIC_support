library(fields)
library(plyr)
rm(list = ls())

# Input
subcrop.file <- "../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
crop.file <- "../../../Data/Transformed/LandUse/CropCalendar_30min_global.csv"
cell.file <- "../../../Data/Transformed/LandUse/CellCalendar_30min_global.csv"
area.file <- "../../../Data/Primary/MIRCA2000/Cell area grid/cell_area_ha_30mn.asc"
fc.cell.out <- "../../../Data/Transformed/LandUse/subcropCalendar_coverageCell_30min_global.csv"
fc.cultivate.out <- "../../../Data/Transformed/LandUse/subcropCalendar_coverageCultivate_30min_global.csv"
fc.crop.out <- "../../../Data/Transformed/LandUse/subcropCalendar_coverageCrop_30min_global.csv"
fc.subcrop.out <- "../../../Data/Transformed/LandUse/subcropCalendar_coverageSubcrop_30min_global.csv"

# Load
subcrop <- read.csv(file = subcrop.file, stringsAsFactors = F)
crop <- read.csv(file = crop.file, stringsAsFactors = F)
cell <- read.csv(file = cell.file, stringsAsFactors = F)
area <- as.matrix(read.table(file = area.file, header = FALSE, sep = " ", skip = 6, stringsAsFactors = F))

# Setup
add.area.cell <- function(x, columns) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  out <- area[x[columns == "row"], x[columns == "column"]]

  return(out)
}
add.fc <- function(x, columns) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  maxValue <- x[columns == "maxarea"]
  out <- x[columns %in% paste0("area.", 1:12)] / maxValue

  return(out)
}

# Calculate
cc.merge <- merge(subcrop[, c("cell_ID", "crop", "subcrop", paste0("area.", 1:12))], subcrop[, c("cell_ID", "crop", "subcrop", "maxarea")], all.x = TRUE)
fc.subcrop <- apply(X = cc.merge, MARGIN = 1, FUN = add.fc, columns = colnames(cc.merge))
fc.subcrop <- as.data.frame(t(fc.subcrop))
colnames(fc.subcrop) <- paste0("fc.subcrop.", 1:12)
fc.subcrop$maxfc.subcrop <- apply(X = fc.subcrop, MARGIN = 1, FUN = max)
fc.subcrop$meanfc.subcrop <- apply(X = fc.subcrop[, 1:(ncol(fc.subcrop) - 1)], MARGIN = 1, FUN = mean)
max(fc.subcrop$maxfc.subcrop)
min(fc.subcrop$maxfc.subcrop)

cc.merge <- merge(subcrop[, c("cell_ID", "crop", "subcrop", paste0("area.", 1:12))], crop[, c("cell_ID", "crop", "maxarea")], all.x = TRUE)
fc.crop <- apply(X = cc.merge, MARGIN = 1, FUN = add.fc, columns = colnames(cc.merge))
fc.crop <- as.data.frame(t(fc.crop))
colnames(fc.crop) <- paste0("fc.crop.", 1:12)
fc.crop$maxfc.crop <- apply(X = fc.crop, MARGIN = 1, FUN = max)
fc.crop$meanfc.crop <- apply(X = fc.crop[, 1:(ncol(fc.crop) - 1)], MARGIN = 1, FUN = mean)
max(fc.crop$maxfc.crop)
min(fc.crop$maxfc.crop)

cc.merge <- merge(subcrop[, c("cell_ID", "crop", "subcrop", paste0("area.", 1:12))], cell[, c("cell_ID", "maxarea")], all.x = TRUE)
fc.cultivate <- apply(X = cc.merge, MARGIN = 1, FUN = add.fc, columns = colnames(cc.merge))
fc.cultivate <- as.data.frame(t(fc.cultivate))
colnames(fc.cultivate) <- paste0("fc.cultivate.", 1:12)
fc.cultivate$maxfc.cultivate <- apply(X = fc.cultivate, MARGIN = 1, FUN = max)
fc.cultivate$meanfc.cultivate <- apply(X = fc.cultivate[, 1:(ncol(fc.cultivate) - 1)], MARGIN = 1, FUN = mean)
max(fc.cultivate$maxfc.cultivate)
min(fc.cultivate$maxfc.cultivate)

cc.merge <- subcrop[, c("cell_ID", "crop", "subcrop", paste0("area.", 1:12))]
cc.merge$maxarea <- apply(X = subcrop, MARGIN = 1, FUN = add.area.cell, columns = colnames(subcrop))
fc.cell <- apply(X = cc.merge, MARGIN = 1, FUN = add.fc, columns = colnames(cc.merge))
fc.cell <- as.data.frame(t(fc.cell))
colnames(fc.cell) <- paste0("fc.cell.", 1:12)
fc.cell$maxfc.cell <- apply(X = fc.cell, MARGIN = 1, FUN = max)
fc.cell$meanfc.cell <- apply(X = fc.cell[, 1:(ncol(fc.cell) - 1)], MARGIN = 1, FUN = mean)
max(fc.cell$maxfc.cell)
min(fc.cell$maxfc.cell)

# Save
dir.create(dirname(fc.cell.out))
write.csv(fc.cell, fc.cell.out, row.names = F)
dir.create(dirname(fc.cultivate.out))
write.csv(fc.cultivate, fc.cultivate.out, row.names = F)
dir.create(dirname(fc.crop.out))
write.csv(fc.crop, fc.crop.out, row.names = F)
dir.create(dirname(fc.subcrop.out))
write.csv(fc.subcrop, fc.subcrop.out, row.names = F)
