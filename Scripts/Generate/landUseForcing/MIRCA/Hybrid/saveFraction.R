library(fields)
library(plyr)
rm(list = ls())

# Input
crop.file <- "./Saves/crop_mapping.csv"
cc.file <- "../../../../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
fc.file <- "Saves/subcropCalendar_coverageTile_30min_global.csv"
avgf.out <- "Saves/subcropCalendar_fractionTile_30min_global.csv"
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
cc <- read.csv(file = cc.file, stringsAsFactors = F)
fc <- read.csv(file = fc.file, stringsAsFactors = F)

# Calculate
cc <- cbind(cc, fc)
fc.cell.irr <- aggregate(formula = meanfc.tile ~ cell_ID, data = cc[cc$crop %in% c(1:2, 4:26) & !cc$crop %in% split$id, ], FUN = sum)
fc.cell.irr$irr.sumfc <- fc.cell.irr$meanfc
fc.cell.rain <- aggregate(formula = meanfc.tile ~ cell_ID, data = cc[cc$crop %in% c(27:52) & !cc$crop %in% split$id, ], FUN = sum)
fc.cell.rain$rain.sumfc <- fc.cell.rain$meanfc

for (i in 1:nrow(split)) {
  fc.cell.split <- aggregate(formula = meanfc.tile ~ cell_ID, data = cc[cc$crop %in% split$id[i], ], FUN = sum)
  fc.cell.split[, paste0(split$name[i], ".sumfc")] <- fc.cell.split$meanfc
  assign(x = paste0("fc.cell.split.", i), value = fc.cell.split)
}

cc.merge <- cc
cc.merge <- join(cc.merge, fc.cell.irr[, c("cell_ID", "irr.sumfc")])
cc.merge <- join(cc.merge, fc.cell.rain[, c("cell_ID", "rain.sumfc")])

for (i in 1:nrow(split)) {
  fc.cell.split <- get(x = paste0("fc.cell.split.", i))
  cc.merge <- join(cc.merge, fc.cell.split[, c("cell_ID", paste0(split$name[i], ".sumfc"))])
}

cc.merge$avgf <- NA
cc.merge$avgf[cc.merge$crop %in% c(1:2, 4:26) & !cc$crop %in% split$id] <-
  cc.merge$meanfc[cc.merge$crop %in% c(1:2, 4:26) & !cc$crop %in% split$id] /
    cc.merge$irr.sumfc[cc.merge$crop %in% c(1:2, 4:26) & !cc$crop %in% split$id]
cc.merge$avgf[cc.merge$crop %in% c(27:52) & !cc$crop %in% split$id] <-
  cc.merge$meanfc[cc.merge$crop %in% c(27:52) & !cc$crop %in% split$id] /
    cc.merge$rain.sumfc[cc.merge$crop %in% c(27:52) & !cc$crop %in% split$id]

for (i in 1:nrow(split)) {
  cc.merge$avgf[cc$crop %in% split$id[i]] <-
    cc.merge$meanfc[cc$crop %in% split$id[i]] /
      cc.merge[cc$crop %in% split$id[i], paste0(split$name[i], ".sumfc")]
}

max(cc.merge$avgf)
min(cc.merge$avgf)

avgf.cell.irr <- aggregate(formula = avgf ~ cell_ID, data = cc.merge[cc.merge$crop %in% c(1:2, 4:26) & !cc$crop %in% split$id, ], FUN = sum)
avgf.cell.irr$irr.maxavgf <- avgf.cell.irr$avgf
avgf.cell.rain <- aggregate(formula = avgf ~ cell_ID, data = cc.merge[cc.merge$crop %in% c(27:52) & !cc$crop %in% split$id, ], FUN = sum)
avgf.cell.rain$rain.maxavgf <- avgf.cell.rain$avgf

for (i in 1:nrow(split)) {
  seasons <- crops$season[crops$mirca == split$id[i]]
  avgf.cell.split <- aggregate(formula = avgf ~ cell_ID, data = cc.merge[cc$crop %in% split$id[i] & cc$subcrop %in% seasons, ], FUN = sum)
  avgf.cell.split[, paste0(split$name[i], ".maxavgf")] <- avgf.cell.split$avgf
  assign(x = paste0("avgf.cell.split.", i), value = avgf.cell.split)
}

cc.merge <- join(cc.merge, avgf.cell.irr[, c("cell_ID", "irr.maxavgf")])
cc.merge <- join(cc.merge, avgf.cell.rain[, c("cell_ID", "rain.maxavgf")])

for (i in 1:nrow(split)) {
  avgf.cell.split <- get(x = paste0("avgf.cell.split.", i))
  cc.merge <- join(cc.merge, avgf.cell.split[, c("cell_ID", paste0(split$name[i], ".maxavgf"))])
}

cc.merge$maxavgf <- NA
cc.merge$maxavgf[cc.merge$crop %in% c(1:2, 4:26) & !cc$crop %in% split$id] <-
  cc.merge$irr.maxavgf[cc.merge$crop %in% c(1:2, 4:26) & !cc$crop %in% split$id]
cc.merge$maxavgf[cc.merge$crop %in% c(27:52) & !cc$crop %in% split$id] <-
  cc.merge$rain.maxavgf[cc.merge$crop %in% c(27:52) & !cc$crop %in% split$id]

for (i in 1:nrow(split)) {
  cc.merge$maxavgf[cc$crop %in% split$id[i]] <-
    cc.merge[cc$crop %in% split$id[i], paste0(split$name[i], ".maxavgf")]
}

# Save
dir.create(dirname(avgf.out))
write.csv(cc.merge[, c("avgf", "maxavgf")], avgf.out, row.names = F)
