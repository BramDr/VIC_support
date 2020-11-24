library(fields)
library(plyr)
rm(list = ls())

# Input
cc.file <- "../../../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
fc.file <- "Saves/subcropCalendar_coverageTile_30min_global.csv"
avgf.out <- "Saves/subcropCalendar_fractionTile_30min_global.csv"

# Load
cc <- read.csv(file = cc.file, stringsAsFactors = F)
fc <- read.csv(file = fc.file, stringsAsFactors = F)

# Calculate
cc <- cbind(cc, fc)
fc.cell.paddy <- aggregate(formula = meanfc.tile ~ cell_ID, data = cc[cc$crop == 3, ], FUN = sum)
fc.cell.paddy$paddy.sumfc <- fc.cell.paddy$meanfc
fc.cell.irr <- aggregate(formula = meanfc.tile ~ cell_ID, data = cc[cc$crop %in% c(1:2, 4:26), ], FUN = sum)
fc.cell.irr$irr.sumfc <- fc.cell.irr$meanfc
fc.cell.rain <- aggregate(formula = meanfc.tile ~ cell_ID, data = cc[cc$crop %in% c(27:52), ], FUN = sum)
fc.cell.rain$rain.sumfc <- fc.cell.rain$meanfc

cc.merge <- cc
cc.merge <- join(cc.merge, fc.cell.paddy[, c("cell_ID", "paddy.sumfc")])
cc.merge <- join(cc.merge, fc.cell.irr[, c("cell_ID", "irr.sumfc")])
cc.merge <- join(cc.merge, fc.cell.rain[, c("cell_ID", "rain.sumfc")])

cc.merge$avgf <- NA
cc.merge$avgf[cc.merge$crop == 3] <- cc.merge$meanfc[cc.merge$crop == 3] /
  cc.merge$paddy.sumfc[cc.merge$crop == 3]
cc.merge$avgf[cc.merge$crop %in% c(1:2, 4:26)] <- cc.merge$meanfc[cc.merge$crop %in% c(1:2, 4:26)] /
  cc.merge$irr.sumfc[cc.merge$crop %in% c(1:2, 4:26)]
cc.merge$avgf[cc.merge$crop %in% c(27:52)] <- cc.merge$meanfc[cc.merge$crop %in% c(27:52)] /
  cc.merge$rain.sumfc[cc.merge$crop %in% c(27:52)]
max(cc.merge$avgf)
min(cc.merge$avgf)

avgf.cell.paddy <- aggregate(formula = avgf ~ cell_ID, data = cc.merge[cc.merge$crop == 3, ], FUN = sum)
avgf.cell.paddy$paddy.maxavgf <- avgf.cell.paddy$avgf
avgf.cell.irr <- aggregate(formula = avgf ~ cell_ID, data = cc.merge[cc.merge$crop %in% c(1:2, 4:26), ], FUN = sum)
avgf.cell.irr$irr.maxavgf <- avgf.cell.irr$avgf
avgf.cell.rain <- aggregate(formula = avgf ~ cell_ID, data = cc.merge[cc.merge$crop %in% c(27:52), ], FUN = sum)
avgf.cell.rain$rain.maxavgf <- avgf.cell.rain$avgf

cc.merge <- join(cc.merge, avgf.cell.paddy[, c("cell_ID", "paddy.maxavgf")])
cc.merge <- join(cc.merge, avgf.cell.irr[, c("cell_ID", "irr.maxavgf")])
cc.merge <- join(cc.merge, avgf.cell.rain[, c("cell_ID", "rain.maxavgf")])

cc.merge$maxavgf <- NA
cc.merge$maxavgf[cc.merge$crop == 3] <- cc.merge$paddy.maxavgf[cc.merge$crop == 3]
cc.merge$maxavgf[cc.merge$crop %in% c(1:2, 4:26)] <- cc.merge$irr.maxavgf[cc.merge$crop %in% c(1:2, 4:26)]
cc.merge$maxavgf[cc.merge$crop %in% c(27:52)] <- cc.merge$rain.maxavgf[cc.merge$crop %in% c(27:52)]

# Save
dir.create(dirname(avgf.out))
write.csv(cc.merge[, c("avgf", "maxavgf")], avgf.out, row.names = F)
