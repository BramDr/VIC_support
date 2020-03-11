library(fields)
library(plyr)
rm(list = ls())

# Input
cc.file <- "../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
fc.file <- "Saves/croppingCalendars_coverageTile.csv"
avgf.out <- "Saves/croppingCalendars_fraction.csv"

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

avgf <- as.data.frame(cc.merge$avgf)
colnames(avgf) <- "avgf"

# Save
dir.create(dirname(avgf.out))
write.csv(cc.merge$avgf, avgf.out, row.names = F)
