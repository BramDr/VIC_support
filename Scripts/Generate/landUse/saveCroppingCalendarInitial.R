library(fields)
library(plyr)
rm(list = ls())

# Input
cc.file <- "../../../Data/Primary/MIRCA2000/Growing periods listed/cropping_calendars_30min.txt"
fao.file <- "../../../Data/Primary/FAO/Allen1998/FAO_crop_characteristics.csv"
cc.out <- "Saves/MIRCA2000_cropping_calendars_corrected.csv"

# Load
cc <- read.table(file = cc.file, header = TRUE, stringsAsFactors = F)
cc$rowname <- 1:nrow(cc)

fao <- read.csv(fao.file, stringsAsFactors = F)

# Setup
days.per.month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

get.area <- function(x, columns) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  if (x[columns == "start"] < x[columns == "end"]) {
    period <- x[columns == "start"]:x[columns == "end"]
  } else {
    period <- c(x[columns == "start"]:12, 1:x[columns == "end"])
  }

  area <- rep(0, 12)
  area[period] <- x[columns == "area"]
  return(area)
}

get.kc <- function(x, columns) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  row <- which((x[columns == "crop"] - 1) %% 26 + 1 == fao$number)

  factors <- as.numeric(fao[row, c("kc_init", "kc_mid", "kc_end")])
  lengths <- as.numeric(fao[row, c("len_init", "len_dev", "len_mid", "len_end")])
  factors <- factors / max(factors)
  lengths <- lengths / sum(lengths)

  if (x[columns == "start"] < x[columns == "end"]) {
    period <- x[columns == "start"]:x[columns == "end"]
  } else {
    period <- c(x[columns == "start"]:12, 1:x[columns == "end"])
  }

  period.d <- days.per.month[period]
  period.d.cs <- cumsum(period.d)
  period.m <- rep(1:length(period), days.per.month[period])

  # get development lenght in months and days
  lengths.d <- lengths * sum(period.d)
  lengths.d.cs <- round(cumsum(lengths.d), digits = 0)

  # set development stage per day
  dev.stage <- rep(0, sum(period.d))
  dev.stage[1:lengths.d.cs[1]] <- 1
  dev.stage[lengths.d.cs[1]:lengths.d.cs[2]] <- 2
  dev.stage[lengths.d.cs[2]:lengths.d.cs[3]] <- 3
  dev.stage[lengths.d.cs[3]:lengths.d.cs[4]] <- 4

  # set kc per day
  kc <- rep(NA, length(dev.stage))
  kc[dev.stage == 1] <- factors[1]
  kc[dev.stage == 2] <- factors[1] + (1:sum(dev.stage == 2) / (sum(dev.stage == 2) + 1)) * (factors[2] - factors[1])
  kc[dev.stage == 3] <- factors[2]
  kc[dev.stage == 4] <- factors[2] + (1:sum(dev.stage == 4) / (sum(dev.stage == 4) + 1)) * (factors[3] - factors[2])

  # aggregate kc to month
  kc.month <- rep(0, 12)
  kc.month[period] <- aggregate(x = kc, by = list(period.m), FUN = mean)[, 2]

  return(kc.month)
}

# Calculate KC
## Calculate kc in steps due to the large number of lines to process
steps <- seq(from = 1, to = nrow(cc), by = 500)
for (i in 1:length(steps)) {
  if (i == 1) {
    next
  } else if (i == 2) {
    monthly.kc <- apply(X = cc[steps[i - 1]:(steps[i] - 1), ], MARGIN = 1, FUN = get.kc, columns = colnames(cc))
  } else {
    monthly.kc <- cbind(monthly.kc, apply(X = cc[steps[i - 1]:(steps[i] - 1), ], MARGIN = 1, FUN = get.kc, columns = colnames(cc)))
  }
}
monthly.kc <- cbind(monthly.kc, apply(X = cc[steps[i]:nrow(cc), ], MARGIN = 1, FUN = get.kc, columns = colnames(cc)))
monthly.kc <- t(monthly.kc)

cc <- cbind(cc, monthly.kc)
new.colnames <- colnames(cc)[1:(ncol(cc) - 12)]
new.colnames <- c(new.colnames, paste0("Kc.", 1:12))
colnames(cc) <- new.colnames

# Calculate area
monthly.area <- apply(X = cc, MARGIN = 1, FUN = get.area, columns = colnames(cc))
monthly.area <- t(monthly.area)

cc <- cbind(cc, monthly.area)
new.colnames <- colnames(cc)[1:(ncol(cc) - 12)]
new.colnames <- c(new.colnames, paste0("Area.", 1:12))
colnames(cc) <- new.colnames

# Calculate cell area (to asses crop fraction per month)
cc.cell.paddy <- aggregate(
  formula = cbind(Area.1, Area.2, Area.3, Area.4, Area.5, Area.6, Area.7, Area.8, Area.9, Area.10, Area.11, Area.12) ~ cell_ID + row + column,
  data = cc[cc$crop == 3, ], FUN = sum
)
cc.cell.paddy$paddyMax <- apply(X = cc.cell.paddy[, paste0("Area.", 1:12)], MARGIN = 1, FUN = max)

cc.cell.irr <- aggregate(
  formula = cbind(Area.1, Area.2, Area.3, Area.4, Area.5, Area.6, Area.7, Area.8, Area.9, Area.10, Area.11, Area.12) ~ cell_ID + row + column,
  data = cc[cc$crop %in% c(1:2, 4:26), ], FUN = sum
)
cc.cell.irr$irrMax <- apply(X = cc.cell.irr[, paste0("Area.", 1:12)], MARGIN = 1, FUN = max)

cc.cell.rain <- aggregate(
  formula = cbind(Area.1, Area.2, Area.3, Area.4, Area.5, Area.6, Area.7, Area.8, Area.9, Area.10, Area.11, Area.12) ~ cell_ID + row + column,
  data = cc[cc$crop %in% c(27:52), ], FUN = sum
)
cc.cell.rain$rainMax <- apply(X = cc.cell.rain[, paste0("Area.", 1:12)], MARGIN = 1, FUN = max)

cc.merge <- cc
cc.merge <- join(cc.merge, cc.cell.paddy[, c("cell_ID", "paddyMax")])
cc.merge <- join(cc.merge, cc.cell.irr[, c("cell_ID", "irrMax")])
cc.merge <- join(cc.merge, cc.cell.rain[, c("cell_ID", "rainMax")])
cc.merge$areaMax <- NA
cc.merge$areaMax[cc.merge$crop == 3] <- cc.merge$paddyMax[cc.merge$crop == 3]
cc.merge$areaMax[cc.merge$crop %in% c(1:2, 4:26)] <- cc.merge$irrMax[cc.merge$crop %in% c(1:2, 4:26)]
cc.merge$areaMax[cc.merge$crop %in% c(27:52)] <- cc.merge$rainMax[cc.merge$crop %in% c(27:52)]

cc.merge <- subset(cc.merge, select = -c(paddyMax, irrMax, rainMax))

# Save
dir.create(dirname(cc.out))
write.csv(cc.merge, cc.out, row.names = F)
