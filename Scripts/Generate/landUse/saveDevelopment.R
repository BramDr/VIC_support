library(fields)
library(plyr)
rm(list = ls())

# Input
cc.file <- "../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
fao.file <- "../../../Data/Primary/FAO/Allen1998/FAO_crop_characteristics.csv"
Kc.out <- "Saves/croppingCalendars_development.csv"

# Load  
cc <- read.csv(file = cc.file, stringsAsFactors = F)
fao <- read.csv(fao.file, stringsAsFactors = F)

# Setup
days.per.month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

get.kc <- function(x, columns) {
  x <- as.numeric(x)
  print(x[columns == "rowname"])

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

# Calculate
## Calculate kc in steps due to the large number of lines to process
# steps <- seq(from = 1, to = nrow(cc), by = 500)
# for (i in 1:length(steps)) {
#   if (i == 1) {
#     next
#   } else if (i == 2) {
#     kc <- apply(X = cc[steps[i - 1]:(steps[i] - 1), ], MARGIN = 1, FUN = get.kc, columns = colnames(cc))
#   } else {
#     kc <- cbind(kc, apply(X = cc[steps[i - 1]:(steps[i] - 1), ], MARGIN = 1, FUN = get.kc, columns = colnames(cc)))
#   }
# }
# kc <- cbind(kc, apply(X = cc[steps[i]:nrow(cc), ], MARGIN = 1, FUN = get.kc, columns = colnames(cc)))
kc <- apply(X = cc, MARGIN = 1, FUN = get.kc, columns = colnames(cc))
kc <- as.data.frame(t(kc))
colnames(kc) = paste0("Kc.", 1:12)

kc$maxKc = apply(X = kc, MARGIN = 1, FUN = max)
kc$meanKc = apply(X = kc[,1:(ncol(kc) - 1)], MARGIN = 1, FUN = mean)

# Save
dir.create(dirname(Kc.out))
write.csv(kc, Kc.out, row.names = F)
