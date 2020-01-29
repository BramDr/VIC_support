library(fields)
library(ncdf4)

rm(list = ls())

# Input
dam.file <- "../../../Data/Transformed/Dams/GRanDcorrected.csv"
ldam.out <- "Saves/localDams.csv"
gdam.out <- "Saves/globalDams.csv"
ldam.merge.out <- "Saves/localDamsMerge.csv"
gdam.merge.out <- "Saves/globalDamsMerge.csv"

# Load
dams <- read.csv(dam.file, stringsAsFactors = F)

# Setup
local.rows <- c()
global.rows <- c()
for (i in 1:nrow(dams)) {
  if (dams$MODEL_AREA_FRAC[i] < 1) {
    local.rows <- c(local.rows, i)
  } else {
    global.rows <- c(global.rows, i)
  }
}
gdams <- dams[global.rows, ]
ldams <- dams[local.rows, ]

# Calculate
gdams.merge <- aggregate(formula = cbind(CAP_MCM, MODEL_AREA_FRAC) ~ MODEL_LONG_DD + MODEL_LAT_DD, data = gdams, FUN = sum, na.rm = T)
gdams.merge <- merge(gdams.merge, aggregate(formula = cbind(YEAR) ~ MODEL_LONG_DD + MODEL_LAT_DD, data = gdams, FUN = min, na.rm = T))
gdams.merge <- merge(gdams.merge, aggregate(formula = cbind(CATCH_SKM) ~ MODEL_LONG_DD + MODEL_LAT_DD, data = gdams, FUN = max, na.rm = T))
gdams.merge <- merge(gdams.merge, aggregate(formula = cbind(USE_IRRI, USE_SUPP) ~ MODEL_LONG_DD + MODEL_LAT_DD, data = gdams, FUN = function(x) {
  max(x != "", na.rm = T)
}))
gdams.merge$MODEL_AREA_FRAC[gdams.merge$MODEL_AREA_FRAC > 1] <- 1

ldams.merge <- aggregate(formula = cbind(CAP_MCM, MODEL_AREA_FRAC) ~ MODEL_LONG_DD + MODEL_LAT_DD, data = ldams, FUN = sum, na.rm = T)
ldams.merge <- merge(ldams.merge, aggregate(formula = cbind(YEAR) ~ MODEL_LONG_DD + MODEL_LAT_DD, data = ldams, FUN = min, na.rm = T))
ldams.merge <- merge(ldams.merge, aggregate(formula = cbind(CATCH_SKM) ~ MODEL_LONG_DD + MODEL_LAT_DD, data = ldams, FUN = max, na.rm = T))
ldams.merge <- merge(ldams.merge, aggregate(formula = cbind(USE_IRRI, USE_SUPP) ~ MODEL_LONG_DD + MODEL_LAT_DD, data = ldams, FUN = function(x) {
  max(x != "", na.rm = T)
}))
ldams.merge$MODEL_AREA_FRAC[ldams.merge$MODEL_AREA_FRAC > 1] <- 1

# Save
dir.create(dirname(ldam.out))
write.csv(x = ldams, file = ldam.out, row.names = F)
dir.create(dirname(gdam.out))
write.csv(x = gdams, file = gdam.out, row.names = F)
dir.create(dirname(ldam.merge.out))
write.csv(x = ldams.merge, file = ldam.merge.out, row.names = F)
dir.create(dirname(gdam.merge.out))
write.csv(x = gdams.merge, file = gdam.merge.out, row.names = F)
