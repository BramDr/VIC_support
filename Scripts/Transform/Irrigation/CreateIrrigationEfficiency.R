library(fields)
library(openxlsx)
library(raster)
rm(list = ls())

# Input
eff.file <- "../../../Data/Primary/FAO/Frenken2012/irrigationEfficiencySubregion.xlsx"
subregion.file <- "../../../Data/Transformed/Country/subregion_6min_global.RDS"
eff.out <- "../../../Data/Transformed/Irrigation/irrigationEfficiency_30min_global.RDS"

# Load
eff <- read.xlsx(eff.file)
subregion <- readRDS(subregion.file)

# Calculate
eff.map <- array(NA, dim = dim(subregion))
for (x in 1:dim(eff.map)[1]) {
  for (y in 1:dim(eff.map)[2]) {
    if (is.na(subregion[x, y])) {
      next
    }

    row <- which(eff$Subregion_number == subregion[x, y])
    eff.map[x, y] <- 1 / (eff$Efficiency[row] / 100)
  }
}
image.plot(eff.map)

eff.map.r <- raster(eff.map)
extent(eff.map.r) <- c(-90, 90, -180, 80)
eff.map.agg <- aggregate(eff.map.r, fact = 5, fun = mean)
eff.map.agg <- as.matrix(eff.map.agg)
image.plot(eff.map.agg)

# Save
dir.create(dirname(eff.out))
saveRDS(eff.map.agg, eff.out)
