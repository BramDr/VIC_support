library(fields)
library(ncdf4)
rm(list = ls())

# Input
crop.file <- "./Saves/crop_mapping.csv"
cc.file <- "../../../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
Cv.file <- "../../../../../Data/Transformed/LandUse/subcropCalendar_coverageCell_30min_global.csv"
cv.monthly.out <- "Saves/Cv_monthly_30min_global.RDS"

# Load
crops <- read.csv(crop.file, stringsAsFactors = F)
cc <- read.csv(cc.file, stringsAsFactors = F)
Cv <- read.csv(Cv.file, stringsAsFactors = F)

# Setup
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
lons <- seq(from = -179.75, to = 179.75, by = 0.5)

# Calculate
map <- array(NA, dim = c(length(lons), length(lats), nrow(crops), 12))

i <- 1
for (i in 1:nrow(crops)) {
  print(crops$name[i])

  sel <- cc$crop == crops$mirca[i] & cc$subcrop == crops$season[i]
  cc.crop <- cc[sel, ]
  Cv.crop <- Cv[sel, ]

  for (j in 1:nrow(Cv.crop)) {
    x <- which(lons == cc.crop$lon[j])
    y <- which(lats == cc.crop$lat[j])

    map[x, y, i, ] <- as.numeric(Cv.crop[j, paste0("fc.cell.", 1:12)])
  }
}
map[!is.na(map) & map > 0] <- 1

image.plot(map[, , 1, 1])
image.plot(map[, , 1, 8])

# Save
dir.create(dirname(cv.monthly.out))
saveRDS(map, cv.monthly.out)
