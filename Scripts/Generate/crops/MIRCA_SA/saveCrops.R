rm(list = ls())

# Input
scc.file <- "../../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
crops.out <- "./Saves/crop_mapping.csv"

# Load
scc <- read.csv(scc.file, stringsAsFactors = F, header = T)

# sum(scc$area[scc$crop %in% c(1:3, 8)]) / sum(scc$area[scc$crop %in% c(1:24, 26)])
# sum(scc$area[scc$crop %in% (c(1:3, 8) + 26)]) / sum(scc$area[scc$crop %in% (c(1:24, 26) + 26)])

# Setup
name <- c("wheat", "wheat", "maize", "maize", "rice", "rice", "soybean", "soybean")
mirca <- c(1, 27, 2, 28, 3, 29, 8, 34)
vic <- c(14, 13, 16, 15, 18, 17, 20, 19)
wofost <- c("wheat", "wheat", "maize", "maize", "rice", "rice", "soybean", "soybean")
fertilizer <- c("whe", "whe", "mai", "mai", "ric", "ric", "soy", "soy")
water <- c("irrigated", "rainfed", "irrigated", "rainfed", "irrigated", "rainfed", "irrigated", "rainfed")
crop <- c("wheat", "wheat", "maize", "maize", "rice", "rice", "soybean", "soybean")
max.season <- c(2, 2, 1, 1, 2, 3, 1, 1)

# Calculate
crops <- data.frame(
  name = character(),
  mirca = numeric(),
  vic = numeric(),
  wofost = character(),
  fertilizer = character(),
  water = character(),
  season = numeric(),
  crop = character(),
  stringsAsFactors = F
)

for (i in 1:length(mirca)) {
  crop.scc <- scc[scc$crop == mirca[i], ]
  crop.season <- aggregate(x = crop.scc$subcrop, by = list(crop.scc$crop), FUN = max)

  for (j in 1:crop.season$x) {
    if (j > max.season[i]) {
      next
    }

    crops[nrow(crops) + 1, ] <- c(
      name[i], mirca[i], vic[i], wofost[i], fertilizer[i],
      water[i], j, crop[i]
    )
  }
}

# Save
dir.create(dirname(crops.out))
write.csv(crops, crops.out, row.names = F)
