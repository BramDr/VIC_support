rm(list = ls())

# Input
scc.file <- "./Saves/subcropCalendar_5min_Indus.RDS"
crops.out <- "./Saves/crop_mapping.RDS"

# Load
scc <- readRDS(scc.file)

# Setup
name <- c("wheat", "wheat", "rice", "rice")
mirca <- c(1, 27, 3, 29)
vic <- c(18, 17, 20, 19)
wofost <- c("wheat", "wheat", "rice", "rice")
fertilizer <- c("whe", "whe", "ric", "ric")
water <- c("irrigated", "rainfed", "irrigated", "rainfed")
crop <- c("wheat", "wheat", "rice", "rice")
season <- c("max","max",2,2)

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

i = 1
for (i in 1:length(mirca)) {
  if (season[i] == "max") {
    crops[nrow(crops) + 1, ] <- c(
      name[i], mirca[i], vic[i], wofost[i], fertilizer[i],
      water[i], "max", crop[i]
    )
  } else {
    for (j in 1:season[i]) {
      crops[nrow(crops) + 1, ] <- c(
        name[i], mirca[i], vic[i], wofost[i], fertilizer[i],
        water[i], j, crop[i]
      )
    }
  }
}

# Save
dir.create(dirname(crops.out))
saveRDS(crops, crops.out)
