library(fields)
rm(list = ls())

# Input
soil.file <- "../../../Data/Primary/Nijssen2001/0.5deg/global_soil_param_new"
header.file <- "../../../Data/Primary/Nijssen2001/2deg/world.soil.parameter.hdr"
frozen.out <- "./Saves/frozen_Nijssen120min_30min_global.RDS"

# Load
soil <- read.table(soil.file)
header <- read.table(header.file, stringsAsFactors = F)
header <- c(unlist(header[, 1:27]), paste0("bubble[", 1:3, "]"), paste0("quartz[", 1:3, "]"), unlist(header[, 30:48]), "frozen_flag")

# Setup
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)

fs_active.idx <- which(header == "frozen_flag")
T_avg.idx <- which(header == "avg_T")

# Calculate
fs_active.map <- array(NA, dim = c(length(lons), length(lats)))

for (i in 1:nrow(soil)) {
  x <- which(lons == soil[i, 4])
  y <- which(lats == soil[i, 3])

  # fs_active.map[x,y] = 0
  fs_active.map[x, y] <- as.numeric(soil[i, T_avg.idx] <= 5)
}
image.plot(fs_active.map)

# Save
dir.create(dirname(frozen.out))
saveRDS(fs_active.map, frozen.out)
