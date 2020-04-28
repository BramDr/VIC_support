library(fields)
rm(list = ls())

# Input
soil.file <- "../../../Data/Primary/vanVliet2016/global_soil_file.new.arno.modified.fe.wfd"
depth.out <- "./Saves/depth_Vliet30min_30min_global.RDS"

# Load
soil <- read.table(file = soil.file)

# Setup
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
nlayers <- 3

# Calculate
depth.map <- array(NA, dim = c(length(lons), length(lats), nlayers))
for (i in 1:nrow(soil)) {
  x <- which(lons == soil[i, 4])
  y <- which(lats == soil[i, 3])

  depth.map[x, y, 1] <- soil[i, 23]
  depth.map[x, y, 2] <- soil[i, 24]
  depth.map[x, y, 3] <- soil[i, 25]
}
image.plot(depth.map[, , 2])

# Save
dir.create(dirname(depth.out))
saveRDS(depth.map, depth.out)
