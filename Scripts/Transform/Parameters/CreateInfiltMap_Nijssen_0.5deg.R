library(fields)
rm(list = ls())

# Input
soil.file <- "../../../Data/Primary/Nijssen2001/0.5deg/global_soil_param_new"
infilt.out <- "../../../Data/Transformed/Parameters/infilt_Nijssen30min_30min_global.RDS"

# Load
soil <- read.table(file = soil.file)

# Setup
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)

# Calculate
infilt.map <- array(NA, dim = c(length(lons), length(lats)))
for (i in 1:nrow(soil)) {
  x <- which(lons == soil[i, 4])
  y <- which(lats == soil[i, 3])

  infilt.map[x, y] <- soil[i, 5]
}
image.plot(infilt.map)

# Save
dir.create(dirname(infilt.out))
saveRDS(infilt.map, infilt.out)
