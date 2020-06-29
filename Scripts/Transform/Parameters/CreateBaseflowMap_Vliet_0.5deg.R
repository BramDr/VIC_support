library(fields)
rm(list = ls())

# Input
soil.file <- "../../../Data/Primary/vanVliet2016/global_soil_file.new.arno.modified.fe.wfd"
baseflow.out <- "../../../Data/Transformed/Parameters/baseflow_Vliet30min_30min_global.RDS"

# Load
soil <- read.table(file = soil.file)

# Setup
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)

# Calculate
d1.map <- array(NA, dim = c(length(lons), length(lats)))
d2.map <- array(NA, dim = c(length(lons), length(lats)))
d3.map <- array(NA, dim = c(length(lons), length(lats)))
d4.map <- array(NA, dim = c(length(lons), length(lats)))
for (i in 1:nrow(soil)) {
  x <- which(lons == soil[i, 4])
  y <- which(lats == soil[i, 3])

  d1.map[x, y] <- soil[i, 6]
  d2.map[x, y] <- soil[i, 7]
  d3.map[x, y] <- soil[i, 8]
  d4.map[x, y] <- soil[i, 9]
}
image.plot(d1.map)
image.plot(d2.map)
image.plot(d3.map)
image.plot(d4.map)

# Save
baseflow.out.tmp <- gsub(x = baseflow.out, pattern = "baseflow", replacement = "baseflowD1")
dir.create(dirname(baseflow.out.tmp))
saveRDS(d1.map, baseflow.out.tmp)
baseflow.out.tmp <- gsub(x = baseflow.out, pattern = "baseflow", replacement = "baseflowD2")
dir.create(dirname(baseflow.out.tmp))
saveRDS(d2.map, baseflow.out.tmp)
baseflow.out.tmp <- gsub(x = baseflow.out, pattern = "baseflow", replacement = "baseflowD3")
dir.create(dirname(baseflow.out.tmp))
saveRDS(d3.map, baseflow.out.tmp)
baseflow.out.tmp <- gsub(x = baseflow.out, pattern = "baseflow", replacement = "baseflowD4")
dir.create(dirname(baseflow.out.tmp))
saveRDS(d4.map, baseflow.out.tmp)
