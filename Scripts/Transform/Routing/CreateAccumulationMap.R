library(ncdf4)
library(fields)
rm(list = ls())

# Input
area.file <- "../../../Data/Transformed/Routing/area_30min_global.RDS"
downstream.file <- "../../../Data/Transformed/Routing/downstream_30min_global.RDS"
accumulation.out <- "../../../Data/Transformed/Routing/accumulation_30min_global.RDS"

# Load
downstream <- readRDS(downstream.file)
area <- readRDS(area.file)

# Calculate
accumulation <- array(0, dim = dim(area))
for (x in 1:dim(area)[1]) {
  for (y in 1:dim(area)[2]) {
    cur <- c(x, y)
    nex <- downstream[x, y, ]

    while (TRUE) {
      accumulation[cur[1], cur[2]] <- accumulation[cur[1], cur[2]] + area[x, y]

      if (cur[1] == nex[1] && cur[2] == nex[2]) {
        break
      }

      cur <- nex
      nex <- downstream[cur[1], cur[2], ]
    }
  }
}
image.plot(accumulation)

# Save
dir.create(dirname(accumulation.out))
saveRDS(accumulation, accumulation.out)
