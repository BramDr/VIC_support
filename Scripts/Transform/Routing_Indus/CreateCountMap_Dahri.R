library(ncdf4)
library(fields)
rm(list = ls())

# Input
downstream.file <- "../../../Data/Transformed/Routing/downstream_Dahri_5min_Indus.RDS"
count.out <- "../../../Data/Transformed/Routing/count_Dahri_5min_Indus.RDS"

# Load
downstream <- readRDS(downstream.file)

# Calculate
count <- array(0, dim = dim(downstream)[1:2])
for (x in 1:dim(downstream)[1]) {
  for (y in 1:dim(downstream)[2]) {
    if (is.na(downstream[x, y, 1])) {
      next
    }

    cur <- c(x, y)
    nex <- downstream[x, y, ]

    while (TRUE) {
      count[cur[1], cur[2]] <- count[cur[1], cur[2]] + 1

      if (cur[1] == nex[1] && cur[2] == nex[2]) {
        break
      }

      cur <- nex
      nex <- downstream[cur[1], cur[2], ]
    }
  }
}
image.plot(count)

# Save
dir.create(dirname(count.out))
saveRDS(count, count.out)
