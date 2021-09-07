library(fields)
library(ncdf4)
rm(list = ls())

# Input
ldam.file <- "Saves/localDamsMerge.csv"
gdam.file <- "Saves/globalDamsMerge.csv"
accumulation.file <- "../../../Data/Transformed/Routing/accumulation_5min_Indus.RDS"
downstream.file <- "../../../Data/Transformed/Routing/downstream_5min_Indus.RDS"
ldam.service.out <- "Saves/localDamsService.RDS"
gdam.service.out <- "Saves/globalDamsService.RDS"

# Load
accumulation = readRDS(accumulation.file)
downstream <- readRDS(downstream.file)
ldam <- read.csv(ldam.file, stringsAsFactors = F)
gdam <- read.csv(gdam.file, stringsAsFactors = F)

# Calculate
gdam.services <- list()
i = 7
for (i in 1:nrow(gdam)) {
  x <- gdam$MODEL_X[i]
  y <- gdam$MODEL_Y[i]

  gdam.service <- data.frame(
    X = numeric(),
    Y = numeric(),
    FRAC = numeric()
  )

  if (is.na(downstream[x, y, 1]) || !(gdam$USE_IRR[i] == 1)) {
    gdam.services[[length(gdam.services) + 1]] <- gdam.service
    next
  }

  cur <- c(x, y)
  nex <- downstream[x, y, ]
  service.map = array(0, dim = dim(accumulation))
  while (TRUE) {
    frac <- accumulation[x, y] / accumulation [cur[1], cur[2]]
    
    # exclude fractions < 0.25
    if (frac < 0.25) {
      break
    }

    gdam.service <- rbind(gdam.service, c(cur[1], cur[2], frac))
    service.map[cur[1], cur[2]] = frac
    

    if (cur[1] == nex[1] && cur[2] == nex[2]) {
      break
    }

    cur <- nex
    nex <- downstream[cur[1], cur[2], ]
  }
  # image.plot(service.map)

  colnames(gdam.service) <- c("X", "Y", "FRAC")
  gdam.services[[length(gdam.services) + 1]] <- gdam.service
}

ldam.services <- list()
for (i in 1:nrow(ldam)) {
  ldam.service <- data.frame(
    X = numeric(),
    Y = numeric(),
    FRAC = numeric()
  )
  ldam.services[[length(ldam.services) + 1]] <- ldam.service
}

# visualize
gdam.service.map <- array(NA, dim = dim(accumulation))
ldam.service.map <- array(NA, dim = dim(accumulation))
for (x in 1:dim(accumulation)[1]) {
  for (y in 1:dim(accumulation)[2]) {
    if (is.na(accumulation[x, y])) {
      next
    }
    gdam.service.map[x, y] <- 0
    ldam.service.map[x, y] <- 0
  }
}

for (i in 1:length(gdam.services)) {
  gdam.service <- gdam.services[[i]]

  if (nrow(gdam.service) == 0) {
    next
  }

  for (j in 1:nrow(gdam.service)) {
    x <- gdam.service$X[j]
    y <- gdam.service$Y[j]

    gdam.service.map[x, y] <- gdam.service.map[x, y] + gdam.service$FRAC[j]
  }
}
image.plot(gdam.service.map, zlim = c(0,2))

for (i in 1:length(ldam.services)) {
  ldam.service <- ldam.services[[i]]

  if (nrow(ldam.service) == 0) {
    next
  }

  for (j in 1:nrow(ldam.service)) {
    x <- ldam.service$X[j]
    y <- ldam.service$Y[j]

    ldam.service.map[x, y] <- ldam.service.map[x, y] + ldam.service$FRAC[j]
  }
}
image.plot(ldam.service.map, zlim = c(0, 1))

# Save
dir.create(dirname(gdam.service.out))
saveRDS(gdam.services, gdam.service.out)
dir.create(dirname(ldam.service.out))
saveRDS(ldam.services, ldam.service.out)

