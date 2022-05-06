library(fields)
library(ncdf4)
rm(list = ls())

# Input
ldam.file <- "Saves/localDamsMerge.csv"
gdam.file <- "Saves/globalDamsMerge.csv"
discharge.file <- "../../../Data/Transformed/VIC/fluxes_VlietAlt30min_WB.yearmean.nc"
downstream.file <- "../../../Data/Transformed/Routing/downstream_30min_global.RDS"
id.file = "./Saves/idMap.RDS"
ldam.service.out <- "Saves/localDamsService.RDS"
gdam.service.out <- "Saves/globalDamsService.RDS"
ldam.service.fraction.out <- "Saves/localDamsServiceFraction.RDS"
gdam.service.fraction.out <- "Saves/globalDamsServiceFraction.RDS"

# Load
nc <- nc_open(discharge.file)
discharge <- apply(X = ncvar_get(nc, "OUT_DISCHARGE", start = c(1, 1, 2), count = c(-1, -1, -1)), MARGIN = c(1, 2), FUN = mean)
nc_close(nc)

downstream <- readRDS(downstream.file)
id = readRDS(id.file)
ldam <- read.csv(ldam.file, stringsAsFactors = F)
gdam <- read.csv(gdam.file, stringsAsFactors = F)

# Setup
lats <- seq(-89.75, 89.75, by = 0.5)
lons <- seq(-179.75, 179.75, by = 0.5)

gdam$MODEL_X <- NA
gdam$MODEL_Y <- NA
for (i in 1:nrow(gdam)) {
  gdam$MODEL_X[i] <- which(gdam$MODEL_LONG_DD[i] == lons)
  gdam$MODEL_Y[i] <- which(gdam$MODEL_LAT_DD[i] == lats)
}
ldam$MODEL_X <- NA
ldam$MODEL_Y <- NA
for (i in 1:nrow(ldam)) {
  ldam$MODEL_X[i] <- which(ldam$MODEL_LONG_DD[i] == lons)
  ldam$MODEL_Y[i] <- which(ldam$MODEL_LAT_DD[i] == lats)
}

# Calculate
Ndams = array(0, dim = dim(id))
for (i in order(gdam$CAP_MCM)) {
  print(i)
  
  x <- gdam$MODEL_X[i]
  y <- gdam$MODEL_Y[i]
  Ndams[x,y] = Ndams[x,y] + 1
}
image.plot(Ndams)

global.service.map = array(NA, dim = c(dim(id), nrow(gdam)))
global.service.fraction.map = array(NA, dim = c(dim(id), nrow(gdam)))
i = 1082
for (i in order(gdam$CAP_MCM)) {
  x <- gdam$MODEL_X[i]
  y <- gdam$MODEL_Y[i]
  dam.id <- id[x,y]
  
  if (is.na(downstream[x, y, 1]) || !(gdam$USE_IRR[i] == 1)) {
    next
  }
  print(i)
  
  cur <- c(x, y)
  nex <- downstream[x, y, ]
  while (TRUE) {
    frac <- discharge[x, y] / discharge [cur[1], cur[2]]
    
    # exclude fractions < 0.25
    if (frac < 0.1) {
      break
    }
    
    # Include cell
    global.service.map[cur[1],cur[2], i] = dam.id
    global.service.fraction.map[cur[1],cur[2], i] = frac
    
    if (cur[1] == nex[1] && cur[2] == nex[2]) {
      break
    }
    
    cur <- nex
    nex <- downstream[cur[1], cur[2], ]
  }
}
image.plot(global.service.map[,,1082])
image.plot(global.service.map[,,1103])
global.service.map.sum = apply(X = global.service.map, MARGIN = c(1,2), FUN = function(x){sum(!is.na(x))})
global.service.fraction.map.sum = apply(X = global.service.fraction.map, MARGIN = c(1,2), FUN = function(x){sum(x, na.rm = T)})
image.plot(global.service.map.sum)
image.plot(global.service.fraction.map.sum)

# Save
dir.create(dirname(gdam.service.out))
saveRDS(global.service.map, gdam.service.out)
dir.create(dirname(gdam.service.fraction.out))
saveRDS(global.service.fraction.map, gdam.service.fraction.out)
