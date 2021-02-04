library(fields)
rm(list = ls())

# Input
basin2.file <- "../../../Data/Transformed/Routing/basins_Dahri_5min_Indus.RDS"
downstream.file <- "../../../Data/Transformed/Routing/downstream_5min_Indus.RDS"

# Load
basin2 = readRDS(basin2.file)
downstream <- readRDS(downstream.file)

# Setup
for(z in 1:dim(downstream)[3]){
  downstream.tmp = downstream[,,z]
  downstream.tmp[is.na(basin2)] = NA
  downstream[,,z] = downstream.tmp
}

# Calculate
basin <- array(NA, dim = dim(downstream)[1:2])
id.counter <- 0
x = 122
y = 144
for (x in 1:dim(downstream)[1]) {
  for (y in 1:dim(downstream)[2]) {
    if (is.na(downstream[x, y, 1])) {
      next
    }

    cur <- c(x, y)
    nex <- downstream[x, y, ]
    river <- list()

    while (TRUE) {
      river[[length(river) + 1]] <- cur

      if (!is.na(basin[cur[1], cur[2]])) {
        id <- basin[cur[1], cur[2]]
        break
      }
      if (is.na(nex[1]) || is.na(nex[2])) {
        id <- id.counter
        id.counter <- id.counter + 1
        break
      }
      if (cur[1] == nex[1] && cur[2] == nex[2]) {
        id <- id.counter
        id.counter <- id.counter + 1
        break
      }

      cur <- nex
      nex <- downstream[cur[1], cur[2], ]
    }
    
    if(x == 122 && y == 144){
      print(river)
      print(basin[x,y])
    }
    
    for (i in 1:length(river)) {
      r.x <- river[[i]][1]
      r.y <- river[[i]][2]
      basin[r.x, r.y] <- id
    }
  }
}
image.plot(basin, zlim = c(0,50))
image.plot(basin2)
