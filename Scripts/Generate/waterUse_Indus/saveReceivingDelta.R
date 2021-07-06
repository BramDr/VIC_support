library(ncdf4)
library(fields)
rm(list = ls())

# Input
count.file <- "../../../Data/Transformed/Routing/count_5min_Indus.RDS"
basin.file <- "../../../Data/Transformed/Delta/deltaBasins_5min_Indus.RDS"
delta.file <- "../../../Data/Transformed/Delta/deltaAdj_5min_Indus.RDS"
downstream.file <- "../../../Data/Transformed/Routing/downstream_5min_Indus.RDS"
id.file = "Saves/idMap.RDS"
receiving.out <- "Saves/receivingDelta.RDS"

# Load
count <- readRDS(count.file)
basin <- readRDS(basin.file)
delta <- readRDS(delta.file)
id = readRDS(id.file)
downstream <- readRDS(downstream.file)

# Setup
check.in.stream <- function(downstream, xfrom, yfrom, xto, yto) {
  cur <- c(xfrom, yfrom)
  nex <- downstream[cur[1], cur[2], ]
  
  if(xfrom == xto && yfrom == yto){
    return(T)
  }
  
  while (TRUE) {
    if (cur[1] == xto && cur[2] == yto) {
      return(T)
    }
    if (cur[1] == nex[1] && cur[2] == nex[2]) {
      return(F)
    }

    cur <- nex
    nex <- downstream[cur[1], cur[2], ]
  }
  return(F)
}

# Calculate

## Get the basin associated with the delta
delta.basin <- data.frame(delta = numeric(), basin = numeric())
for (x in 1:dim(delta)[1]) {
  for (y in 1:dim(delta)[2]) {
    if (is.na(delta[x, y])) {
      next
    }

    delta.basin[nrow(delta.basin) + 1, ] <- c(delta[x, y], basin[x, y])
  }
}
delta.basin <- unique(delta.basin)

## Get the basin outflow points associated with the deltas
delta.basin$count <- 0
delta.basin$x <- NA
delta.basin$y <- NA
for (x in 1:dim(basin)[1]) {
  for (y in 1:dim(basin)[2]) {
    if (is.na(basin[x, y])) {
      next
    }
    if (!basin[x, y] %in% delta.basin$basin) {
      next
    }

    row <- which(delta.basin$basin == basin[x, y])
    if (count[x, y] > delta.basin$count[row]) {
      delta.basin$count[row] <- count[x, y]
      delta.basin$x[row] <- x
      delta.basin$y[row] <- y
    }
  }
}

## Get delta cells
delta.adj = delta
for(i in 1:nrow(delta.basin)){
  x = delta.basin$x[i]
  y = delta.basin$y[i]
  delta.id = delta.basin$delta[i]
  delta.adj[!is.na(delta) & delta == delta.id] = id[x,y]
}
for (x in 1:dim(delta.adj)[1]) {
  for (y in 1:dim(delta.adj)[2]) {
    if(is.na(delta.adj[x,y])){
      next
    }
    
    out.id = delta.adj[x,y]
    out.x = which(apply(X = id == out.id, MARGIN = 1, FUN = sum, na.rm = T) == 1)
    out.y = which(apply(X = id == out.id, MARGIN = 2, FUN = sum, na.rm = T) == 1)
    if(check.in.stream(downstream, x, y, out.x, out.y)){
      delta.adj[x,y] = NA
    }
  }
}
image.plot(delta.adj)

## Nreceiving
Nreceiving <- array(0, dim = dim(delta.adj)[1:2])
for(i in 1:nrow(delta.basin)){
  x = delta.basin$x[i]
  y = delta.basin$y[i]
  delta.id = delta.basin$delta[i]
  Nreceiving[x,y] = sum(!is.na(delta.adj) & delta.adj == id[x,y])
}
image.plot(Nreceiving)

# Save
dir.create(dirname(receiving.out))
saveRDS(delta.adj, receiving.out)
