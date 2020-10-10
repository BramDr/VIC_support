library(fields)
library(raster)
rm(list = ls())

# Input
count.file <- "../../../Data/Transformed/Routing/count_30min_global.RDS"
basin.file <- "../../../Data/Transformed/Routing/basins_30min_global.RDS"
delta.file <- "../../../Data/Transformed/Delta/delta_30min_global.RDS"
basin.out <- "../../../Data/Transformed/Delta/deltaBasins_30min_global.RDS"
delta.out <- "../../../Data/Transformed/Delta/deltaAdj_30min_global.RDS"

# Load
count <- readRDS(count.file)
basin <- readRDS(basin.file)
delta <- readRDS(delta.file)

# Calculate
## Get the basins associated with the delta
delta.basin <- data.frame(delta = numeric(), basin = numeric())
for (x in 1:dim(delta)[1]) {
  for (y in 1:dim(delta)[2]) {
    if (is.na(delta[x, y])) {
      next
    }
    
    delta.basin[nrow(delta.basin) + 1,] = c(delta[x,y], basin[x,y])
  }
}
delta.basin = unique(delta.basin)

## Get the basin outflow points associated with the deltas
delta.basin$count = 0
delta.basin$x = NA
delta.basin$y = NA
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

## Make new deltas for large river basins
get.distance = function(xfrom, yfrom, xto, yto){
  return(sqrt((xfrom - xto) ^ 2 + (yfrom - yto)^2))
}

delta.basin.adj = delta.basin
delta.basin.adj$olddelta = delta.basin$delta
for(delta.id in unique(delta.basin$delta)){
  rows <- which(delta.basin$delta == delta.id)
  if(length(rows) <=1){
    next
  }
  delta.basin.sel = delta.basin[rows,]
  
  rows = which(delta.basin.sel$count > 15)
  if(length(rows) <=1){
    next
  }
  delta.basin.major = delta.basin.sel[rows,]
  
  rows = which(delta.basin.sel$count <= 15)
  delta.basin.minor = delta.basin.sel[rows,]
  
  j = 1
  for(j in 2:nrow(delta.basin.major)){
    new.delta.id = max(delta.basin.adj$delta) + 1
    
    row = which(delta.basin$delta == delta.basin.major$delta[j] &
                  delta.basin$basin == delta.basin.major$basin[j] &
                  delta.basin$count == delta.basin.major$count[j] &
                  delta.basin$x == delta.basin.major$x[j] &
                  delta.basin$y == delta.basin.major$y[j])
    delta.basin.adj$delta[row] = new.delta.id
    
    for(k in 1:nrow(delta.basin.minor)){
      new.dist = get.distance(delta.basin.major$x[j], delta.basin.major$y[j],
                              delta.basin.minor$x[k], delta.basin.minor$y[k])
      orig.dist = get.distance(delta.basin.major$x[1], delta.basin.major$y[1],
                              delta.basin.minor$x[k], delta.basin.minor$y[k])
      if(new.dist < orig.dist - 0.5){
        row = which(delta.basin$delta == delta.basin.minor$delta[k] &
                      delta.basin$basin == delta.basin.minor$basin[k] &
                      delta.basin$count == delta.basin.minor$count[k] &
                      delta.basin$x == delta.basin.minor$x[k] &
                      delta.basin$y == delta.basin.minor$y[k])
        delta.basin.adj$delta[row] = new.delta.id
      }
    }
  }
}

## Adjust delta map
delta.adj <- delta
for (x in 1:dim(delta)[1]) {
  for (y in 1:dim(delta)[2]) {
    if (is.na(delta[x, y])) {
      next
    }
    
    rows <- which(delta.basin.adj$olddelta == delta[x, y])
    if(length(rows) <= 1){
      next
    }
    
    for(row in rows){
      delta.sel = delta == delta.basin.adj$olddelta[row] & basin == delta.basin.adj$basin[row]
      delta.adj[delta.sel] = delta.basin.adj$delta[row]
    }
  }
}
image.plot(delta.adj)
image.plot(delta != delta.adj)

## Combine delta upstream area with basin upstream area
basin.adj <- basin
for (x in 1:dim(delta.adj)[1]) {
  for (y in 1:dim(delta.adj)[2]) {
    if (is.na(delta.adj[x, y])) {
      next
    }
    
    rows <- which(delta.basin.adj$delta == delta.adj[x, y])
    delta.basin.sel = delta.basin.adj[rows,]
    row <- which.max(delta.basin.sel$count)
    
    for(i in 1:nrow(delta.basin.sel)){
      basin.adj[!is.na(basin) & basin == delta.basin.sel$basin[i]] <- delta.basin.sel$basin[row]
    }
  }
}
image.plot(basin.adj)
image.plot(basin != basin.adj)

dir.create(dirname(basin.out))
saveRDS(object = basin.adj, file = basin.out)
dir.create(dirname(delta.out))
saveRDS(object = delta.adj, file = delta.out)

