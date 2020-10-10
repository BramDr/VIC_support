library(ncdf4)
library(fields)
rm(list = ls())

# Input
delta.receiving.file <- "Saves/receivingDelta.RDS"
neighbour.receiving.file <- "Saves/receivingNeighbour.RDS"
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
receiving.id.out <- "Saves/receiving_id.RDS"
nreceiving.out <- "Saves/Nreceiving.RDS"
receiving.out <- "Saves/receiving.RDS"

# Load
nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

delta.receiving = readRDS(delta.receiving.file)
neighbour.receiving = readRDS(neighbour.receiving.file)

# Calculate
## receiving
total.receiving = vector(mode = "list", length = 720)
for(x in 1:dim(mask)[1]){
  total.receiving[[x]] = vector(mode = "list", length = 360)
}
for (x in 1:dim(mask)[1]) {
  for (y in 1:dim(mask)[2]) {
    if (length(names(neighbour.receiving[[x]][[y]])) == 0 &&
        length(names(delta.receiving[[x]][[y]])) == 0) {
      next
    }
    
    df = data.frame(x = numeric(), y = numeric())
    
    if (length(names(neighbour.receiving[[x]][[y]])) != 0) {
      df = rbind(df, neighbour.receiving[[x]][[y]]$df)
    }
    
    if (length(names(delta.receiving[[x]][[y]])) != 0) {
      df = rbind(df, delta.receiving[[x]][[y]]$df)
    }
    
    df = unique(df)
    
    total.receiving[[x]][[y]]$df <- df
  }
}

## Nreceiving
Nreceiving <- mask * 0
for (x in 1:dim(mask)[1]) {
  for (y in 1:dim(mask)[2]) {
    if (length(names(total.receiving[[x]][[y]])) == 0) {
      next
    }
    
    df = total.receiving[[x]][[y]]$df
    
    Nreceiving[x, y] <- Nreceiving[x, y] + nrow(df)
  }
}
image.plot(Nreceiving)

## Give cells a receiving id
receiving.id <- mask * 0
id.counter <- 1
for (x in 1:dim(receiving.id)[1]) {
  for (y in 1:dim(receiving.id)[2]) {
    if (is.na(mask[x, y])) {
      next
    }
    
    receiving.id[x, y] <- id.counter
    id.counter <- id.counter + 1
  }
}
image.plot(receiving.id)

## Make receiving
receiving <- array(NA, dim = c(dim(mask), max(Nreceiving, na.rm = T)))
for (x in 1:dim(mask)[1]) {
  for (y in 1:dim(mask)[2]) {
    if (length(names(total.receiving[[x]][[y]])) == 0) {
      next
    }
    
    df = total.receiving[[x]][[y]]$df
    
    for(i in 1:nrow(df)){
      receiving[x, y, i] <- receiving.id[df$x[i], df$y[i]]
    }
  }
}
image.plot(receiving[, , 5])

# Save
dir.create(dirname(receiving.out))
saveRDS(receiving, receiving.out)
dir.create(dirname(receiving.id.out))
saveRDS(receiving.id, receiving.id.out)
dir.create(dirname(nreceiving.out))
saveRDS(Nreceiving, nreceiving.out)
