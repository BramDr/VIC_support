library(raster)
library(fields)
rm(list = ls())

livestock.dir <- "../../../Data/Primary/FAO/GLW/"
livestock.out <- "../../../Data/Transformed/Livestock/livestockCount_30min_global.csv"

# Load
livestock.files <- list.files(livestock.dir, pattern = ".tif", full.names = T)
print(livestock.files)

# Setup
lats <- seq(-89.75, 89.75, by = 0.5)
lons <- seq(-179.75, 179.75, by = 0.5)

get.number <- function(x, ct, bf, dk, ch, gt, sh, ho, pg) {
  lon <- x[1]
  lat <- x[2]
  rowname <- x[3]

  print(as.numeric(rowname))

  ret <- rep(0, 8)

  sel <- ct$lon == lon & ct$lat == lat
  if (sum(sel) > 0) {
    ret[1] <- ct$animals[sel]
  }

  sel <- bf$lon == lon & bf$lat == lat
  if (sum(sel) > 0) {
    ret[2] <- bf$animals[sel]
  }

  sel <- dk$lon == lon & dk$lat == lat
  if (sum(sel) > 0) {
    ret[3] <- dk$animals[sel]
  }

  sel <- ch$lon == lon & ch$lat == lat
  if (sum(sel) > 0) {
    ret[4] <- ch$animals[sel]
  }

  sel <- gt$lon == lon & gt$lat == lat
  if (sum(sel) > 0) {
    ret[5] <- gt$animals[sel]
  }

  sel <- sh$lon == lon & sh$lat == lat
  if (sum(sel) > 0) {
    ret[6] <- sh$animals[sel]
  }

  sel <- ho$lon == lon & ho$lat == lat
  if (sum(sel) > 0) {
    ret[7] <- ho$animals[sel]
  }

  sel <- pg$lon == lon & pg$lat == lat
  if (sum(sel) > 0) {
    ret[8] <- pg$animals[sel]
  }

  return(ret)
}

livestock.ct <- raster(grep(x = livestock.files, pattern = "Ct_.*_Da", value = T))
livestock.bf <- raster(grep(x = livestock.files, pattern = "Bf_.*_Da", value = T))
livestock.dk <- raster(grep(x = livestock.files, pattern = "Dk_.*_Da", value = T))
livestock.ch <- raster(grep(x = livestock.files, pattern = "Ch_.*_Da", value = T))
livestock.gt <- raster(grep(x = livestock.files, pattern = "Gt_.*_Da", value = T))
livestock.sh <- raster(grep(x = livestock.files, pattern = "Sh_.*_Da", value = T))
livestock.ho <- raster(grep(x = livestock.files, pattern = "Ho_.*_Da", value = T))
livestock.pg <- raster(grep(x = livestock.files, pattern = "Pg_.*_Da", value = T))

livestock.ct <- data.frame(rasterToPoints(aggregate(x = livestock.ct, fact = 6, fun = sum)))
livestock.bf <- data.frame(rasterToPoints(aggregate(x = livestock.bf, fact = 6, fun = sum)))
livestock.dk <- data.frame(rasterToPoints(aggregate(x = livestock.dk, fact = 6, fun = sum)))
livestock.ch <- data.frame(rasterToPoints(aggregate(x = livestock.ch, fact = 6, fun = sum)))
livestock.gt <- data.frame(rasterToPoints(aggregate(x = livestock.gt, fact = 6, fun = sum)))
livestock.sh <- data.frame(rasterToPoints(aggregate(x = livestock.sh, fact = 6, fun = sum)))
livestock.ho <- data.frame(rasterToPoints(aggregate(x = livestock.ho, fact = 6, fun = sum)))
livestock.pg <- data.frame(rasterToPoints(aggregate(x = livestock.pg, fact = 6, fun = sum)))

colnames(livestock.ct) <- c("lon", "lat", "animals")
colnames(livestock.bf) <- c("lon", "lat", "animals")
colnames(livestock.dk) <- c("lon", "lat", "animals")
colnames(livestock.ch) <- c("lon", "lat", "animals")
colnames(livestock.gt) <- c("lon", "lat", "animals")
colnames(livestock.sh) <- c("lon", "lat", "animals")
colnames(livestock.ho) <- c("lon", "lat", "animals")
colnames(livestock.pg) <- c("lon", "lat", "animals")

# Calculate
livestock <- data.frame(lon = rep(lons, length(lats)), lat = rep(lats, each = length(lons)))
livestock$rownames <- 1:nrow(livestock)

numbers <- apply(
  X = livestock, MARGIN = c(1), FUN = get.number,
  ct = livestock.ct,
  bf = livestock.bf,
  dk = livestock.dk,
  ch = livestock.ch,
  gt = livestock.gt,
  sh = livestock.sh,
  ho = livestock.ho,
  pg = livestock.pg
)

livestock <- cbind(livestock, t(numbers))
colnames(livestock)[(ncol(livestock) - nrow(numbers) + 1):ncol(livestock)] <- c("ct", "bf", "dk", "ch", "gt", "sh", "ho", "pg")

# Save
dir.create(dirname(livestock.out))
write.csv(livestock, livestock.out, row.names = T)
