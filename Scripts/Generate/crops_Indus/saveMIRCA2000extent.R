rm(list = ls())
library(fields)

# Input
scc.file <- "./Saves/subcropCalendar_5min_Indus.RDS"
scc.out <- "./Saves/subcropCalendar_extent_5min_Indus.RDS"
cc.out <- "./Saves/cropCalendar_extent_5min_Indus.RDS"
gc.out <- "./Saves/gridCalendar_extent_5min_Indus.RDS"

# Load
scc <- readRDS(scc.file)
scc$rowname <- 1:nrow(scc)

# Setup
get.area <- function(x, columns) {
  x <- as.numeric(x)
  print(x[columns == "rowname"])
  
  if (x[columns == "start"] < x[columns == "end"]) {
    period <- x[columns == "start"]:x[columns == "end"]
  } else {
    period <- c(x[columns == "start"]:12, 1:x[columns == "end"])
  }
  
  area <- rep(0, 12)
  area[period] <- x[columns == "area"]
  return(area)
}

sc.extent <- apply(X = scc, MARGIN = 1, FUN = get.area, columns = colnames(scc))
sc.extent = t(sc.extent)
colnames(sc.extent) = paste0("area.", 1:12)
sc.extent = cbind(scc[,c("Cell_ID", "crop", "subcrop")], sc.extent)
sc.extent$area.max = apply(X = sc.extent[, paste0("area.", 1:12)], MARGIN = 1, FUN = max)

c.exent <- aggregate(
  formula = cbind(area.1, area.2, area.3, area.4, area.5, area.6, area.7, area.8, area.9, area.10, area.11, area.12) ~ Cell_ID + crop,
  data = sc.extent, FUN = sum
)
c.exent$area.max <- apply(X = c.exent[, paste0("area.", 1:12)], MARGIN = 1, FUN = max)

g.extent <- aggregate(
  formula = cbind(area.1, area.2, area.3, area.4, area.5, area.6, area.7, area.8, area.9, area.10, area.11, area.12) ~ Cell_ID,
  data = sc.extent, FUN = sum
)
g.extent$area.max <- apply(X = g.extent[, paste0("area.", 1:12)], MARGIN = 1, FUN = max)

dir.create(dirname(scc.out))
saveRDS(sc.extent, scc.out)
dir.create(dirname(cc.out))
saveRDS(c.exent, cc.out)
dir.create(dirname(gc.out))
saveRDS(g.extent, gc.out)
