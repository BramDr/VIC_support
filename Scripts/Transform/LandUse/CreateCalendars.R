rm(list = ls())

# Input
scc.file = "../../../Data/Primary/MIRCA2000/Growing periods listed/cropping_calendars_30min.txt"
scc.out = "../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
cc.out = "../../../Data/Transformed/LandUse/cropCalendar_30min_global.csv"
ccc.out = "../../../Data/Transformed/LandUse/cellCalendar_30min_global.csv"

# Load
scc = read.table(scc.file, stringsAsFactors = F, header = T)
scc$rowname = 1:nrow(scc)

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

monthly.area = apply(X = scc, MARGIN = 1, FUN = get.area, columns = colnames(scc))
scc = cbind(scc, t(monthly.area))
new.colnames <- colnames(scc)[1:(ncol(scc) - 12)]
new.colnames <- c(new.colnames, paste0("area.", 1:12))
colnames(scc) <- new.colnames
scc$maxarea <- apply(X = scc[, paste0("area.", 1:12)], MARGIN = 1, FUN = max)

cc <- aggregate(
  formula = cbind(area.1, area.2, area.3, area.4, area.5, area.6, area.7, area.8, area.9, area.10, area.11, area.12) ~ cell_ID + row + column + crop + lat + lon,
  data = scc, FUN = sum
)
cc$maxarea <- apply(X = cc[, paste0("area.", 1:12)], MARGIN = 1, FUN = max)

ccc <- aggregate(
  formula = cbind(area.1, area.2, area.3, area.4, area.5, area.6, area.7, area.8, area.9, area.10, area.11, area.12) ~ cell_ID + row + column + lat + lon,
  data = cc, FUN = sum
)
ccc$maxarea <- apply(X = ccc[, paste0("area.", 1:12)], MARGIN = 1, FUN = max)

# Save
dir.create(dirname(scc.out))
write.csv(scc, scc.out, row.names = F)
dir.create(dirname(cc.out))
write.csv(cc, cc.out, row.names = F)
dir.create(dirname(ccc.out))
write.csv(ccc, ccc.out, row.names = F)

