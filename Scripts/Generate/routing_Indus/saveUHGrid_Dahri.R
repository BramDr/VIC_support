library(fields)
library(ncdf4)
rm(list = ls())

# Input
slope.file <- "../../../Data/Transformed/Routing/slope_5min_Indus.RDS"
area.file <- "../../../Data/Transformed/Routing/area_5min_Indus.RDS"
uh.file <- "../../../Data/Primary/SCS/dimensionless_unitHydrograph.csv"
distance.file <- "../../../Data/Transformed/Routing/distance_Dahri_5min_Indus.RDS"
uh.out <- "Saves/UH_grid_Dahri.RDS"

# Load
distance <- readRDS(distance.file)
slope <- readRDS(slope.file)
area <- readRDS(area.file)
uh <- read.table(uh.file, sep = ";")
colnames(uh) = c("t_tp", "q_qp", "-")

# Setup
max.days = 2
steps.per.day = 48
times <- cumsum(rep(60 * 60 * 24 / steps.per.day, steps.per.day * max.days))
times <- c(0, times)

# image.plot(distance < min(sqrt(area)))
# image.plot(slope < 1e-4)
slope[is.na(slope) | slope < 1e-4] = 1e-4 # limit minimum slope
distance[!is.na(distance) & distance < min(sqrt(area))] = min(sqrt(area)) # limit minimum distance (to straight flow)
image.plot(slope)
image.plot(distance)

interp <- function(x, x1, x2, y1, y2) {
  if (x < x1[1]) {
    return(c(x = 1, y = y1[1]))
  }
  if (x > x2[length(x2)]) {
    return(c(x = length(y2), y = y2[length(y2)]))
  }

  idx <- max(which(x1 - x <= 0))
  xdist <- x2[idx] - x1[idx]
  xfrac <- (x - x1[idx]) / xdist
  ydist <- y2[idx] - y1[idx]
  return(c(x = idx, y = y1[idx] + ydist * xfrac))
}

get.grid.tp <- function(distance, slope) {
  # Tc calculation based on: Kirpich, Z. P. (1940), Time of concentration of small agricultural watersheds
  # Review of calculation methods in Fang, X., Thompson, D. B., Cleveland, T. G., Pradhan, P., & Malla, R. (2008), Time of concentration estimated using watershed parameters determined by automated and manual methods
  tc <- 0.01947 * distance^0.770 / (slope^0.385) # Tc in seconds
  tp <- 0.6 * tc
  tp <- tp * 60
  return(tp)
}

# tp = tp.grid.map[x,y]
get.grid.uh <- function(tp) {
  uh.grid.temp <- uh
  uh.grid.temp$time <- uh.grid.temp$t_tp * tp

  uh.grid <- data.frame(Time = times)
  
  if(uh.grid.temp$time[2] > uh.grid$Time[2]){
    # Output time > input time, disaggregate by linear interpolation
    fraction.points = apply(
      X = uh.grid[, "Time", drop = F], MARGIN = 1, FUN = interp,
      x1 = uh.grid.temp[1:(nrow(uh.grid.temp) - 1), "time"],
      x2 = uh.grid.temp[2:nrow(uh.grid.temp), "time"],
      y1 = uh.grid.temp[1:(nrow(uh.grid.temp) - 1), "q_qp"],
      y2 = uh.grid.temp[2:nrow(uh.grid.temp), "q_qp"]
    )
    uh.grid$X = fraction.points[1,]
    uh.grid$Y = fraction.points[2,]
    uh.grid$Fraction = uh.grid$Y
  } 
  else {
    # Output time <= input time, aggregate by previous mean
    uh.grid$Fraction = 0
    for(i in 2:nrow(uh.grid)){
      sel = which(uh.grid.temp$time < uh.grid$Time[i] & uh.grid.temp$time > uh.grid$Time[i - 1])
      if(length(sel) == 0){
        break
      }
      uh.grid$Fraction[i] = mean(uh.grid.temp$q_qp[sel])
    }
  }

  uh.grid$Fraction <- uh.grid$Fraction / sum(uh.grid$Fraction)
  return(uh.grid[,c("Time", "Fraction")])
}

# Calculate
tp.grid.map <- array(NA, dim = c(dim(distance)[1], dim(distance)[2]))
for (x in 1:dim(tp.grid.map)[1]) {
  for (y in 1:dim(tp.grid.map)[2]) {
    if (is.na(distance[x, y])) {
      next
    }

    tp.grid <- get.grid.tp(distance[x, y], slope[x, y])
    tp.grid.map[x, y] <- tp.grid
  }
}
image.plot(tp.grid.map / 60 / 60 / 24 * 5, main = "total time [days]")
image.plot(tp.grid.map / 60 / 60 / 24 * 5 > 2)

uh.grid.map <- array(NA, dim = c(dim(distance)[1], dim(distance)[2], length(times)))
x = 1
y = 170
for (x in 1:dim(uh.grid.map)[1]) {
  print(x)
  for (y in 1:dim(uh.grid.map)[2]) {
    if (is.na(distance[x, y])) {
      next
    }
    
    uh.grid <- get.grid.uh(tp.grid.map[x,y])
    if(is.na(uh.grid$Fraction[1])){
      print(x)
      print(y)
    }
    uh.grid.map[x, y, ] <- uh.grid$Fraction
  }
}

calc.max.index <- function(x) {
  if (length(na.omit(x)) == 0) {
    return(NA)
  } else {
    return(which(x == max(x)))
  }
}
max.uh.index <- apply(uh.grid.map, MARGIN = c(1, 2), FUN = calc.max.index)
image.plot(max.uh.index)
image.plot(max.uh.index < 3)

# Save
dir.create(dirname(uh.out))
saveRDS(uh.grid.map, uh.out)
