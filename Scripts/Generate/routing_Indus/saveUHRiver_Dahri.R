library(fields)
library(ncdf4)

rm(list = ls())

# Input
area.file <- "../../../Data/Transformed/Routing/area_5min_Indus.RDS"
distance.file <- "../../../Data/Transformed/Routing/distance_Dahri_5min_Indus.RDS"
uh.out <- "Saves/UH_river_Dahri.RDS"

# Load
distance <- readRDS(distance.file)
area <- readRDS(area.file)

# Setup
max.days = 2
steps.per.day = 48
velocity <- 1
diffusion <- 2000
times <- cumsum(rep(60 * 60 * 24 / steps.per.day, steps.per.day * max.days))
times <- c(0, times)

# image.plot(distance < min(sqrt(area)))
distance[!is.na(distance) & distance < min(sqrt(area))] = min(sqrt(area)) # limit minimum distance (to straight flow)
image.plot(distance)

get.river.uh <- function(velocity, diffusion, distance, time) {
  uh <- rep(0, length(time))

  calc.h <- function(time, velocity, diffusion, distance) {
    pot <- ((velocity * time - distance)^2) / (4 * diffusion * time)

    if (pot <= 69) {
      h <- 1 / (2 * sqrt(pi * diffusion)) * distance / (time^1.5) * exp(-pot)
    } else {
      h <- 0
    }
    return(h)
  }

  h <- apply(array(time, dim = c(length(time))), MARGIN = 1, FUN = calc.h, velocity = velocity, diffusion = diffusion, distance = distance)

  uh <- h / sum(h)

  return(data.frame(Time = time - time[1], Fraction = uh))
}

# Calculate
uh.river.map <- array(NA, dim = c(dim(distance)[1], dim(distance)[2], length(times)))
for (x in 1:dim(uh.river.map)[1]) {
  print(x)
  for (y in 1:dim(uh.river.map)[2]) {
    if (is.na(distance[x, y])) {
      next
    }

    uh.river <- get.river.uh(velocity, diffusion, distance[x, y], times)
    # plot(uh.river$Time, uh.river$Fraction, type = "l")

    uh.river.map[x, y, ] <- uh.river$Fraction / sum(uh.river$Fraction)
  }
}
plot(uh.river)

# Save
dir.create(dirname(uh.out))
saveRDS(uh.river.map, uh.out)
