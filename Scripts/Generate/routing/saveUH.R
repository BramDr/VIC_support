library(fields)
library(ncdf4)

rm(list = ls())

# Input
distance.file <- "../../../Data/Transformed/Routing/distance_30min_global.RDS"
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
uh.runoff.file <- "../../../Data/Primary/Lohmann1996/UH_runoff.txt"
uh.inflow.out <- "Saves/UH_inflow.RDS"
uh.runoff.out <- "Saves/UH_runoff.RDS"

# Load
distance <- readRDS(distance.file)

nc <- nc_open(filename = mask.file)
mask <- ncvar_get(nc = nc, "mask")
nc_close(nc = nc)
image.plot(mask)

uh.runoff <- read.table(file = uh.runoff.file, header = T, sep = "\t")

# Setup
velocity <- 1
diffusion <- 2000
max.steps <- 48

get.inflow.uh <- function(velocity, diffusion, distance, time) {
  uh <- rep(0, length(time))

  exponent <- -1 * ((velocity * time - distance)^2) / (4 * diffusion * time)
  green <- distance / (2 * time * sqrt(pi * time * distance)) * exp(exponent)
  uh <- green / sum(green)

  return(data.frame(Time = time - time[1], Fraction = uh))
}

# Calculate
uh.runoff$Fraction <- uh.runoff$Fraction / sum(uh.runoff$Fraction)
plot(uh.runoff$Time, uh.runoff$Fraction, type = "l")

uh.inflow.map <- array(NA, dim = c(dim(distance)[1], dim(distance)[2], max.steps))
uh.runoff.map <- array(NA, dim = c(dim(distance)[1], dim(distance)[2], length(uh.runoff$Time)))
time <- (1:max.steps) * 3600
for (x in 1:dim(uh.inflow.map)[1]) {
  for (y in 1:dim(uh.inflow.map)[2]) {
    if (is.na(distance[x, y])) {
      next
    }

    uh.inflow <- get.inflow.uh(velocity, diffusion, distance[x, y], time)
    # plot(uh.inflow$Time, uh.inflow$Fraction, type = "l")

    uh.inflow.map[x, y, ] <- uh.inflow$Fraction
    uh.runoff.map[x, y, ] <- uh.runoff$Fraction
  }
}

# Save
dir.create(dirname(uh.inflow.out))
saveRDS(uh.inflow.map, uh.inflow.out)
dir.create(dirname(uh.runoff.out))
saveRDS(uh.runoff.map, uh.runoff.out)
