library(fields)
library(ncdf4)

rm(list = ls())

# Input
distance.file = "Input/RVIC_input_global.nc"
mask.file = "Input/domain_global.nc"
uh.runoff.file = "Input/UH_runoff.txt"
uh.inflow.out = "Saves/UH_inflow.RDS"
uh.runoff.out = "Saves/UH_runoff.RDS"

# Load
nc = nc_open(filename = distance.file)
distance = ncvar_get(nc = nc, varid = "flow_distance")
nc_close(nc = nc)
image.plot(distance)

nc = nc_open(filename = mask.file)
mask = ncvar_get(nc = nc, "mask")
nc_close(nc = nc)
image.plot(mask)

uh.runoff = read.table(file = uh.runoff.file, header = T, sep = "\t")

# Setup
velocity = 1
diffusion = 2000
max.steps = 48

get.inflow.uh = function(velocity, diffusion, distance, time){
  uh = rep(0, length(time))
  
  exponent = -1 * ((velocity * time - distance) ^ 2) / (4 * diffusion * time)
  green = distance / (2 * time * sqrt(pi * time * distance)) * exp(exponent)
  uh = green / sum(green)
  
  return(data.frame(Time = time - time[1], Fraction = uh))
}

## If distance is missing, take mean
for(x in 1:dim(mask)[1]){
  for(y in 1:dim(mask)[2]){
    if(is.na(mask[x,y])){
      distance[x,y] = NA
    } else if (is.na(distance[x,y])){
      distance[x,y] = mean(distance, na.rm = T)
    }
  }
}
image.plot(distance)

# Calculate
uh.runoff$Fraction = uh.runoff$Fraction / sum(uh.runoff$Fraction)
plot(uh.runoff$Time, uh.runoff$Fraction, type = "l")

uh.inflow.map = array(NA, dim = c(dim(distance)[1], dim(distance)[2], max.steps))
uh.runoff.map = array(NA, dim = c(dim(distance)[1], dim(distance)[2], length(uh.runoff$Time)))
time = (1:max.steps) * 3600
for(x in 1:dim(uh.inflow.map)[1]){
  for(y in 1:dim(uh.inflow.map)[2]){
    if(is.na(distance[x,y])){
      next
    }
    
    uh.inflow = get.inflow.uh(velocity, diffusion, distance[x,y], time)
    #plot(uh.inflow$Time, uh.inflow$Fraction, type = "l")
    
    uh.inflow.map[x,y,] = uh.inflow$Fraction
    uh.runoff.map[x,y,] = uh.runoff$Fraction
  }
}

# Save
saveRDS(uh.inflow.map, uh.inflow.out)
saveRDS(uh.runoff.map, uh.runoff.out)
