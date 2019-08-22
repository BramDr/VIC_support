library(ncdf4)
library(fields)
rm(list = ls())

# Input
mask.file = "Input/domain_global.nc"
direction.file = "Input/RVIC_input_global.nc"
downstream.out = "Saves/downstream.RDS"

# Load
nc = nc_open(filename = direction.file)
direction = ncvar_get(nc = nc, "flow_direction")
nc_close(nc = nc)
image.plot(direction)

nc = nc_open(mask.file)
mask = ncvar_get(nc, "mask")
nc_close(nc)
image.plot(mask)

# Setup
direction.to.index = function(direction){
  if(direction < 1 || direction > 9){
    warning("Direction (" + direction + ") is outside of range [1-9], defaulting to 9 [outlet]")
    direction = 9
  }
  
  index = switch(direction,
                 c(0,1),   # north
                 c(1,1),   # north east
                 c(1,0),   # east
                 c(1,-1),  # south east
                 c(0,-1),  # south
                 c(-1,-1), # south west
                 c(-1,0),  # west
                 c(-1,1),  # north west
                 c(0,0))   # outlet
  
  return(index)
}

# Calculate
downstream = array(NA, dim = c(dim(direction), 2))
for(x in 1:dim(direction)[1]){
  for(y in 1:dim(direction)[2]){
    
    if(is.na(mask[x,y])){
      next
    }
    
    if(is.na(direction[x,y])){
      direction[x,y] = 9
    }
    
    downstream[x,y,] = c(x,y) + direction.to.index(direction[x,y])
    
    if(is.na(mask[downstream[x,y,1], downstream[x,y,2]])){
      downstream[x,y,] = c(x,y)
    }
  }
}
image.plot(downstream[,,1])
image.plot(downstream[,,2])

# Save
saveRDS(downstream, downstream.out)
