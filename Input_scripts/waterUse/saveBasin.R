library(fields)
rm(list = ls())

# Input
downstream.file = "Saves/downstream.RDS"
basin.out = "Saves/basin.RDS"

# Load
downstream = readRDS(downstream.file)

# Calculate
basin = array(NA, dim = dim(downstream)[1:2])
id.counter = 0
for(x in 1:dim(downstream)[1]){
  for(y in 1:dim(downstream)[2]){
    if(is.na(downstream[x,y,1]) || !is.na(basin[x,y])){
      next
    }
    
    cur = c(x,y)
    nex = downstream[x,y,]
    river = list()
    
    while(TRUE){
      river[[length(river) + 1]] = cur
      
      if(!is.na(basin[cur[1], cur[2]])){
        id = basin[cur[1], cur[2]]
        break
      }
      if(cur[1] == nex[1] && cur[2] == nex[2]){
        id = id.counter
        id.counter = id.counter + 1
        break
      }
      
      cur = nex
      nex = downstream[cur[1],cur[2],]
    }
    
    for(i in 1:length(river)){
      r.x = river[[i]][1]
      r.y = river[[i]][2]
      basin[r.x, r.y] = id
    }
  }
}
image.plot(basin)

# Save
saveRDS(basin, basin.out)
