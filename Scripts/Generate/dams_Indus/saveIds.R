library(fields)
rm(list = ls())

# Input
downstream.file <- "../../../Data/Transformed/Routing/downstream_5min_Indus.RDS"
id.out <- "Saves/idMap.RDS"

# Load
downstream = readRDS(downstream.file)

# Calculate
id.map = array(NA, dim = dim(downstream)[1:2])
id = 1
for (x in 1:dim(downstream)[1]) {
  for (y in 1:dim(downstream)[2]) {
    if(is.na(downstream[x,y,1])){
      next
    }
    
    id.map[x,y] = id
    id = id + 1
  }
}
image.plot(id.map)

# Save
dir.create(dirname(id.out))
saveRDS(id.map, id.out)

