library(fields)
library(raster)
rm(list = ls())

# Input
country.file = "Input/UN_map_6min_nodata.tif"
iso.file = "Input/ISO3166.csv"
subregion.out = "Saves/subregion_map_6min.RDS"

# Load
iso = read.csv(iso.file)

country = raster(country.file)
plot(country)

# Setup
country.m = as.matrix(country)
country.m = t(country.m[nrow(country.m):1,])
image.plot(country.m)

# Calculate
subregion = array(NA, dim = dim(country.m))
for(x in 1:dim(subregion)[1]){
  for(y in 1:dim(subregion)[2]){
    if(is.na(country.m[x,y])){
      next
    }
    
    row = which(iso$UN == country.m[x,y])
    subregion[x,y] = iso$SUBREGION[row]
  }
}
image.plot(subregion)

#save
saveRDS(subregion, subregion.out)
