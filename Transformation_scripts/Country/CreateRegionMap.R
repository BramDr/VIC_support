library(fields)
library(ncdf4)
library(raster)
rm(list = ls())

# Input
iso.file = "Input/ISO3166_dev.csv"
mask.file = "Input/domain_global.nc"
country.file = "Output/country_6min_global.RDS"
region.out = "Output/region_global.RDS"

# Load
iso = read.csv(iso.file, stringsAsFactors = F)
country = readRDS(country.file)

nc = nc_open(mask.file)
mask = ncvar_get(nc, "mask")
nc_close(nc)

# Setup
getMost = function(x, na.rm = T){
  if(na.rm){
    x = na.omit(x)
  }
  
  val = table(x)
  if(length(val) == 0){
    return(NA)
  }
  
  val = names(val)[order(val, decreasing = T)[1]]
  return(as.numeric(val))
}

getNearest = function(x,y,data){
  for(dis in 1:1000){
    x.min = max(x - dis, 1)
    y.min = max(y - dis, 1)
    x.max = min(x + dis, dim(data)[1])
    y.max = min(y + dis, dim(data)[2])
    
    val = c(data[x.min:x.max, y.min:y.max], na.rm = T)
    if(length(val) == 0){
      next
    }
    
    val = table(val)
    val = names(val)[order(val, decreasing = T)[1]]
    val = as.numeric(val)
    
    if(!is.na(val)){
      return(mean(val))
    }
  }
  
  warning("Nearest out of iterations")
  return(NA)
}

# Calculate
region = country
for(x in 1:dim(region)[1]){
  for(y in 1:dim(region)[2]){
    if(is.na(country[x,y])){
      next
    }
    
    row = which(iso$Country_number == country[x,y])
    region[x,y] = iso$Region_number[row]
  }
}
image.plot(region)

region.r = raster(region)
extent(region.r) = c(-180,180,-90,90)
region.agg.r = aggregate(region.r, fact = 5, fun = getMost)
region.agg = as.matrix(region.agg.r)
image.plot(region.agg)

for(x in 1:dim(mask)[1]){
  for(y in 1:dim(mask)[2]){
    if(is.na(mask[x,y])){
      region.agg[x,y] = NA
      next
    }
    
    if(is.na(region.agg[x,y])){
      region.agg[x,y] = getNearest(x,y,region.agg)
    }
  }
}
image.plot(region.agg)

# Save
saveRDS(region.agg, region.out)
