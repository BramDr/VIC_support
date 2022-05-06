library(fields)
library(raster)
rm(list = ls())

depth.file <- "../../../../Data/Primary/Pelletier2016/average_soil_and_sedimentary-deposit_thickness.tif"
depth.out <- "../../../../Data/Transformed/Soil/Pelletier2016/depth_30min_global.RDS"

# Load
depth <- raster(depth.file)
NAvalue(depth) <- 255
plot(depth)

# Calculate
# depth.m = as.matrix(depth)
# sum.agg = array(0, dim = c(360, 720))
# count.agg = array(0, dim = c(360, 720))
# for(x in 1:dim(depth.m)[1]){
#   for(y in 1:dim(depth.m)[2]){
#     if(is.na(depth.m[x,y])){
#       next
#     }
#     x.agg = (x - 1) %% 60 + 1 + 60 # add offset to account for missing values
#     y.agg = (y - 1) %% 60 + 1
#     sum.agg[x.agg, y.agg] = sum.agg[x.agg, y.agg] + depth.m[x,y]
#     count.agg[x.agg, y.agg] = count.agg[x.agg, y.agg] + 1
#   }
# }
# depth.agg = sum.agg / count.agg
# depth.agg[count.agg == 0] = NA

depth.agg = aggregate(x = depth, fact = 60, FUN = mean, na.rm = T)
depth.agg <- extend(depth.agg, extent(-180, 180, -90, 90))
depth.agg = as.matrix(depth.agg)

depth.agg = t(depth.agg[nrow(depth.agg):1,])
image.plot(depth.agg)

# Save
dir.create(dirname(depth.out))
saveRDS(depth.agg, depth.out)
