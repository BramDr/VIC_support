rm(list = ls())
library(fields)
library(raster)

sand.file = "./Saves/sand_5min_Indus.RDS"
clay.file = "./Saves/clay_5min_Indus.RDS"
silt.file = "./Saves/silt_5min_Indus.RDS"
usda.file = "./Saves/usda_5min_Indus.RDS"
bulk.file = "./Saves/bulk_5min_Indus.RDS"
carbon.file = "./Saves/carbon_5min_Indus.RDS"
ph.file = "./Saves/ph_5min_Indus.RDS"
bulk.out = "/home/bram/Data/Transformed/Soil/HWSD/bulk_5min_Indus.RDS"
carbon.out = "/home/bram/Data/Transformed/Soil/HWSD/carbon_5min_Indus.RDS"
ph.out = "/home/bram/Data/Transformed/Soil/HWSD/ph_5min_Indus.RDS"
quartz.out = "/home/bram/Data/Transformed/Soil/HWSD/quartz_5min_Indus.RDS"
expt.out = "/home/bram/Data/Transformed/Soil/HWSD/expt_5min_Indus.RDS"
bubble.out = "/home/bram/Data/Transformed/Soil/HWSD/bubble_5min_Indus.RDS"

# Load
sand = readRDS(sand.file)
clay = readRDS(clay.file)
silt = readRDS(silt.file)
usda = readRDS(usda.file)
bulk = readRDS(bulk.file)
carbon = readRDS(carbon.file)
ph = readRDS(ph.file)

calc.cosby.univariate <- function(clay, sand) {
  maps <- list()
  
  maps[["b"]] <- 2.91 + clay * 0.159
  maps[["ys"]] <- 10^(1.88 + sand * -0.0131)
  maps[["ks"]] <- 10^(-0.884 + sand * 0.0153)
  maps[["vs"]] <- 48.9 + sand * -0.126
  
  return(maps)
}

cosby = calc.cosby.univariate(clay, sand)

b = cosby$b
image.plot(b[,,1], zlim = c(1, 13))
image.plot(b[,,2], zlim = c(1, 13))

image.plot(bulk[,,1])
image.plot(bulk[,,2])

# lookup.df = data.frame(
#   id.vic = 1:12,
#   id.hwsd = c(13, 12, 11, 7, 6, 9, 10, 4, 5, 8, 2, 3),
#   b = c(4.1, 3.99, 4.84, 3.79, 3.05, 5.3, 8.66, 7.48, 8.02, 13, 9.76, 12.28)
# )
# 
# image.plot(usda[,,1])
# image.plot(usda[,,2])
# 
# b = apply(X = array(c(usda), dim = c(length(usda))), MARGIN = 1, FUN = function(x){
#   if(is.na(x)){return(NA)}
#   if(x == 1){x = 3}
#   row = which(lookup.df$id.hwsd == x)
#   if(length(row) == 0){return(NA)}
#   return(lookup.df$b[row])})
# dim(b) = dim(usda)
# 
# image.plot(b[,,1], zlim = c(1, 13))
# image.plot(b[,,2], zlim = c(1, 13))

quartz = sand / 100
expt = 2 * b + 3
bubble = 0.32 * expt + 4.3
bulk = bulk * 1e3

image.plot(expt[,,1])
image.plot(expt[,,2])

dir.create(dirname(bulk.out))
dir.create(dirname(carbon.out))
dir.create(dirname(ph.out))
dir.create(dirname(quartz.out))
dir.create(dirname(expt.out))
dir.create(dirname(bubble.out))

saveRDS(bulk, bulk.out)
saveRDS(carbon, carbon.out)
saveRDS(ph, ph.out)
saveRDS(quartz, quartz.out)
saveRDS(expt, expt.out)
saveRDS(bubble, bubble.out)

