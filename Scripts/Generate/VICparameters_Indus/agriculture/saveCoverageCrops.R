rm(list = ls())
library(fields)

# Input
crop.grid.file = "./Saves/coverageMIRCA_grid_monthly_5min_Indus.RDS"
crop.rainfed.file = "./Saves/coverageMIRCA_rainfed_monthly_5min_Indus.RDS"
crop.irrigated.file = "./Saves/coverageMIRCA_irrigated_monthly_5min_Indus.RDS"
crop.paddy.file = "./Saves/coverageMIRCA_paddy_monthly_5min_Indus.RDS"
crop.wheat.rainfed.file = "./Saves/coverageMIRCA_wheatRainfed_monthly_5min_Indus.RDS"
crop.wheat.irrigated.file = "./Saves/coverageMIRCA_wheatIrrigated_monthly_5min_Indus.RDS"
crop.rice.rainfed.file = "./Saves/coverageMIRCA_riceRainfed_monthly_5min_Indus.RDS"
crop.rice.irrigated.file = "./Saves/coverageMIRCA_riceIrrigated_monthly_5min_Indus.RDS"
coverage.out = "./Saves/coverage_crops_monthly_5min_Indus.RDS"

# Load
crop.grid.cv = readRDS(crop.grid.file)
crop.rainfed.cv = readRDS(crop.rainfed.file)
crop.irrigated.cv = readRDS(crop.irrigated.file)
crop.paddy.cv = readRDS(crop.paddy.file)
crop.wheat.rainfed.cv = readRDS(crop.wheat.rainfed.file)
crop.wheat.irrigated.cv = readRDS(crop.wheat.irrigated.file)
crop.rice.rainfed.cv = readRDS(crop.rice.rainfed.file)
crop.rice.irrigated.cv = readRDS(crop.rice.irrigated.file)

crop.npaddy.cv.max = apply(X = crop.rainfed.cv + crop.irrigated.cv, MARGIN = c(1,2), FUN = max)
crop.paddy.cv.max = apply(X = crop.paddy.cv, MARGIN = c(1,2), FUN = max)
image.plot(crop.grid.cv)
image.plot(crop.npaddy.cv.max + crop.paddy.cv.max)

# Calculate
crop.cv = array(NA, dim = c(dim(crop.grid.cv)[1:2], 9, 12))
crop.cv[,,1,] = crop.rainfed.cv - 
  crop.wheat.rainfed.cv - 
  crop.rice.rainfed.cv
crop.cv[,,2,] = crop.irrigated.cv - 
  crop.wheat.irrigated.cv
crop.cv[,,3,] = crop.paddy.cv - 
  crop.rice.irrigated.cv
crop.cv[,,4,] = crop.wheat.rainfed.cv
crop.cv[,,5,] = crop.wheat.irrigated.cv
crop.cv[,,6,] = crop.rice.rainfed.cv
crop.cv[,,7,] = crop.rice.irrigated.cv
for(m in 1:12){
  total.bare = crop.grid.cv - crop.rainfed.cv[,,m] - crop.irrigated.cv[,,m] - crop.paddy.cv[,,m]
  paddy.bare = crop.grid.cv - crop.npaddy.cv.max - crop.paddy.cv[,,m]
  crop.cv[,,8,m] = paddy.bare
  crop.cv[,,9,m] = total.bare - paddy.bare
}

crop.cv.sum = apply(X = crop.cv, MARGIN = c(1,2,4), FUN = sum)
crop.cv.max = apply(X = crop.cv.sum, MARGIN = c(1,2), FUN = max)
image.plot(crop.cv.max)

min(crop.cv, na.rm = T)
crop.cv[crop.cv < 0] = 0

# Save
dir.create(dirname(coverage.out))
saveRDS(crop.cv, coverage.out)
