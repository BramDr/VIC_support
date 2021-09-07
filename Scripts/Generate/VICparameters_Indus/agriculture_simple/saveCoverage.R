rm(list = ls())
library(fields)

# Input
vegetation.file = "./Saves/coverage_vegetation_monthly_5min_Indus.RDS"
crops.file = "./Saves/coverage_crops_monthly_5min_Indus.RDS"
vegetation.crop.idx = 12
coverage.out = "./Saves/coverage_monthly_5min_Indus.RDS"

# Load
vegetation = readRDS(vegetation.file)
crops = readRDS(crops.file)

# Calculate
coverage = array(NA, dim = c(dim(vegetation)[1:2], dim(vegetation)[3] + dim(crops)[3] - 1, 12))
coverage[,,1:(dim(vegetation)[3] - 1),] = vegetation[,,1:(dim(vegetation)[3] - 1),]
coverage[,,dim(vegetation)[3]:(dim(vegetation)[3] - 1 + dim(crops)[3] - 1),] = crops[,,1:(dim(crops)[3] - 1),]
coverage[,,dim(coverage)[3],] = vegetation[,,dim(vegetation)[3],] + crops[,,dim(crops)[3],]

coverage.sum = apply(X = coverage, MARGIN = c(1,2,4), FUN = sum)
coverage.max = apply(X = coverage.sum, MARGIN = c(1,2), FUN = max)
image.plot(coverage.max)

# Save
dir.create(dirname(coverage.out))
saveRDS(coverage, coverage.out)
