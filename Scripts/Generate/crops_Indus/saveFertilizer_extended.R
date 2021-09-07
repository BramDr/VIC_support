library(fields)
library(raster)
library(ncdf4)
rm(list = ls())

fertilizer.file <- "../cropsForcing_indus/Saves/fertilizerN_extended_5min_Indus.RDS"
fertilizer.out.2050 <- "./Saves/fertilizer_extended_2050_5min_Indus.RDS"
fertilizer.out.2100 <- "./Saves/fertilizer_extended_2100_5min_Indus.RDS"

# Load
fertilizer <- readRDS(fertilizer.file)
years = as.numeric(dimnames(fertilizer)[[3]])

# Calculate
fertilizer.n.2050 <- array(0, dim = c(dim(fertilizer)[c(1:2,4:5)]))
fertilizer.p.2050 <- array(0, dim = c(dim(fertilizer)[c(1:2,4:5)]))
fertilizer.k.2050 <- array(0, dim = c(dim(fertilizer)[c(1:2,4:5)]))
fertilizer.dvs.2050 <- array(0, dim = c(dim(fertilizer)[c(1:2,4:5)]))

fertilizer.n.2100 <- array(0, dim = c(dim(fertilizer)[c(1:2,4:5)]))
fertilizer.p.2100 <- array(0, dim = c(dim(fertilizer)[c(1:2,4:5)]))
fertilizer.k.2100 <- array(0, dim = c(dim(fertilizer)[c(1:2,4:5)]))
fertilizer.dvs.2100 <- array(0, dim = c(dim(fertilizer)[c(1:2,4:5)]))

fertilizer.dvs.2050[,,,1] = 0
fertilizer.dvs.2050[,,,2] = 0.25
fertilizer.dvs.2050[,,,3] = 0.5
fertilizer.dvs.2050[,,,4] = 0.75

fertilizer.dvs.2100[,,,1] = 0
fertilizer.dvs.2100[,,,2] = 0.25
fertilizer.dvs.2100[,,,3] = 0.5
fertilizer.dvs.2100[,,,4] = 0.75

fertilizer.n.2050 = fertilizer[,,years == 2050,,]
fertilizer.n.2100 = fertilizer[,,years == 2100,,]

image.plot(fertilizer.n.2050[,,1,1])
image.plot(fertilizer.n.2100[,,1,1])

# Save
fertilizer.out.tmp <- gsub(x = fertilizer.out.2050, pattern = "fertilizer_", replacement = "fertilizerDVS_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.dvs.2050, fertilizer.out.tmp)
fertilizer.out.tmp <- gsub(x = fertilizer.out.2050, pattern = "fertilizer_", replacement = "fertilizerN_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.n.2050, fertilizer.out.tmp)
fertilizer.out.tmp <- gsub(x = fertilizer.out.2050, pattern = "fertilizer_", replacement = "fertilizerP_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.p.2050, fertilizer.out.tmp)
fertilizer.out.tmp <- gsub(x = fertilizer.out.2050, pattern = "fertilizer_", replacement = "fertilizerK_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.k.2050, fertilizer.out.tmp)

fertilizer.out.tmp <- gsub(x = fertilizer.out.2100, pattern = "fertilizer_", replacement = "fertilizerDVS_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.dvs.2100, fertilizer.out.tmp)
fertilizer.out.tmp <- gsub(x = fertilizer.out.2100, pattern = "fertilizer_", replacement = "fertilizerN_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.n.2100, fertilizer.out.tmp)
fertilizer.out.tmp <- gsub(x = fertilizer.out.2100, pattern = "fertilizer_", replacement = "fertilizerP_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.p.2100, fertilizer.out.tmp)
fertilizer.out.tmp <- gsub(x = fertilizer.out.2100, pattern = "fertilizer_", replacement = "fertilizerK_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.k.2100, fertilizer.out.tmp)
