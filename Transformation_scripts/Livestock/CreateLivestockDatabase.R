library(raster)
library(fields)
rm(list = ls())

livestock.dir = "Input"
mask.file = "Input/domain_global.nc"
out.dir = "Output/"

# Load
livestock.files = list.files(livestock.dir, pattern = ".tif$", full.names = T)
livestock.ct = raster(grep(x = livestock.files, pattern = "Ct_.*_Da", value = T))
livestock.bf = raster(grep(x = livestock.files, pattern = "Bf_.*_Da", value = T))
livestock.dk = raster(grep(x = livestock.files, pattern = "Dk_.*_Da", value = T))
livestock.ch = raster(grep(x = livestock.files, pattern = "Ch_.*_Da", value = T))
livestock.gt = raster(grep(x = livestock.files, pattern = "Gt_.*_Da", value = T))
livestock.sh = raster(grep(x = livestock.files, pattern = "Sh_.*_Da", value = T))
livestock.ho = raster(grep(x = livestock.files, pattern = "Ho_.*_Da", value = T))
livestock.pg = raster(grep(x = livestock.files, pattern = "Pg_.*_Da", value = T))

# Setup
livestock.ct = data.frame(rasterToPoints(aggregate(x = livestock.ct, fact = 6, fun = sum)))
livestock.bf = data.frame(rasterToPoints(aggregate(x = livestock.bf, fact = 6, fun = sum)))
livestock.dk = data.frame(rasterToPoints(aggregate(x = livestock.dk, fact = 6, fun = sum)))
livestock.ch = data.frame(rasterToPoints(aggregate(x = livestock.ch, fact = 6, fun = sum)))
livestock.gt = data.frame(rasterToPoints(aggregate(x = livestock.gt, fact = 6, fun = sum)))
livestock.sh = data.frame(rasterToPoints(aggregate(x = livestock.sh, fact = 6, fun = sum)))
livestock.ho = data.frame(rasterToPoints(aggregate(x = livestock.ho, fact = 6, fun = sum)))
livestock.pg = data.frame(rasterToPoints(aggregate(x = livestock.pg, fact = 6, fun = sum)))

colnames(livestock.ct) = c("lon", "lat", "animals")
colnames(livestock.bf) = c("lon", "lat", "animals")
colnames(livestock.dk) = c("lon", "lat", "animals")
colnames(livestock.ch) = c("lon", "lat", "animals")
colnames(livestock.gt) = c("lon", "lat", "animals")
colnames(livestock.sh) = c("lon", "lat", "animals")
colnames(livestock.ho) = c("lon", "lat", "animals")
colnames(livestock.pg) = c("lon", "lat", "animals")

# Calculate
ser.cattle = 2.333 / sum(livestock.ct$animals) * 1e9 / 365 # m3/d
ser.pigs = 4.163 /sum(livestock.pg$animals) * 1e9 / 365 # m3/d
ser.chicken = 0.046 / sum(livestock.ch$animals) * 1e9 / 365 / 100 # m3/d

drink.cattle = 11.400 / sum(livestock.ct$animals) * 1e9 / 365 # m3/d
drink.buffalo = 1.360 / sum(livestock.bf$animals) * 1e9 / 365 # m3/d
drink.goat = 0.770 / sum(livestock.gt$animals) * 1e9 / 365 # m3/d
drink.sheep = 1.110 / sum(livestock.sh$animals) * 1e9 / 365 # m3/d
drink.pigs = 0.690 /sum(livestock.pg$animals) * 1e9 / 365 # m3/d
drink.chicken = 0.930 / sum(livestock.ch$animals) * 1e9 / 365 / 100 # m3/d

scale.cattle = c((102.3 - 73.2) / 10, (73.2 - 44.1) / 10) / 73.2
scale.goat = c((11.9 - 9.6) / 10, (9.6 - 7.6) / 10) / 9.6
scale.sheep = c((20.1 - 12.9) / 10, (12.9 - 8.7) / 10) / 12.9
scale.chicken = c((50.5 - 25.8) / 10, (25.8 - 13.2) / 10) / 25.8
scale.pig = c((46.7 - 28.3) / 10, (28.3 - 17.2) / 10) / 28.3

livestock.ct$servicing = ser.cattle * livestock.ct$animals
livestock.bf$servicing = ser.cattle * livestock.bf$animals
livestock.dk$servicing = ser.chicken * livestock.dk$animals
livestock.ch$servicing = ser.chicken * livestock.ch$animals
livestock.gt$servicing = ser.pigs * livestock.gt$animals
livestock.sh$servicing = ser.pigs * livestock.sh$animals
livestock.ho$servicing = ser.cattle * livestock.ho$animals
livestock.pg$servicing = ser.pigs * livestock.pg$animals

livestock.ct$drink = drink.cattle * livestock.ct$animals
livestock.bf$drink = drink.buffalo * livestock.bf$animals
livestock.dk$drink = drink.chicken * livestock.dk$animals
livestock.ch$drink = drink.chicken * livestock.ch$animals
livestock.gt$drink = drink.goat * livestock.gt$animals
livestock.sh$drink = drink.sheep * livestock.sh$animals
livestock.ho$drink = drink.cattle * livestock.ho$animals
livestock.pg$drink = drink.pigs * livestock.pg$animals

livestock.ct$scaleHigh = scale.cattle[1]
livestock.bf$scaleHigh = scale.cattle[1]
livestock.dk$scaleHigh = scale.chicken[1]
livestock.ch$scaleHigh = scale.chicken[1]
livestock.gt$scaleHigh = scale.goat[1]
livestock.sh$scaleHigh = scale.sheep[1]
livestock.ho$scaleHigh = scale.cattle[1]
livestock.pg$scaleHigh = scale.pig[1]

livestock.ct$scaleLow = scale.cattle[2]
livestock.bf$scaleLow = scale.cattle[2]
livestock.dk$scaleLow = scale.chicken[2]
livestock.ch$scaleLow = scale.chicken[2]
livestock.gt$scaleLow = scale.goat[2]
livestock.sh$scaleLow = scale.sheep[2]
livestock.ho$scaleLow = scale.cattle[2]
livestock.pg$scaleLow = scale.pig[2]

# Save
write.csv(livestock.ct, paste0(out.dir, "grid_Ct_global.csv"), row.names = T)
write.csv(livestock.bf, paste0(out.dir, "grid_Bf_global.csv"), row.names = T)
write.csv(livestock.dk, paste0(out.dir, "grid_Dk_global.csv"), row.names = T)
write.csv(livestock.ch, paste0(out.dir, "grid_Ch_global.csv"), row.names = T)
write.csv(livestock.gt, paste0(out.dir, "grid_Gt_global.csv"), row.names = T)
write.csv(livestock.sh, paste0(out.dir, "grid_Sh_global.csv"), row.names = T)
write.csv(livestock.ho, paste0(out.dir, "grid_Ho_global.csv"), row.names = T)
write.csv(livestock.pg, paste0(out.dir, "grid_Pg_global.csv"), row.names = T)
