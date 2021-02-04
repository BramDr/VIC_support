library(readODS)
library(plyr)
rm(list = ls())

observed.file <- "../../../../../Data/Primary/FACE/Kimball2017/Biomass Yield Area Phenology Management Weather Soil Moisture.ods"
out.initial <- "./Saves/initial_observed.RDS"

initial = read_ods(path = observed.file, sheet = "Init_conditions_Soil_layers")
initial$layer = NA
initial$layer[initial$icbl %in% 1:30] = 1
initial$layer[initial$icbl %in% 31:130] = 2
initial$layer[initial$icbl %in% 131:170] = 3
initial$depth = NA
initial$depth[initial$icbl %in% c(5)] = 5
initial$depth[initial$icbl %in% c(15, 60, 70, 120, 130)] = 10
initial$depth[initial$icbl %in% c(30)] = 15
initial$depth[initial$icbl %in% c(50,90,110,150,170,190,210)] = 20

depthtotal = aggregate(formula = depth ~ TRNO + layer, data = initial, FUN = sum)
colnames(depthtotal)[3] = "depthtotal"
initial = join(initial, depthtotal)

moist = aggregate(formula = (ich2o * depth) / depthtotal ~ TRNO + layer, data = initial, FUN = sum)
colnames(moist)[3] = "moist"

initial = data.frame(treatment = moist$TRNO, layer = moist$layer, moist = moist$moist)
dir.create(dirname(out.initial))
saveRDS(moist, out.initial)
