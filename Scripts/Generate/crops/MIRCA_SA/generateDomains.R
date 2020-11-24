library(fields)
library(ncdf4)
rm(list = ls())

# Input
function.script <- "../../../../Scripts/Support/mapFunctions.R"
template.file <- "../../../../Data/Primary/VIC/domain_global.nc"
crop.file <- "./Saves/crop_mapping.csv"
Ncrop.file <- "./Saves/Ncrop_30min_global.RDS"
tsum1.file <- "./Saves/tsum1_30min_global.RDS"
tsum2.file <- "./Saves/tsum2_30min_global.RDS"
domain.out <- "../../../../Data/VIC/Parameters/global/VICWOFOST_SA/domain_global.nc"

# Load
crops <- read.csv(crop.file, stringsAsFactors = F)
Ncrop <- readRDS(Ncrop.file)
tsum1 <- readRDS(tsum1.file)
tsum2 <- readRDS(tsum2.file)

nc <- nc_open(template.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

# Setup
source(function.script)
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
noptions <- 16

i <- 9
for (i in 1:nrow(crops)) {
  print(crops$name[i])

  na.map <- is.na(Ncrop[, , i]) | Ncrop[, , i] == 0
  # image.plot(na.map)

  ## Calculate
  mask.filled <- fillMap(map = mask, na.map = na.map, nearest.function = getNearestZero)

  domain.out.tmp <- gsub(x = domain.out, pattern = "domain_", replacement = paste0("domain_", crops$name[i], "_", crops$water[i], "_", crops$season[i], "_"))
  dir.create(dirname(domain.out.tmp))
  file.copy(from = template.file, to = domain.out.tmp, overwrite = T)

  nc <- nc_open(domain.out.tmp, write = T)
  ncvar_put(nc = nc, varid = "mask", vals = mask.filled)
  nc_close(nc)
}
