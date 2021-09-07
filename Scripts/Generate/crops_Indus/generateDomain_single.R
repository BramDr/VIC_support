library(fields)
library(ncdf4)
rm(list = ls())

# Input
function.script <- "../../Support/mapFunctions.R"
template.file <- "../../../Data/VIC/Parameters/Indus_5min/domain_Indus.nc"
crop.file <- "./Saves/crop_mapping.RDS"
season.file <- "./Saves/season_fraction_crops_monthly_5min_Indus.RDS"
tsum1.file <- "./Saves/tsum1_crops_5min_Indus.RDS"
tsum2.file <- "./Saves/tsum2_crops_5min_Indus.RDS"
domain.out <- "../../../Data/VIC/Parameters/Indus_5min/single/domain_Indus.nc"

# Load
crops <- readRDS(crop.file)
season <- readRDS(season.file)
tsum1 <- readRDS(tsum1.file)
tsum2 <- readRDS(tsum2.file)

nc <- nc_open(template.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

# Setup
source(function.script)

season.max = apply(X = season, MARGIN = c(1,2,3), FUN = max)

i = 1
for (i in 1:nrow(crops)) {
  print(crops$name[i])

  na.map <- is.na(season.max[, , i]) | season.max[, , i] == 0 | is.na(mask) | mask == 0
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
