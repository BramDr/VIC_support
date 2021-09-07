library(fields)
library(ncdf4)
rm(list = ls())

# Input
function.script <- "../../../../Scripts/Support/mapFunctions.R"
template.file <- "../../../../Data/Primary/VIC/domain_global.nc"
crop.file <- "./Saves/crop_mapping.csv"
Ncrop.file <- "./Saves/Ncrop_30min_global.RDS"
domain.out <- "../../../../Data/VIC/Parameters/global/CO2Impact_SA/domain_global.nc"

# Load
crops <- read.csv(crop.file, stringsAsFactors = F)
Ncrop <- readRDS(Ncrop.file)

nc <- nc_open(template.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

# Setup
source(function.script)
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
noptions <- dim(Ncrop)[4]

i <- 1
for (i in 1:nrow(crops)) {
  print(crops$name[i])

  o = 1
  for (o in 1:noptions) {
    na.map <- is.na(Ncrop[, , i, o]) | Ncrop[, , i, o] == 0
    # image.plot(na.map)

    ## Calculate
    mask.filled <- fillMap(map = mask, na.map = na.map, nearest.function = getNearestZero)

    domain.out.tmp <- gsub(x = domain.out, pattern = "domain_", replacement = paste0("domain_", crops$name[i], "_", crops$water[i], "_", crops$season[i], "_", o , "_"))
    dir.create(dirname(domain.out.tmp))
    file.copy(from = template.file, to = domain.out.tmp, overwrite = T)

    nc <- nc_open(domain.out.tmp, write = T)
    ncvar_put(nc = nc, varid = "mask", vals = mask.filled)
    nc_close(nc)
  }
}
