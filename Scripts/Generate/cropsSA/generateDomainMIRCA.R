library(fields)
library(ncdf4)
rm(list = ls())

# Input
template.file <- "../../../Data/Primary/VIC/domain_global.nc"
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
crop.file = "./Saves/crop_mapping_MIRCA.csv"
Cc.dir = "./Saves"
domain.out <- "../../../Data/VIC/Parameters/global/SA/domain_variety_global.nc"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)

nc = nc_open(mask.file)
mask.global = ncvar_get(nc, "mask")
nc_close(nc)

Cc.files = list.files(path = Cc.dir, pattern = "Cc_.*_MIRCA_", full.names = T)

# Calculate and save
for (i in 1:nrow(crops)) {
  print(crops$mirca.name[i])
  
  Cc.file = grep(x = Cc.files, pattern = paste0("Cc_", i, "_"), value = T)
  print(basename(Cc.file))
  Cc = readRDS(Cc.file)
  Cc.sum = apply(X = Cc, MARGIN = c(1,2), FUN = sum)
  
  mask = (Cc.sum > 0) & !is.na(mask.global) & mask.global == 1
  
  domain.out.tmp = gsub(x = domain.out, pattern = "domain_", replacement = paste0("domain_", crops$mirca.name[i], "_", crops$water[i], "_", crops$season[i], "_"))
  print(basename(domain.out.tmp))
  
  dir.create(dirname(domain.out.tmp))
  file.copy(template.file, domain.out.tmp)
  
  nc = nc_open(domain.out.tmp, write = TRUE)
  ncvar_put(nc, "mask", mask)
  ncvar_put(nc, "frac", mask)
  nc_close(nc)
}
