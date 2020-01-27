library(fields)
rm(list = ls())

# Input
crop.file = "./Saves/crop_mapping_MIRCA.csv"
crop.param.dir = "../../../Data/Transformed/WOFOST/Crop"
limits.file = "../../../Data/Primary/WOFOST/Crop/cropTsumLimits.csv"
tsum1.out = "./Saves/tsum1_variety_30min_global.RDS"
tsum2.out = "./Saves/tsum2_variety_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
limits = read.csv(limits.file, stringsAsFactors = F)
crop.param.files = list.files(path = crop.param.dir, full.names = T)

# Setup
tsum.step = 100
tsum.day.step = 10
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

# Calculate & save
for(i in 1:nrow(crops)) {
  print(crops$mirca.name[i])
  
  crop.param.file = grep(x = crop.param.files, pattern = paste0("/", crops$wofost.name[i], ".yaml"), value = T)
  crop.param = read.csv(crop.param.file)
  
  tsum1.wofost = crop.param[crop.param[,1] == "TSUM1_1",2]
  tsum2.wofost = crop.param[crop.param[,1] == "TSUM2_1",2]
  
  tsum1.frac = tsum1.wofost / (tsum1.wofost + tsum2.wofost)
  tsum2.frac = tsum2.wofost / (tsum1.wofost + tsum2.wofost)
  
  limit = limits[limits$name == crops$mirca.name[i],]
  tsums = seq(from = limit$tsum_low, to = limit$tsum_high, by = tsum.step)
  if(crops$mirca.name[i] == "cassava") {
    tsums = seq(from = limit$tsum_low, to = limit$tsum_high, by = tsum.day.step)
  }
  
  tsums1 = tsums * tsum1.frac
  tsums2 = tsums * tsum2.frac
  
  tsum1 = list()
  tsum2 = list()  
  for (j in 1:length(tsums)) {
    tsum1[[as.character(tsums[j])]] = array(tsums1[j], dim = c(length(lons), length(lats)))
    tsum2[[as.character(tsums[j])]] = array(tsums2[j], dim = c(length(lons), length(lats)))
  }
  print(length(tsum1))
  
  tsum1.out.tmp = gsub(tsum1.out, pattern = "tsum1_", replacement = paste0("tsum1_", i, "_"))
  tsum2.out.tmp = gsub(tsum2.out, pattern = "tsum2_", replacement = paste0("tsum2_", i, "_"))
  
  dir.create(dirname(tsum1.out.tmp))
  saveRDS(tsum1, tsum1.out.tmp)
  dir.create(dirname(tsum2.out.tmp))
  saveRDS(tsum2, tsum2.out.tmp)
}
image.plot(tsum1[[1]])
image.plot(tsum2[[1]])
