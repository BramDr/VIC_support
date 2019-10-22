library(ncdf4)
library(fields)
rm(list = ls())

# Input
scc.file = "../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
cc.file = "../../../Data/Transformed/LandUse/cropCalendar_30min_global.csv"
cc.out = "./Saves/Cc_30min_global.RDS"

# Load
scc = read.csv(scc.file, stringsAsFactors = F, header = T)
cc = read.csv(cc.file, stringsAsFactors = F, header = T)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

crop.id = c(1:13, 15:17, 20, 26)
crop.id = c(crop.id, crop.id + 26)
crop.desc = c("wheat", "maize", "rice", "barley", "rye", "milet", "sorghum",
              "soybeans", "sunflower", "potatoes", "cassava", "sugar_cane", 
              "sugar_beet", "rapeseed", "groundnuts", "pulses", "grapes", "other")
crop.desc = paste0(crop.desc, rep(c("_IRC", "_RFC"), each = length(crop.desc)))

crop.scc = scc[scc$crop %in% crop.id, ]
crop.s = aggregate(x = crop.scc$subcrop, by = list(crop.scc$crop), FUN = max)

crop.id.s = c()
crop.desc.s = c()
for (i in 1:nrow(crop.s)) {
  reps = crop.s$x[i]
  id = crop.s$Group.1[i]
  
  idx = which(crop.id == id)
  cdesc = crop.desc[idx]
  
  n.crop.id = rep(id, reps)
  n.crop.desc = paste0(cdesc, paste0("_S", 1:reps))
  
  crop.id.s = c(crop.id.s, n.crop.id)
  crop.desc.s = c(crop.desc.s, n.crop.desc)
}

# Calculate
scc.cols = which(colnames(crop.scc) %in% paste0("area.",1:12))
cc.cols = which(colnames(cc) %in% paste0("area.",1:12))
Cc = array(NA, dim = c(length(lons), length(lats), length(crop.id.s), 12))
for(i in 1:nrow(crop.scc)){
  print(i)
  cid = crop.scc$crop[i]
  scid = crop.scc$subcrop[i]
  
  idx = which(crop.id.s == cid)[scid]
  cdesc = crop.desc[idx]
  x = which(lons == crop.scc$lon[i])
  y = which(lats == crop.scc$lat[i])
  
  cc.row = which(cc$cell_ID == crop.scc$cell_ID[i] & cc$crop == crop.scc$crop[i])
  
  break
  coverage = crop.scc[i, scc.cols] / cc[cc.row, cc.cols]
  coverage[is.na(coverage)] = 0
  Cc[x,y,idx,] = as.numeric(coverage)
}

# Save