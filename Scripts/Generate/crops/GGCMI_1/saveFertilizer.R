library(fields)
library(ncdf4)
rm(list = ls())

# Input
fert.dir = "../../../../Data/Primary/GGCMI/Phase1/AGMIP_NUTRIENTS.HARM.version1.1"
crop.file = "./Saves/crop_mapping.csv"
fertilizer.out = "./Saves/fertilizer_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
fert.files = list.files(fert.dir, pattern = "apprate", full.names = T)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

# Calculate
fertilizer.n = array(0, dim = c(length(lons), length(lats), nrow(crops)))
fertilizer.p = array(0, dim = c(length(lons), length(lats), nrow(crops)))
fertilizer.k = array(0, dim = c(length(lons), length(lats), nrow(crops)))
fertilizer.dvs = array(0, dim = c(length(lons), length(lats), nrow(crops)))
for(i in 1:nrow(crops)){
  print(crops$name[i])
  
  if(crops$priority[i] > 2){
    next
  }
  if(is.na(crops$fertilizer[i])){
    next
  }
  
  fert.file = grep(fert.files, pattern = crops$fertilizer[i], value = T)
  
  nc = nc_open(fert.file)
  fert.n = ncvar_get(nc, nc$var$Napprate)
  fert.p = ncvar_get(nc, nc$var$P2O5apprate)
  fert.k = ncvar_get(nc, nc$var$K2Oapprate)
  nc_close(nc)
  
  fert.n = fert.n[,dim(fert.n)[2]:1]
  fert.p = fert.p[,dim(fert.p)[2]:1]
  fert.k = fert.k[,dim(fert.k)[2]:1]
  
  fert.n.adj = fert.n
  #fert.p.adj = fert.p / (p.a.mass * 2 + o.a.mass * 5) * p.a.mass
  #fert.k.adj = fert.k / (k.a.mass * 2 + o.a.mass * 1) * k.a.mass
  fert.p.adj = fert.p
  fert.k.adj = fert.k
  #image.plot(fert.p)
  #image.plot(fert.p.adj)
  
  fertilizer.n[,,i] = fert.n.adj
  fertilizer.p[,,i] = fert.p.adj
  fertilizer.k[,,i] = fert.k.adj
}

# Save
fertilizer.out.tmp = gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = "fertilizerDVS_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.dvs, fertilizer.out.tmp)
fertilizer.out.tmp = gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = "fertilizerN_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.n, fertilizer.out.tmp)
fertilizer.out.tmp = gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = "fertilizerP_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.p, fertilizer.out.tmp)
fertilizer.out.tmp = gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = "fertilizerK_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.k, fertilizer.out.tmp)

