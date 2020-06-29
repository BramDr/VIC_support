library(fields)
library(ncdf4)
rm(list = ls())

# Input
fert.dir = "../../../../Data/Primary/GGCMI/Phase3/n-fertilizer"
crop.file = "./Saves/crop_mapping.csv"
fertilizer.out = "./Saves/fertilizer_30min_global.RDS"
years = 1850:2015

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
fert.files = list.files(fert.dir, pattern = "fertilizer_application", full.names = T, recursive = T)
man.files = list.files(fert.dir, pattern = "manure_application", full.names = T, recursive = T)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)
nferts = 2
  
scenarios = gsub(x = basename(fert.files), pattern = "fertilizer_application_", replacement = "")
scenarios = gsub(x = scenarios, pattern = "_.*", replacement = "")
scenarios = unique(scenarios)

fertilizer.p = array(0, dim = c(length(lons), length(lats), nferts, length(years)))
fertilizer.k = array(0, dim = c(length(lons), length(lats), nferts, length(years)))
fertilizer.dvs = array(0, dim = c(length(lons), length(lats), nferts, length(years)))
fertilizer.dvs[,,2,] = 0.25

# Calculate
scenario = scenarios[1]
for(scenario in scenarios) {
  
  print(scenario)
  
  man.file = grep(man.files, pattern = paste0("_", scenario, "_"), value = T)
  
  nc = nc_open(man.file)
  man.n = ncvar_get(nc, nc$var$manurerate)
  nc_close(nc)
  
  i = 1
  for(i in 1:nrow(crops)){
    if(crops$water[i] == "irrigated") {
      next
    }
    
    print(crops$name[i])
    
    fertilizer.n = array(0, dim = c(length(lons), length(lats), nferts, length(years)))
    
    if(is.na(crops$fertilizer[i])){
        next
    }

    fert.file = grep(fert.files, pattern = paste0("_", scenario, "_", crops$fertilizer[i], "_"), value = T)

    nc = nc_open(fert.file)
    fert.n = ncvar_get(nc, nc$var$fertrate)
    nc_close(nc)
    
    fertilizer.n[,,1,] = (fert.n[,dim(fert.n)[2]:1,] + man.n[,dim(man.n)[2]:1,]) * 0.2
    fertilizer.n[,,2,] = (fert.n[,dim(fert.n)[2]:1,] + man.n[,dim(man.n)[2]:1,]) * 0.8
    
    # Save
    #fertilizer.out.tmp = gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = paste0("fertilizerDVS_", crops$name[i], "_", scenario , "_"))
    #dir.create(dirname(fertilizer.out.tmp))
    #saveRDS(fertilizer.dvs, fertilizer.out.tmp)
    fertilizer.out.tmp = gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = paste0("fertilizerN_", crops$name[i], "_", scenario , "_"))
    dir.create(dirname(fertilizer.out.tmp))
    saveRDS(fertilizer.n, fertilizer.out.tmp)
    #fertilizer.out.tmp = gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = paste0("fertilizerP_", crops$name[i], "_", scenario , "_"))
    #dir.create(dirname(fertilizer.out.tmp))
    #saveRDS(fertilizer.p, fertilizer.out.tmp)
    #fertilizer.out.tmp = gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = paste0("fertilizerK_", crops$name[i], "_", scenario , "_"))
    #dir.create(dirname(fertilizer.out.tmp))
    #saveRDS(fertilizer.k, fertilizer.out.tmp)
  }
}

