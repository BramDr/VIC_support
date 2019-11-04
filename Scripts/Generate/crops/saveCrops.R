rm(list = ls())

# Input
scc.file = "../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
crops.out = "./Saves/crop_mapping.csv"

# Load
scc = read.csv(scc.file, stringsAsFactors = F, header = T)

# Setup
annual.id = c(1:13, 15:17, 21, 26)
annual.id = c(annual.id, annual.id + 26)
annual.desc = c("wheat", "maize", "rice", "barley", "rye", "millet", "sorghum",
              "soybeans", "sunflower", "potatoes", "cassava", "sugar_cane", 
              "sugar_beet", "rapeseed", "groundnuts", "pulses", "cotton", "other")
annual.desc = paste0(annual.desc, rep(c("_IRC", "_RFC"), each = length(annual.desc)))
vic.id = c(11 + annual.id)

# Calculate
annual.scc = scc[scc$crop %in% annual.id, ]
annual.season = aggregate(x = annual.scc$subcrop, by = list(annual.scc$crop), FUN = max)

crops = data.frame(crop.id = numeric(), season = numeric(),
                   description = character(), vic.id = numeric(),
                   stringsAsFactors = F)

for(j in 1:max(annual.season$x)){
  season = j
  
  for (i in 1:nrow(annual.season)) {
    crop.id = annual.season$Group.1[i]
    crop.idx = which(annual.id == crop.id)
    crop.desc = paste0(annual.desc[crop.idx], "_S", season)
    crop.vic.id = vic.id[crop.idx]
    
    if(annual.season$x[i] >= season){
      crops[nrow(crops) + 1,] = c(crop.id, season, crop.desc, crop.vic.id)
    }
  }
}

# Save
dir.create(dirname(crops.out))
write.csv(crops, crops.out, row.names = F)
