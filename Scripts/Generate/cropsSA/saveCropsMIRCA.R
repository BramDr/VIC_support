rm(list = ls())

# Input
scc.file = "../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
crops.out = "./Saves/crop_mapping_MIRCA.csv"

# Load
scc = read.csv(scc.file, stringsAsFactors = F, header = T)

# Setup
mirca.id = c(1:13, 15:17, 21)
mirca.id = c(mirca.id, mirca.id + 26)
vic.id = rep(1, length(mirca.id))

mirca.name = c("wheat", "maize", "rice", "barley", "rye", "millet", "sorghum",
              "soybean", "sunflower", "potato", "cassava", "sugarcane", 
              "sugarbeet", "rapeseed", "groundnut", "pulses", "cotton")
sage.name = c("Wheat", "Maize", "Rice", "Barley", "Oats", "Millet", "Sorghum",
              "Soybeans", "Sunflower", "Potatoes", "Cassava", "Millet", 
              "Sugarbeets", "Rapeseed.Winter", "Groundnuts", "Pulses", "Cotton")
wofost.name = c("wheat", "maize", "rice", "barley", "wheat", "millet", "sorghum",
              "soybean", "sunflower", "potato", "cassava", "sugarcane", 
              "sugarbeet", "rapeseed", "groundnut", "chickpea", "cotton")
mirca.name = c(mirca.name, mirca.name)
sage.name = c(sage.name, sage.name)
wofost.name = c(wofost.name, wofost.name)

# Calculate
crops = data.frame(mirca.id = numeric(), 
                   vic.id = numeric(),
                   mirca.name = character(), 
                   sage.name = character(), 
                   wofost.name = numeric(),
                   water = character(), 
                   season = numeric(),
                   stringsAsFactors = F)

for (i in 1:length(mirca.id)) {
  crop.mirca.id = mirca.id[i]
  crop.vic.id = vic.id[i]
  crop.mirca.name = mirca.name[i]
  crop.sage.name = sage.name[i]
  crop.wofost.name = wofost.name[i]
  
  water = "irrigated"
  if(crop.mirca.id > 26) {
    water = "rainfed"
  }
  
  crop.scc = scc[scc$crop == crop.mirca.id, ]
  crop.season = aggregate(x = crop.scc$subcrop, by = list(crop.scc$crop), FUN = max)
  for(j in 1:crop.season$x) {
    season = j

    crops[nrow(crops) + 1,] = c(crop.mirca.id, crop.vic.id,
                                crop.mirca.name, crop.sage.name, crop.wofost.name,
                                water, season)
  }
}

# Save
dir.create(dirname(crops.out))
write.csv(crops, crops.out, row.names = F)
