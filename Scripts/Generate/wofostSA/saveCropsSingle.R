rm(list = ls())

# Input
crops.out = "./Saves/crop_mapping_single.csv"

# Setup
mirca.id = c(1:13, 15:17, 21)
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
  water = "rainfed"
  season = 1

  crops[nrow(crops) + 1,] = c(crop.mirca.id, crop.vic.id,
                              crop.mirca.name, crop.sage.name, crop.wofost.name,
                              water, season)
}

# Save
dir.create(dirname(crops.out))
write.csv(crops, crops.out, row.names = F)
