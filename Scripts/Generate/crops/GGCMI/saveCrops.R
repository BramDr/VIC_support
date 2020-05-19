rm(list = ls())

# Inputs
crops.out = "./Saves/crop_mapping.csv"

# Setup
names = c("wheat", "maize", "soybean", "rice", "grass", "sugarcane", "sorghum", "millet",
			   "rapeseed", "sugarbeet", "barley", "cassava", "pea", "sunflower",
			   "groundnut", "bean", "cotton", "potato")
label = c("whe", "mai", "soy", "ric", "mgr", "sug", "sor", "mil", 
          "rap", "sgb", "bar", "cas", "pea", "sun", 
          "nut", "ben", "cot", "pot")
season = c("Wheat", "Maize", "Soybeans", "Rice", NA, "Sugar_cane", "Sorghum", "Millet",
           "Rapeseed", "Sugar_beet", "Barley", "Cassava", "Pulses", "Sunflower",
		   "Groundnuts", "Pulses", "Cotton", "Potatoes")
crop = c("wheat", "maize", "soybean", "rice", NA, "sugarcane", "sorghum", "millet",
		 "rapeseed", "sugarbeet", "barley", "cassava", "fababean", "sunflower",
		 "groundnut", "cowpea", "cotton", "potato")
priority = c(1,1,1,1,3,2,2,2,2,2,2,2,2,2,2,2,2,2)

# Calculate
crops = data.frame(name = rep(names, 2),
                   label = rep(label, 2),
                   season = c(paste0(season, "_ir"), paste0(season, "_rf")),
                   crop = rep(crop, 2),
                   priority = rep(priority, 2),
                   water = c(rep("irrigated", length(names)), rep("rainfed",  length(names))),
                   stringsAsFactors = F)

# Save
dir.create(dirname(crops.out))
write.csv(crops, crops.out, row.names = F)
