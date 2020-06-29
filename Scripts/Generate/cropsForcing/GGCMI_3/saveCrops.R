rm(list = ls())

# Inputs
crops.out = "./Saves/crop_mapping.csv"

# Setup
names = c("winterwheat", "springwheat", "maize", "soybean", 
            "rice1", "rice2", "grass", "sugarcane", "sorghum", "millet",
			   "rapeseed", "sugarbeet", "barley", "cassava", "pea", "sunflower",
			   "groundnut", "bean", "cotton", "potato", "rye")
label = c("whe", "swh", "mai", "soy", 
            "ri1", "ri2", "mgr", "sgc", "sor", "mil", 
          "rap", "sgb", "bar", "cas", "pea", "sun", 
          "nut", "bea", "cot", "pot", "rye")
season = c("wwh", "swh", "mai", "soy", 
            "ri1", "ri2", NA, "sgc", "sor", "mil",
           "rap", "sgb", "bar", "cas", "pea", "sun",
		   "nut", "bea", "cot", "pot", "rye")
fertilizer = c("whe", "whe", "mai", "soy", 
            "ric", "ric", "mgr", "sgc", "sor", "mil",
           "rap", "sgb", "bar", "cas", "pea", "sun",
		   "nut", "bea", "cot", "pot", "rye")
crop = c("wheat", "wheat", "maize", "soybean", 
        "rice", "rice", NA, "sugarcane", "sorghum", "millet",
		 "rapeseed", "sugarbeet", "barley", "cassava", "fababean", "sunflower",
		 "groundnut", "cowpea", "cotton", "potato", "rye")
priority = c(1,1,1,1,
             1,1,3,2,2,2,
             2,2,2,2,2,2,
             2,2,2,2,2)

# Calculate
crops = data.frame(name = rep(names, 2),
                   label = rep(label, 2),
                   season = c(paste0(season, "_ir"), paste0(season, "_rf")),
                   fertilizer = rep(fertilizer, 2),
                   crop = rep(crop, 2),
                   priority = rep(priority, 2),
                   water = c(rep("irrigated", length(names)), rep("rainfed",  length(names))),
                   stringsAsFactors = F)

# Save
dir.create(dirname(crops.out))
write.csv(crops, crops.out, row.names = F)
