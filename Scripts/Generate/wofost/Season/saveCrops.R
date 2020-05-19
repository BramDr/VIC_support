rm(list = ls())

# Input
crops.out = "./Saves/crop_mapping.csv"

# Setup

names = c("wheat", "maize", "soybean", "rice", "sugarcane", "sorghum", "millet",
          "rapeseed", "sugarbeet", "barley", "cassava", "chickpea", "sunflower",
          "groundnut", "cowpea", "cotton", "potato", "fababean", 
          "mungbean", "pigeonpea", "sweetpotato", "tobacco")
water = c(rep("irrigated", length(names)), rep("rainfed", length(names)))
names = rep(names, 2)
mirca = c(1,2,8,3,12,7,6,
          15,13,4,11,17,9,
          16,17,21,10,17,
          17,17,NA,NA)
sage = c("Wheat", "Maize", "Soybeans", "Rice", NA, "Sorghum", "Millet",
         "Rapeseed.Winter", "Sugarbeets", "Barley", "Cassava", "Pulses", "Sunflower",
         "Groundnuts", "Pulses", "Cotton", "Potatoes", "Pulses",
         "Pulses", "Pulses", "Sweet.Potatoes", NA)
sage = rep(sage, 2)

# Calculate
crops = data.frame(names = names,
                   water = water,
                   mirca = mirca,
                   sage = sage,
                   stringsAsFactors = F)

# Save
dir.create(dirname(crops.out))
write.csv(crops, crops.out, row.names = F)
