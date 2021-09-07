rm(list = ls())

# Inputs
crops.out <- "./Saves/wofost_crop_mapping.csv"

# Setup
names <- c(
  "wheat", "maize", "soybean", "rice", "sugarcane", "sorghum", "millet",
  "rapeseed", "sugarbeet", "barley", "cassava", "chickpea", "sunflower",
  "groundnut", "cowpea", "cotton", "potato", "fababean",
  "mungbean", "pigeonpea", "sweetpotato", "tobacco", "rye"
)
label <- c(
  "whe", "mai", "soy", "ric", "sug", "sor", "mil",
  "rap", "sgb", "bar", "cas", "cip", "sun",
  "nut", "cop", "cot", "pot", "fab",
  "mub", "pip", "spo", "tob", "rye"
)
yaml <- c(
  "wheat", "maize", "soybean", "rice", "sugarcane", "sorghum", "millet",
  "rapeseed", "sugarbeet", "barley", "cassava", "chickpea", "sunflower",
  "groundnut", "cowpea", "cotton", "potato", "fababean",
  "mungbean", "pigeonpea", "sweetpotato", "tobacco", "barley"
)
DATp <- c(
  "WWHEAT1", "MAG202", "SOY0902", "RIC501", "SUGRCANE", "SORGHUM", "MILLET",
  "RAP1001", "SUG0601", "BAR1", "CASSAVA", "CHICKPEA", "SUN1101",
  "GR_NUT", "COWPEA", "COTTON", "POT701", "FBE0801",
  "MUNGBEAN", "PIGEOPEA", "SWPOTATO", "TOBACCO", "BAR1"
)

# Calculate
crops <- data.frame(
  name = names,
  label = label,
  yaml = yaml,
  DATp = DATp,
  stringsAsFactors = F
)

# Save
dir.create(dirname(crops.out))
write.csv(crops, crops.out, row.names = F)
