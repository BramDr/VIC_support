rm(list = ls())

# Input
crop.map.file = "./saves/crop_mapping.csv"
crop.dir = "../../../Data/WOFOST/Parameters/Crop/global/"
manage.file = "../../../Data/WOFOST/Parameters/Management/global/managment_params_default.txt"
out.file = "../../../Data/VIC/Parameters/global/wofost_params_global.txt"

# Load
crop.map = read.csv(crop.map.file)

crop.files = list.files(crop.dir, full.names = T)

# Setup
crop.names = crop.map$description
crop.names = gsub(x = crop.names, pattern = "_IRC.*", replacement = "")
crop.names = gsub(x = crop.names, pattern = "_RFC.*", replacement = "")
crop.names = gsub(x = crop.names, pattern = "_", replacement = "")

crop.map$wofost.param = NA
for(i in 1:nrow(crop.map)) {
  crop.name = crop.names[i]
  if(crop.name == "rye") {
    crop.name = "wheat"
  } else if (crop.name == "soybeans") {
    crop.name = "soybean"
  } else if (crop.name == "potatoes") {
    crop.name = "potato"
  } else if (crop.name == "groundnuts") {
    crop.name = "groundnut"
  } else if (crop.name == "pulses") {
    crop.name = "chickpea"
  } else if (crop.name == "other") {
    crop.name = "wheat"
  }
  
  wofost.param.file = grep(x = crop.files, pattern = paste0("_", crop.name), value = T)
  
  if(length(wofost.param.file) > 0){
    crop.map$wofost.param[i] = wofost.param.file
  }
}

# Calculate
text = c()
for(i in 1:nrow(crop.map)) {
  line = paste0(getwd(), " ", crop.map$wofost.param[i], " ", manage.file, " ", 0)
  text = c(text, line)
}
header = "
** WOFOST PARAMETER CONFIGURATION FILE for use with WOFOST-C, October 2019
** For use in VIC-WOFOST
"
text = c(header, text)

# Save
dir.create(dirname(out.file))
writeLines(text = text, con = out.file)
