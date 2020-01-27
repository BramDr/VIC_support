rm(list = ls())

# Input
crop.file = "./Saves/crop_mapping_single.csv"
crop.dir = "../../../Data/WOFOST/Parameters/Crop/global/SA/"
management.dir = "../../../Data/WOFOST/Parameters/Management/global/"
conf.out = "../../../Data/VIC/Parameters/global/SA/wofost_params_single_global.txt"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
crop.files = list.files(crop.dir, full.names = T)
management.files = list.files(management.dir, full.names = T, pattern = "limited")

# Calculate & save
header = "
** WOFOST PARAMETER CONFIGURATION FILE for use with WOFOST-C, October 2019
** For use in VIC-WOFOST
"

for (i in 1:nrow(crops)){
  print(crops$mirca.name[i])
  
  text = c()
  conf.out.tmp = gsub(x = conf.out, pattern = "wofost_params_", replacement = paste0("wofost_params_", crops$mirca.name[i], "_"))
  
  crop.file = grep(x = crop.files, pattern = paste0("crop_params_", crops$wofost.name[i], ".txt"), value = T)
  for (j in 1:length(management.files)) {
        line = paste0(getwd(), "/ ", 
                      crop.file, " ", 
                      management.files[j], " ", 
                      " 0 ")
        text = c(text, line)
  }
  
  text = c(header, text)
  dir.create(dirname(conf.out.tmp))
  writeLines(text = text, con = conf.out.tmp)
}
