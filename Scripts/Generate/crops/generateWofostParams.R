rm(list = ls())

# Input
crop.file = "./Saves/crop_mapping_MIRCA.csv"
crop.dir = "../../../Data/WOFOST/Parameters/Crop/global/SA/"
management.dir = "../../../Data/WOFOST/Parameters/Management/global/"
conf.out = "../../../Data/VIC/Parameters/global/wofost_params_global.txt"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
crop.files = list.files(crop.dir, full.names = T)
management.files = list.files(management.dir, full.names = T)

# Calculate
header = paste0("
** WOFOST PARAMETER CONFIGURATION FILE
** For use in VIC-WOFOST
** ", date(), "
")

text = c()
for (i in 1:nrow(crops)){
  print(crops$mirca.name[i])
  
  crop.file = grep(x = crop.files, pattern = paste0("crop_params_", crops$mirca.name[i], ".txt"), value = T)
  for (j in 1:length(management.files)) {
        line = paste0(getwd(), "/ ", 
                      crop.file, " ", 
                      management.files[j], " ", 
                      " 0 ")
        text = c(text, line)
  }
}

text = c(header, text)

# Save
dir.create(dirname(conf.out))
writeLines(text = text, con = conf.out)
