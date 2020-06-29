rm(list = ls())

# Input
crop.file = "./Saves/crop_mapping.csv"
crop.dir = "../../../../Data/WOFOST/Parameters/Crop/global/"
management.dir = "../../../../Data/WOFOST/Parameters/Management/global/"
conf.out = "../../../../Data/VIC/Parameters/global/wofost_params_MIRCAhybrid_global.txt"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
crop.files = list.files(crop.dir, full.names = T, pattern = "crop_params")
management.files = list.files(management.dir, full.names = T, pattern = "default")

# Calculate & save
header = "
** WOFOST PARAMETER CONFIGURATION FILE for use with WOFOST-C
** For use in VIC-WOFOST
"
text = c()

i = 1
for (i in 1:nrow(crops)){
  print(crops$name[i])
  
  crop.file = grep(x = crop.files, pattern = paste0("_", crops$wofost[i], ".txt"), value = T)
  
  crop.file = normalizePath(crop.file)
  
  base.dir = gsub(x = crop.file, pattern = "WOFOST/Parameters.*", replacement = "WOFOST/")
  crop.file = gsub(x = crop.file, pattern = base.dir, replacement = "")
  
  emergence = 0
  if(crops$name[i] == "rice"){
      emergence = 1
  }
  
  for (j in 1:length(management.files)) {
    management.file = management.files[j]
      
    management.file = normalizePath(management.file)
    management.file = gsub(x = management.file, pattern = base.dir, replacement = "")
    
    line = paste0(base.dir, " ",
                "./", crop.file, " ", 
                "./", management.file, " ", 
                "01-01 ", emergence)
    text = c(text, line)
  }
}
text = c(header, text)

dir.create(dirname(conf.out))
writeLines(text = text, con = conf.out)

