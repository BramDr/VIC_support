rm(list = ls())

# Input
crop.dir = "../../../../Data/WOFOST/Parameters/Crop/global/"
management.dir = "../../../../Data/WOFOST/Parameters/Management/global/"
conf.out = "../../../../Data/VIC/Parameters/global/GGCMI_3/wofost_params_global.txt"

# Load
crop.files = list.files(crop.dir, full.names = T, pattern = "crop_params")
management.files = list.files(management.dir, full.names = T, pattern = "default")

# Calculate & save
header = "
** WOFOST PARAMETER CONFIGURATION FILE for use with WOFOST-C
** For use in VIC-WOFOST
"
i = 1
for (i in 1:length(crop.files)){
  crop.file = crop.files[i]
  crop.pattern = gsub(x = basename(crop.file), pattern = ".txt", replacement = "")
  crop.pattern = gsub(x = crop.pattern, pattern = "crop_params_", replacement = "")
  print(crop.pattern)
  
  text = c()
  conf.out.tmp = gsub(x = conf.out, pattern = "wofost_params_", replacement = paste0("wofost_params_", crop.pattern, "_"))
  
  for (j in 1:length(management.files)) {
        management.file = management.files[j]
        
        crop.file = normalizePath(crop.file)
        management.file = normalizePath(management.file)
        
        base.dir = gsub(x = crop.file, pattern = "WOFOST/Parameters.*", replacement = "WOFOST/")
        crop.file = gsub(x = crop.file, pattern = base.dir, replacement = "")
        management.file = gsub(x = management.file, pattern = base.dir, replacement = "")
      
        line = paste0(base.dir, " ", 
                      "./", crop.file, " ", 
                      "./", management.file, " ", 
                      "01-01 0 ")
        text = c(text, line)
  }
  
  text = c(header, text)
  dir.create(dirname(conf.out.tmp))
  writeLines(text = text, con = conf.out.tmp)
}

