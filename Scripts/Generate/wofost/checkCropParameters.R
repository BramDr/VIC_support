rm(list = ls())

# Input
crop.file = "./Saves/crop_mapping_MIRCA.csv"
param.desc.file = "../../../Data/Primary/WOFOST/Crop/cropParameterDescription.csv"
in.dir = "../../../Data/Transformed/WOFOST/Crop/"
out.dir = "../../../Data/WOFOST/Parameters/Crop/global/"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
param.desc = read.csv(param.desc.file, stringsAsFactors = F)

# Setup
wofost.name.u = unique(crops$wofost.name)

# Calculate & save
n.space = 16
in.files = list.files(path = in.dir, full.names = T)
for (wofost.name in wofost.name.u) {
  print(wofost.name)
  
  in.files.c = grep(x = in.files, pattern = paste0("//",wofost.name), value = TRUE)
  
  in.params = read.csv(in.files.c[1], stringsAsFactors = F, row.names = 1)
  #in.params[is.na(in.params)] = 0
  if(length(in.files.c) > 1) {
    for(j in 2:length(in.files.c)) {
      in.params.2 = read.csv(in.files.c[j], stringsAsFactors = F, row.names = 1)
      #in.params.2[is.na(in.params.2)] = 0
      
      diffs = in.params - in.params.2
      diffs.r = apply(X = diffs, MARGIN = 1, FUN = function(x) { sum(x != 0, na.rm = T) > 0 })
      
      if (sum(diffs.r) > 0) {
        print(paste0(basename(in.files.c[1]), " <-> ", basename(in.files.c[j])))
        print(diffs[diffs.r,])
      }
    }
  }
}
