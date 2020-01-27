rm(list = ls())

# Input
crop.file = "./Saves/crop_mapping_single.csv"
param.desc.file = "../../../Data/Primary/WOFOST/Crop/cropParameterDescription.csv"
in.dir = "../../../Data/Transformed/WOFOST/Crop/"
out.dir = "../../../Data/WOFOST/Parameters/Crop/global/SA/"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
param.desc = read.csv(param.desc.file, stringsAsFactors = F)

# Calculate & save
n.space = 16
in.files = list.files(path = in.dir, pattern = ".yaml", full.names = T)
for (i in 1:nrow(crops)) {
  print(crops$mirca.name[i])
  
  in.file = grep(x = in.files, pattern = paste0("//",crops$wofost.name[i]), value = TRUE)
  out.file = paste0(out.dir, "/", "crop_params_", crops$mirca.name[i], ".txt")
  
  in.params = read.csv(in.file, stringsAsFactors = F, row.names = 1)
  text = c()
  for (i in 1:nrow(param.desc)) {
    
    ## Add section headers
    if (i == 1){
      section = paste0("** ", param.desc$section[i])
      text = c(text, " ",  section)
    } else if (param.desc$section[i] != param.desc$section[i-1]){
      section = paste0("** ", param.desc$section[i])
      text = c(text, " ", section)
    }
    
    ## Get parameter name
    name = paste0(param.desc$name[i])
    space.before = rep(x = " ", times = max(1, n.space - nchar(name)))
    space.before = paste0(space.before, collapse = "")
    space.before2 = rep(x = " ", times = n.space)
    space.before2 = paste0(space.before2, collapse = "")
    
    ## Get parameter value(s)
    if(param.desc$size[i] == 1){
      for(j in 1:param.desc$size[i]){
        value = in.params[paste0(param.desc$name[i], "_", j), 1]
        if(param.desc$name[i] == "MaxHeight") {
          value = 2
        } else if(param.desc$name[i] == "RGL") {
          value = 100
        } else if(param.desc$name[i] == "RadAtten") {
          value = 0.5
        } else if(param.desc$name[i] == "WindAtten") {
          value = 0.5
        } else if(param.desc$name[i] == "TrunkRatio") {
          value = 0.2
        } else if(param.desc$name[i] == "Albedo") {
          value = 0.1
        } else if(param.desc$name[i] == "MinStomResist") {
          value = 100
        } else if(param.desc$name[i] == "MaxArchResist") {
          value = 2
        } else if(param.desc$name[i] == "IDSL") {
          value = 0
        }
        
        if (is.na(value)) {
          next
        }
        if (length(grep(x = param.desc$name[i], pattern = "VERN")) > 0) {
          next
        }
        
        space.after = rep(x = " ", times = max(1, n.space - nchar(value)))
        space.after = paste0(space.after, collapse = "")
        
        description = paste0("! ", param.desc$description[i])
        
        line = paste0(name,  space.before, "= ", value, space.after, description)
        text = c(text, line)
      }
    } else {
      for(j in 1:param.desc$size[i]){
        valuex = in.params[paste0(param.desc$name[i], "_", j), 1]
        valuey = in.params[paste0(param.desc$name[i], "_", j), 2]
        if (is.na(valuex)) {
          next
        }
        if (is.na(valuey)) {
          stop("X is present but not Y?")
        }
        if (length(grep(x = param.desc$name[i], pattern = "VERN")) > 0) {
          next
        }
        value = paste0(valuex, ", ", valuey)
        
        valuex = in.params[paste0(param.desc$name[i], "_", j + 1), 1]
        if(!is.na(valuex)){
          value = paste0(value, ",")
        }
        
        if(j == 1){
          space.after = rep(x = " ", times = max(1, n.space - nchar(value)))
          space.after = paste0(space.after, collapse = "")
          
          description = paste0("! ", param.desc$description[i])
          
          line = paste0(name,  space.before, "= ", value, space.after, description)
        } else {
          line = paste0(space.before2, "  ", value)
        }
        
        text = c(text, line)
      }
    }
  }
  
  header = paste0("
** WOFOST MANAGEMENT FILE
** Based on Allard de Wit crop files
** ", date(), "
"
)
  text = c(header, text)
  
  dir.create(dirname(out.file))
  writeLines(text = text, con = out.file)
}
