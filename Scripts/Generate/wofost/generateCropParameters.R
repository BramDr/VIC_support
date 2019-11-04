rm(list = ls())

# Input
param.desc.file = "../../../Data/Primary/WOFOST/Crop/cropParameterDescription.csv"
in.dir = "../../../Data/Transformed/WOFOST/Crop/"
out.dir = "../../../Data/WOFOST/Parameters/Crop/global/"

# Load
param.desc = read.csv(param.desc.file, stringsAsFactors = F)

# Calculate & save
n.space = 16
in.files = list.files(path = in.dir, pattern = ".yaml", full.names = T)
for (in.file in in.files) {
  out.file = gsub(x = basename(in.file), pattern = ".yaml", replacement = "")
  out.file = paste0(out.dir, "/", "crop_params_", out.file, ".txt")
  print(basename(out.file))
  
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
        if (is.na(value)) {
          value = ""
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
  
  header = "
** WOFOST MANAGEMENT FILE for use with WOFOST-C, October 2019
** Based on Allard de Wit crop files
"
  text = c(header, text)
  
  dir.create(dirname(out.file))
  writeLines(text = text, con = out.file)
}
