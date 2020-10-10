rm(list = ls())

# Input
crop.file = "./Saves/crop_mapping.csv"
param.desc.file = "../../../../Data/Primary/WOFOST/Crop/cropParameterDescription.csv"
in.dir = "../../../../Data/Transformed/Crops/"
out.dir = "../../../../Data/WOFOST/Parameters/Crop/global/"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
param.desc = read.csv(param.desc.file, stringsAsFactors = F, sep = ";")

# Calculate & save
n.space = 16
in.files = list.files(path = in.dir, pattern = ".yaml", full.names = T)
in.files2 = list.files(path = in.dir, pattern = ".DATp", full.names = T)

i = 4
for (i in 1:nrow(crops)) {
  print(crops$name[i])
  
  in.file = grep(x = in.files, pattern = paste0("//",crops$yaml[i]), value = TRUE)
  in.file2 = grep(x = in.files2, pattern = paste0("//",crops$DATp[i]), value = TRUE)
  
  in.params = read.csv(in.file, stringsAsFactors = F, row.names = 1)
  in.params2 = read.csv(in.file2, stringsAsFactors = F, row.names = 1)
  
  text = c()
  for (k in 1:nrow(param.desc)) {
    
    ## Add section headers
    if (k == 1){
      section = paste0("** ", param.desc$section[k])
      text = c(text, " ",  section)
    } else if (param.desc$section[k] != param.desc$section[k-1]){
      section = paste0("** ", param.desc$section[k])
      text = c(text, " ", section)
    }
    
    ## Get parameter name
    name = paste0(param.desc$name[k])
    description = paste0("! ", param.desc$description[k])
    space.before = rep(x = " ", times = max(1, n.space - nchar(name)))
    space.before = paste0(space.before, collapse = "")
    space.before2 = rep(x = " ", times = n.space)
    space.before2 = paste0(space.before2, collapse = "")
    
    ## Get parameters
    rows = which(rownames(in.params) %in% paste0(param.desc$name[k], "_", 1:param.desc$size[k]))
    rows2 = which(rownames(in.params2) %in% paste0(param.desc$name[k], "_", 1:param.desc$size[k]))
    
    values = in.params[rows,]
    values = values[complete.cases(values$X),]
    values2 = in.params2[rows2,]
    values2 = values2[complete.cases(values2$X),]
    
    ## Exchange parameters
    param.used = in.params
    if(param.desc$name[k] != "PERDL" && param.desc$name[k] != "RRI") {
      if (nrow(values2)!= 0) {
        if(param.desc$size[k] == 1) {
          if(values$X != values2$X){
            print(paste0("Changed ", param.desc$name[k], " from ", values$X, " to ", values2$X))
            values = values2
          }
        } else {
          if(nrow(values) != nrow(values2) || sum(values - values2) != 0) {
            print(paste0("Changed ", param.desc$name[k], " table"))
            print(values)
            print(values2)
            param.used = in.params2
            values = values2
          }
        }
      }
    }
    
    ## Put parameters
    if(param.desc$size[k] == 1){
      value = values[1]
      
      if(param.desc$name[k] == "MaxHeight") {
        value = 2
      } else if(param.desc$name[k] == "RGL") {
        value = 100
      } else if(param.desc$name[k] == "RadAtten") {
        value = 0.5
      } else if(param.desc$name[k] == "WindAtten") {
        value = 0.5
      } else if(param.desc$name[k] == "TrunkRatio") {
        value = 0.2
      } else if(param.desc$name[k] == "Albedo") {
        value = 0.1
      } else if(param.desc$name[k] == "MinStomResist") {
        value = 80
      } else if(param.desc$name[k] == "MaxArchResist") {
        value = 2
      } else if(param.desc$name[k] == "Fcanopy") {
        value = 1
      } else if(param.desc$name[k] == "IDSL") {
        value = 0
      }
      
      if(crops$name[i] == "rice"){
        if(param.desc$name[k] == "Fcanopy") {
            value = 0.75
        }
        if(param.desc$name[k] == "TSUMEM") {
            value = 0
        }
      }
      if(crops$name[i] == "soybean"){
        if(param.desc$name[k] == "MaxArchResist") {
          value = 2.5
        }
      }
      if(crops$name[i] == "maize"){
        if(param.desc$name[k] == "RDMCR") {
          value = 200
        }
        if(param.desc$name[k] == "MinStomResist") {
          value = 120
        }
        if(param.desc$name[k] == "MaxArchResist") {
          value = 50
        }
      }
      if(crops$name[i] == "wheat"){
        if(param.desc$name[k] == "MinStomResist") {
          value = 100
        }
      }
      
      if (is.na(value)) {
        next
      }
      if (length(grep(x = param.desc$name[k], pattern = "VERN")) > 0) {
        next
      }
      
      space.after = rep(x = " ", times = max(1, n.space - nchar(value)))
      space.after = paste0(space.after, collapse = "")
      
      line = paste0(name,  space.before, "= ", value, space.after, description)
      text = c(text, line)
    } else {
      if (sum(is.na(values$Y)) > 0) {
        stop("X is present but not Y?")
      }
      
      if(crops$name[i] == "rice") {
        if(param.desc$name[k] == "AMAXTB") {
          values$Y = 70
        }
      }
      
      for(j in 1:nrow(values)){
        value = paste0(values$X[j], ", ", values$Y[j])
        
        valuex = param.used[paste0(param.desc$name[k], "_", j + 1), 1]
        if(!is.na(valuex)){
          value = paste0(value, ",")
        }
        
        if(j == 1){
          space.after = rep(x = " ", times = max(1, n.space - nchar(value)))
          space.after = paste0(space.after, collapse = "")
          
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
  
  out.file = paste0(out.dir, "/", "crop_params_", crops$name[i], ".txt")
  
  dir.create(dirname(out.file))
  writeLines(text = text, con = out.file)
}
