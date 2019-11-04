rm(list = ls())
options(warn = 1)

param.desc.file = "../../../Data/Primary/WOFOST/Crop/cropParameterDescription.csv"
in.dirs = c("../../../Data/Primary/WOFOST/Crop/Python_version/",
            "../../../Data/Primary/WOFOST/Crop/NPK_version/",
            "../../../Data/Primary/WOFOST/Crop/EU_project/",
            "../../../Data/Primary/WOFOST/Crop/CSA_practical/")
out.dir = "../../../Data/Transformed/WOFOST/Crop/"

in.files = c()
for (i in 1:length(in.dirs)) {
  in.files = c(in.files, list.files(path = in.dirs[i], full.names = T))
}

for (in.file in in.files) {
  print(basename(in.file))
  
  # Load
  param.desc = read.csv(param.desc.file, stringsAsFactors = F)
  param.orig = readLines(con = in.file)
  
  # Setup
  in.extension = strsplit(basename(in.file), "\\.")
  in.extension = unlist(in.extension)
  in.extension = in.extension[length(in.extension)]
  
  find.data.yaml = function(file.text, variable, size) {
    value = NA
    
    for (i in 1:length(file.text)) {
      param.line = file.text[i]
      param.line = gsub(pattern = " ", replacement = "", x = param.line)
      param.line = gsub(pattern = ":", replacement = "", x = param.line)
      
      if (param.line == variable) {
        if (size == 1) {
          j = i + 1
          value.line = file.text[j]
          
          if(j == i + 1 && length(grep(pattern = "\\[", x = value.line)) != 0){
            stop(paste0("ERROR: Found [ in first line. ", 
                         variable, " (", size, "): ", value.line))
          }
          
          value.line = gsub(pattern = " ", replacement = "", x = value.line)
          value.line = gsub(pattern = "-", replacement = "", x = value.line)
          
          # print(value.line)
          if(variable == "CRPNAM"){
            value.line = ""
          }
          value = as.numeric(value.line)
        } else {
          value = c()
          last.line = F
          
          for (j in (i + 1):(i + size)) {
            value.line = file.text[j]
            
            if (last.line) {
              # Reached last line
              value = c(value, NA, NA)
              next
            }
            
            if(j == (i + size) && length(grep(pattern = "\\]", x = value.line)) == 0){
              stop(paste0("ERROR: Could not find ] in last line. ", 
                           variable, " (", size, "): ", value.line))
            } else if (length(grep(pattern = "\\]", x = value.line)) != 0) {
              # Reached last line
              last.line = T
            }
            
            value.line = gsub(pattern = "\\[", replacement = "", x = value.line)
            value.line = gsub(pattern = "\\]", replacement = "", x = value.line)
            value.line = gsub(pattern = " ", replacement = "", x = value.line)
            value.line = gsub(pattern = "-", replacement = "", x = value.line)
            value.line = strsplit(x = value.line, split = ",")
            
            # print(value.line)
            value = c(value, as.numeric(as.character(unlist(value.line))))
          }
        }
        break
      }
    }
    
    return(value)
  }
  find.data.txt = function(file.text, variable, size) {
    value = NA
    
    for (i in 1:length(file.text)) {
      param.line = file.text[i]
      param.line = gsub(pattern = " ", replacement = "", x = param.line)
      param.line = gsub(pattern = "=.*", replacement = "", x = param.line)
      
      if (param.line == variable) {
        if (size == 1) {
          j = i
          value.line = file.text[j]
          
          if(j == i + 1 && length(grep(pattern = "\\,", x = value.line)) != 0){
            stop(paste0("ERROR: Found , in first line. ", 
                         variable, " (", size, "): ", value.line))
          }
          
          value.line = gsub(pattern = "!.*", replacement = "", x = value.line)
          value.line = gsub(pattern = ".*=", replacement = "", x = value.line)
          
          # print(value.line)
          if(variable == "CRPNAM"){
            value.line = ""
          }
          value = as.numeric(value.line)
        } else {
          value = c()
          last.line = F
          
          for (j in i:(i + size - 1)) {
            value.line = file.text[j]
            
            if (last.line) {
              # Reached last line
              value = c(value, NA, NA)
              next
            }
            
            if(j == (i + size) && length(grep(pattern = "\\,", x = value.line)) == 0){
              stop(paste0("ERROR: Could not find , in last line. ", 
                           variable, " (", size, "): ", value.line))
            } 
            
            value.line = gsub(pattern = "!.*", replacement = "", x = value.line)
            value.line = gsub(pattern = ".*=", replacement = "", x = value.line)
            value.line = strsplit(x = value.line, split = ",")
            
            if(j == (i + size) && length(value.line) != 2){
              stop(paste0("ERROR: Could not find second , in last line. ", 
                           variable, " (", size, "): ", value.line))
            } else if (length(value.line) <= 2) {
              # Reached last line
              last.line = T
            }
            
            # print(value.line)
            value = c(value, as.numeric(as.character(unlist(value.line)[1:2])))
          }
        }
        break
      }
    }
    
    return(value)
  }
  
  # Calculate
  col.names = c("X", "Y")
  row.names = c()
  for (i in 1:nrow(param.desc)) {
    for (j in 1:param.desc$size[i]) {
      row.names = c(row.names, paste0(param.desc$name[i], "_", j))
    }
  }
  param.data = data.frame(matrix(ncol = length(col.names), nrow = length(row.names)))
  rownames(param.data) = row.names
  colnames(param.data) = col.names
  
  options(warn = 2)
  for (i in 1:nrow(param.desc)) {
    #print(param.desc$name[i])
    if (in.extension == "yaml") {
      values = find.data.yaml(file.text = param.orig, variable = param.desc$name[i], size = param.desc$size[i])
    } else {
      values = find.data.txt(file.text = param.orig, variable = param.desc$name[i], size = param.desc$size[i])
    }
    
    row = paste0(param.desc$name[i], "_", 1:param.desc$size[i])
    if(param.desc$size[i] == 1){
      param.data[row, "X"] = values
    } else {
      param.data[row, c("X", "Y")] = matrix(values, ncol = 2, byrow = T)
    }
  }
  options(warn = 1)
  
  out.file = paste0(out.dir, "/", basename(in.file))
  dir.create(dirname(out.file), showWarnings = F)
  write.csv(x = param.data, file = out.file)
}
