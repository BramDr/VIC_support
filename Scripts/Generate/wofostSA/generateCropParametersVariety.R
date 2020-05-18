rm(list = ls())

# Input
crop.file = "./Saves/crop_mapping_single.csv"
param.desc.file = "../../../Data/Primary/WOFOST/Crop/cropParameterDescription.csv"
limits.file = "../../../Data/Primary/WOFOST/Crop/cropTsumLimits.csv"
in.dir = "../../../Data/Transformed/Crops/"
out.dir = "../../../Data/WOFOST/Parameters/Crop/global/SA/"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
limits = read.csv(limits.file, stringsAsFactors = F)
param.desc = read.csv(param.desc.file, stringsAsFactors = F, sep = ";")

# Setup
get.name.alt = function(name) {
  if(name == "barley"){
    return("BAR1")
  } else if (name == "fababean"){
    return("FBE0801")
  } else if (name == "groundnut"){
    return("GR_NUT")
  } else if (name == "maize"){
    return("MAG202")
  } else if (name == "potato"){
    return("POT701")
  } else if (name == "rapeseed"){
    return("RAP1001")
  } else if (name == "rice"){
    return("RIC501")
  } else if (name == "soybean"){
    return("SOY0902")
  } else if (name == "sugarcane"){
    return("SUGRCANE")
  } else if (name == "sugarbeet"){
    return("SUG0601")
  } else if (name == "sunflower"){
    return("SUN1101")
  } else if (name == "sweetpotato"){
    return("SWPOTATO")
  } else if (name == "wheat"){
    return("WWHEAT1")
  } else {
    return(toupper(name))
  }
}

tsum.step = 100
tsum.day.step = 10

# Calculate & save
n.space = 16
in.files = list.files(path = in.dir, pattern = ".yaml", full.names = T)
in.files2 = list.files(path = in.dir, pattern = ".DATp", full.names = T)
for (i in 1:nrow(crops)) {
  print(crops$mirca.name[i])
  
  in.file = grep(x = in.files, pattern = paste0("//",crops$wofost.name[i]), value = TRUE)
  in.file2 = grep(x = in.files2, pattern = paste0("//",get.name.alt(crops$wofost.name[i])), value = TRUE)
  
  in.params = read.csv(in.file, stringsAsFactors = F, row.names = 1)
  in.params2 = read.csv(in.file2, stringsAsFactors = F, row.names = 1)
  
  limit = limits[limits$name == crops$mirca.name[i],]
  tsums = seq(from = limit$tsum_low, to = limit$tsum_high, by = tsum.step)
  if(crops$mirca.name[i] == "cassava") {
    tsums = seq(from = limit$tsum_low, to = limit$tsum_high, by = tsum.day.step)
  }
  
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
        value = 100
      } else if(param.desc$name[k] == "MaxArchResist") {
        value = 25
      } else if(param.desc$name[k] == "IDSL") {
        value = 0
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
  
  tsum1.wofost = in.params[rownames(in.params) == "TSUM1_1",1]
  tsum2.wofost = in.params[rownames(in.params) == "TSUM2_1",1]
  tsum1.frac = tsum1.wofost / (tsum1.wofost + tsum2.wofost)
  tsum2.frac = tsum2.wofost / (tsum1.wofost + tsum2.wofost)
  
  tsums1 = tsums * tsum1.frac
  tsums2 = tsums * tsum2.frac
  
  for (j in 1:length(tsums)) {
    out.file = paste0(out.dir, "/", "crop_params_", crops$mirca.name[i], "_", tsums[j], "_variety.txt")
    
    text.adj = text
    tsum1.line = grep(x = text.adj, pattern = "TSUM1")
    tsum2.line = grep(x = text.adj, pattern = "TSUM2")
    text.adj[tsum1.line] = paste0("TSUM1           = ", tsums1[j], "            ! Daily temperature sum from emergence to anthesis [C day-1]")
    text.adj[tsum2.line] = paste0("TSUM2           = ", tsums2[j], "            ! Daily temperature sum from emergence to anthesis [C day-1]")
    
    dir.create(dirname(out.file))
    writeLines(text = text.adj, con = out.file)
  }
}
