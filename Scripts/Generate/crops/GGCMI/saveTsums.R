library(fields)
library(ncdf4)
library(Rcpp)
rm(list = ls())

# Input
crop.file = "./Saves/crop_mapping.csv"
cc.file = "./Saves/cc_30min_global.RDS"
plant.file = "./Saves/plantDay_30min_global.RDS"
harvest.file = "./Saves/harvestDay_30min_global.RDS"
tair.file = "../../../../Data/Transformed/WFDEI/Tair_daily_WFDEI_1991_2000.myda.nc"
crop.param.dir = "../../../../Data/WOFOST/Parameters/Crop/global"
tsum1.out = "./Saves/tsum1_30min_global.RDS"
tsum2.out = "./Saves/tsum2_30min_global.RDS"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
crop.param.files = list.files(path = crop.param.dir, full.names = T)
cc = readRDS(cc.file)
plant = readRDS(plant.file)
harvest = readRDS(harvest.file)

nc = nc_open(tair.file)
Tair = ncvar_get(nc, "Tair")
nc_close(nc)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

cppFunction(
  'NumericVector afgen(NumericMatrix table, NumericVector values){
    NumericVector output(values.size());
    
    Dimension dim = table.attr("dim");
    
    for(int i = 0; i < values.size(); i++){
      if(values[i] <= table(0,0)) {
        output[i] = table(0,1);
        continue;
      }
      
      if(values[i] >= table(dim[0] - 1, 0)) {
        output[i] = table(dim[0] - 1, 1);
        continue;
      }
      
      for(int j = 0; j < dim[0] - 1; j++){
        if(values[i] >= table(j, 0) && values[i] < table(j+1, 0)){
          output[i] = table(j, 1) +
              (values[i] - table(j, 0)) *
              (table(j+1, 1) - table(j, 1)) /
              (table(j+1, 0) - table(j, 0));
          break;
        }
      }
    }
    
    return(output);
  }'
)
calcPhaseDuration <- function(tot_duration, crop, tsum1, tsum2) {
  if (crop=="maize") {
    v_duration <- 83.58-(0.85*tot_duration)+(0.006*tot_duration^2)
    r_duration <- NA
    
    if(tot_duration > 180){
      v_duration.lim <- 18.68+(0.21*180)+(0.0027*180^2)
      v_duration <- tot_duration * v_duration.lim / 180
    } else if(tot_duration < 60){
      v_duration.lim <- 18.68+(0.21*60)+(0.0027*60^2)
      v_duration <- tot_duration * v_duration.lim / 60
    }
    
  } else if (crop == "sugarcane" || crop == "cassava" || crop == "potato") {
    
    v_duration <- tot_duration * (Tsum1 / (Tsum1 + Tsum2))
    r_duration <- tot_duration * (Tsum2 / (Tsum1 + Tsum2))
    
  } else {
    
    v_duration <- 18.68+(0.21*tot_duration)+(0.0027*tot_duration^2)
    r_duration <- -306.52+344.83*(1-(exp(-0.0432*tot_duration)))
    
    if(tot_duration > 180){
      v_duration.lim <- 18.68+(0.21*180)+(0.0027*180^2)
      v_duration <- tot_duration * v_duration.lim / 180
    } else if(tot_duration < 60){
      v_duration.lim <- 18.68+(0.21*60)+(0.0027*60^2)
      v_duration <- tot_duration * v_duration.lim / 60
    }
    
  }
  
  #return(c(v_duration, r_duration))
  return(c(v_duration, tot_duration - v_duration))
}

# Calculate
i = 4
tsum1.map = array(NA, dim = dim(plant))
tsum2.map = array(NA, dim = dim(plant))
tsum.map = array(NA, dim = dim(plant))
for(i in 1:nrow(crops)) {
  print(crops$name[i])
  
  if(crops$priority[i] > 2){
    next
  }
  
  ## Get crop specific emergence and development parameters
  crop.param.file = grep(x = crop.param.files, pattern = paste0("crop_params_", crops$crop[i], ".txt"), value = T)
  crop.param = readLines(crop.param.file)
  
  Tmax = grep(x = crop.param, patter = "^TEFFMX ", value = T)
  Tmax = gsub(x = Tmax, pattern = ".*=", replacement = "")
  Tmax = gsub(x = Tmax, pattern = "!.*", replacement = "")
  Tmax = as.numeric(trimws(Tmax))
  Tbase = grep(x = crop.param, patter = "^TBASEM ", value = T)
  Tbase = gsub(x = Tbase, pattern = ".*=", replacement = "")
  Tbase = gsub(x = Tbase, pattern = "!.*", replacement = "")
  Tbase = as.numeric(trimws(Tbase))
  Tem = grep(x = crop.param, patter = "^TSUMEM ", value = T)
  Tem = gsub(x = Tem, pattern = ".*=", replacement = "")
  Tem = gsub(x = Tem, pattern = "!.*", replacement = "")
  Tem = as.numeric(trimws(Tem))
  
  Itable = grep(x = crop.param, patter = "^DTSMTB ")
  Ttable = data.frame(temp = numeric(), tsum = numeric())
  for(j in 1:25){
    line = crop.param[Itable + j - 1]
    if(j != 1 && length(grep(x = line, pattern = "=")) > 0){
      break
    }
    
    line = gsub(x = line, pattern = ".*=", replacement = "")
    line = gsub(x = line, pattern = "!.*", replacement = "")
    fields = strsplit(x = trimws(line), split = ",")
    
    x = as.numeric(fields[[1]][1])
    y = as.numeric(fields[[1]][2])
    Ttable[nrow(Ttable) + 1,] = c(x,y)
  }
  Tsum1 = grep(x = crop.param, patter = "^TSUM1 ", value = T)
  Tsum1 = gsub(x = Tsum1, pattern = ".*=", replacement = "")
  Tsum1 = gsub(x = Tsum1, pattern = "!.*", replacement = "")
  Tsum1 = as.numeric(trimws(Tsum1))
  Tsum2 = grep(x = crop.param, patter = "^TSUM2 ", value = T)
  Tsum2 = gsub(x = Tsum2, pattern = ".*=", replacement = "")
  Tsum2 = gsub(x = Tsum2, pattern = "!.*", replacement = "")
  Tsum2 = as.numeric(trimws(Tsum2))
  
  ## Loop over growing area
  cc.crop = cc[,,i,]
  cc.crop = apply(X = cc.crop, MARGIN = c(1,2), FUN = sum)
  plant.crop = plant[,,i]
  harvest.crop = harvest[,,i]
  x = 378
  y = 188
  for(x in 1:dim(cc.crop)[1]){
    for(y in 1:dim(cc.crop)[2]){
      if(is.na(cc.crop[x,y]) || cc.crop[x,y] <= 0){
        next
      }
      
      ## Get crop specific temperatures
      plant.sel = ceiling(plant.crop[x,y])
      harvest.sel = ceiling(harvest.crop[x,y])
      if(is.na(harvest.sel) || harvest.sel == plant.sel){
        harvest.sel = plant.sel - 1
      }
      
      if(plant.sel < harvest.sel) {
        Tair.sel = Tair[x,y,plant.sel:harvest.sel] - 273.15
      } else if(plant.sel > harvest.sel){
        Tair.sel = c(Tair[x,y,plant.sel:365], Tair[x,y,1:harvest.sel]) - 273.15
      }
      
      if(is.na(Tair.sel[1])){
        next
      }
      
      ## Sowing
      Tair.sow = Tair.sel[1]
      Isow = 1
      
      ## Emergence
      Tair.em = Tair.sel - Tbase
      Tair.em[Tair.em < 0] = 0
      Tair.em[Tair.em > Tmax - Tbase] = Tmax - Tbase
      Tair.em = Tair.em[(Isow + 1):length(Tair.em)] # Start 1 day after sowing (WOFOST standard)
      Tair.em = cumsum(Tair.em)
      Iem = min(which(Tair.em >= Tem))
      
      if(is.infinite(Iem)){
        tsum1.map[x,y,i] = NA
        tsum2.map[x,y,i] = NA
        next
      }
      
      ## Growing
      Tair.dev = afgen(as.matrix(Ttable), Tair.sel)
      Tair.dev = Tair.dev[(Isow + Iem):length(Tair.dev)] # Start directly after emergence (WOFOST standard)
      Tair.dev = cumsum(Tair.dev)
      Idev = length(Tair.dev)
      
      if(length(Tair.dev) <= 1){
        tsum1.map[x,y,i] = NA
        tsum2.map[x,y,i] = NA
        next
      }
      
      Ldevs = calcPhaseDuration(length(Tair.dev), crops$name[i])
      Ldevs = round(Ldevs)
      if(Ldevs[1] <= 0 || Ldevs[2] <= 0){
        tsum1.map[x,y,i] = NA
        tsum2.map[x,y,i] = NA
        next
      }
      
      tsum1.map[x,y,i] = Tair.dev[Ldevs[1]]
      tsum2.map[x,y,i] = Tair.dev[Idev] - Tair.dev[Ldevs[1]]
      tsum.map[x,y,i] = Tair.dev[Idev]
    }
  }
  
  image.plot(tsum1.map[,,i], main = paste0(crops$name[i], " TSUM1"))
  image.plot(tsum2.map[,,i], main = paste0(crops$name[i], " TSUM2"))
  #image.plot(tsum.map[,,i], main = paste0(crops$name[i], " TSUMTOT"))
}

# Save
dir.create(dirname(tsum1.out))
saveRDS(tsum1.map, tsum1.out)
dir.create(dirname(tsum2.out))
saveRDS(tsum2.map, tsum2.out)
