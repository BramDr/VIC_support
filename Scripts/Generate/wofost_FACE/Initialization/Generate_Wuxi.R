rm(list = ls())
library(openxlsx)
library(plyr)
library(Rcpp)

crop.param.file = "../Parameters_base/crop_params_rice.txt"
obs.a.file <- "../../../../Data/Primary/FACE/Hasegawa2017/Wuxi_FACE_R/Crop_soil_Wuxi_01_03_ambient_corrected_130502.xlsx"
obs.e.file <- "../../../../Data/Primary/FACE/Hasegawa2017/Wuxi_FACE_R/Crop_soil_Wuxi_01_03_elevated_corrected_130502.xlsx"
weather.file = "../../weather_FACE/Saves/weather_Wuxi_nursery.RDS"
initial.out = "./Saves/initial_Wuxi.RDS"

treatment = data.frame(year = rep(2001:2003, each = 6),
                       nutrient = rep(rep(c("LN", "MN", "HN"), each = 2), 3),
                       co2 = c("A", "E"))
treatment$treatment = paste0(substr(treatment$year, 3, 4), treatment$nutrient, treatment$co2)
treatment = treatment[!(treatment$year == 2001 & treatment$nutrient == "HN"),]

weather = readRDS(weather.file)

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

# Calculate
## Get parameters
crop.param <- readLines(crop.param.file)

Tmax <- grep(x = crop.param, patter = "^TEFFMX ", value = T)
Tmax <- gsub(x = Tmax, pattern = ".*=", replacement = "")
Tmax <- gsub(x = Tmax, pattern = "!.*", replacement = "")
Tmax <- as.numeric(trimws(Tmax))
Tbase <- grep(x = crop.param, patter = "^TBASEM ", value = T)
Tbase <- gsub(x = Tbase, pattern = ".*=", replacement = "")
Tbase <- gsub(x = Tbase, pattern = "!.*", replacement = "")
Tbase <- as.numeric(trimws(Tbase))
Tem <- grep(x = crop.param, patter = "^TSUMEM ", value = T)
Tem <- gsub(x = Tem, pattern = ".*=", replacement = "")
Tem <- gsub(x = Tem, pattern = "!.*", replacement = "")
Tem <- as.numeric(trimws(Tem))

Itable <- grep(x = crop.param, patter = "^DTSMTB ")
Ttable <- data.frame(temp = numeric(), tsum = numeric())
for (j in 1:25) {
  line <- crop.param[Itable + j - 1]
  if (j != 1 && length(grep(x = line, pattern = "=")) > 0) {
    break
  }
  
  line <- gsub(x = line, pattern = ".*=", replacement = "")
  line <- gsub(x = line, pattern = "!.*", replacement = "")
  fields <- strsplit(x = trimws(line), split = ",")
  
  x <- as.numeric(fields[[1]][1])
  y <- as.numeric(fields[[1]][2])
  Ttable[nrow(Ttable) + 1, ] <- c(x, y)
}

initial = data.frame(treatment = character(),
                     tsum = numeric(),
                     dry.matter = numeric(),
                     stringsAsFactors = F)

# Load
i = 1
for(i in 1:nrow(treatment)) {
  pattern = substr(treatment$treatment[i], 1, 4)
  
  if(treatment$co2[i] == "A"){
    obs.file = obs.a.file
  } else if(treatment$co2[i] == "E"){
    obs.file = obs.e.file
  }
  obs = read.xlsx(obs.file, sheet = pattern)
  
  dry.matter.init = 100
  dry.matter.init = obs$`totaldwt.(g/m2)`[2]
  
  tsum.init <- afgen(as.matrix(Ttable), 
                     as.numeric(weather$mean.air.temp[weather$treatment == treatment$treatment[i]]))
  tsum.init = sum(tsum.init)
  
  initial[nrow(initial) + 1,] = c(treatment$treatment[i], tsum.init, dry.matter.init)
}
initial$tsum = as.numeric(initial$tsum)
initial$dry.matter = as.numeric(initial$dry.matter)
initial$tsum[is.na(initial$tsum == 0) | initial$tsum == 0] = mean(initial$tsum[initial$tsum != 0], na.rm = T)

# Save
dir.create(dirname(initial.out))
saveRDS(initial, initial.out)
