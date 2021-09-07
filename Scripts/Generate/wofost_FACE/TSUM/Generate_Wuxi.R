library(ncdf4)
library(Rcpp)
rm(list = ls())

crop.param.file = "../Parameters_base/crop_params_rice.txt"
phenology.file <- "../Phenology/Saves/phenology_Wuxi.RDS"
weather.dir <- "../../../../Data/VIC/Forcing/FACE/Wuxi/"
tsum.out <- "./Saves/tsum_Wuxi.RDS"

# Load
phenology = readRDS(phenology.file)
weather.files <- list.files(path = weather.dir, pattern = "forcing.*nc", full.names = T)

# Setup
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

## Get temperature
temp = data.frame(time = seq.Date(from = as.Date("2001-01-01"), 
                                  to = as.Date("2003-12-31"), 
                                  by = "day"),
                  temp = NA)

weather.file = weather.files[1]
for(weather.file in weather.files){
  print(basename(weather.file))
  
  nc = nc_open(weather.file)
  time = as.Date(nc$dim$time$vals, "1970-01-01")
  tas = ncvar_get(nc, "tas")
  nc_close(nc)
  
  sel = match(temp$time, time)
  sel.tas = sel[!is.na(sel)]
  sel.temp = which(!is.na(sel))
  temp$temp[sel.temp] = tas[sel.tas]
}

## Get tsum
tsums = data.frame(treatment = phenology$treatment, tem = NA, tsum1 = NA, tsum2 = NA)
treatment = phenology$treatment[16]
for(treatment in phenology$treatment){
  time.min = temp$time >= as.Date(phenology$transplant[phenology$treatment == treatment]) - 1
  time.max = temp$time <= as.Date(phenology$maturity[phenology$treatment == treatment])
  time.sel = which(time.min & time.max)
  Tair.sel = temp$temp[time.sel]
  time.sel = temp$time[time.sel]
  
  ## Sowing
  Tair.sow <- Tair.sel[1]
  Isow <- 1
  ## Emergence
  Iem = 0
  tem = 0
  ## TSUM 1
  Itsum1 = which(time.sel == as.Date(phenology$anthesis[phenology$treatment == treatment]))
  Tair.tsum1 <- Tair.sel[(Iem + 1):Itsum1] # Start directly after emergence (rice starts at emergence)
  Tair.tsum1 <- afgen(as.matrix(Ttable), Tair.tsum1)
  Tair.tsum1 <- cumsum(Tair.tsum1)
  tsum1 = Tair.tsum1[length(Tair.tsum1)]
  ## TSUM 2
  Itsum2 = which(time.sel == as.Date(phenology$maturity[phenology$treatment == treatment]))
  Tair.tsum2 <- Tair.sel[Itsum1:Itsum2] # Start directly after anthesis (WOFOST standard)
  Tair.tsum2 <- afgen(as.matrix(Ttable), Tair.tsum2)
  Tair.tsum2 <- cumsum(Tair.tsum2)
  tsum2 = Tair.tsum2[length(Tair.tsum2)]
  
  tsums$tem[tsums$treatment == treatment] = tem
  tsums$tsum1[tsums$treatment == treatment] = tsum1
  tsums$tsum2[tsums$treatment == treatment] = tsum2
}
print(tsums)

# Save
dir.create(dirname(tsum.out))
saveRDS(tsums, tsum.out)
