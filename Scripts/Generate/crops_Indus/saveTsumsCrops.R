library(fields)
library(ncdf4)
library(Rcpp)
rm(list = ls())

# Input
crop.file <- "./Saves/crop_mapping.RDS"
plant.file <- "./Saves/plantday_crops_5min_Indus.RDS"
harvest.file <- "./Saves/harvestday_crops_5min_Indus.RDS"
tair.file <- "../../../Data/Transformed/ERA5/tas_daily_ERA5_ydaymean_1990_2010.nc"
crop.param.dir <- "../../../Data/WOFOST/Parameters/Crop/Indus_5min"
tsum1.out <- "./Saves/tsum1_crops_5min_Indus.RDS"
tsum2.out <- "./Saves/tsum2_crops_5min_Indus.RDS"

# Load
crops <- readRDS(crop.file)
plant <- readRDS(plant.file)
harvest <- readRDS(harvest.file)
crop.param.files <- list.files(path = crop.param.dir, full.names = T)

nc <- nc_open(tair.file)
Tair <- ncvar_get(nc, "tas")
nc_close(nc)

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

calcPhaseWOFOST <- function(Tair.dev, crop, Idvs = 0) {
  tsumtot <- Tair.dev[length(Tair.dev)]

  if (crop == "wheat") {
    tsum1 <- (tsumtot - 100) / 2 + 100
    tsum2 <- (tsumtot - 100) / 2
    
    if(tsumtot < 100){
      tsum1 = tsumtot / 2
      tsum2 = tsumtot / 2
    }
    
    tsum1 <- tsum1 / (1 - Idvs / 2)
    tsum2 <- tsum2 / (1 - Idvs / 2)
    
    if (tsum2 <= 0) {
      tsum2 <- 1
    }
    if (tsum2 > 1500) {
      tsum1 <- tsum1 + tsum2 - 1500
      tsum2 <- 1500
    }
    
    return(c(tsum1, tsum2))
  } 
  else if (crop == "rice") {
    tsum1 <- (tsumtot / 5) * 3
    tsum2 <- (tsumtot / 5) * 2
    
    tsum1 <- tsum1 / (1 - Idvs / 2)
    tsum2 <- tsum2 / (1 - Idvs / 2)
    
    if (tsum2 > 650) {
      tsum1 <- tsum1 + tsum2 - 650
      tsum2 <- 650
    }
    
    return(c(tsum1, tsum2))
  }

  tsum1 <- tsumtot * Tsum1 / (Tsum1 + Tsum2)
  tsum2 <- tsumtot * Tsum2 / (Tsum1 + Tsum2)
  return(c(tsum1, tsum2))
}

# Calculate
tsum1.map <- array(NA, dim = dim(plant))
tsum2.map <- array(NA, dim = dim(plant))
i <- 5
for (i in 1:nrow(crops)) {
  print(crops$name[i])

  if (is.na(crops$crop[i])) {
    next
  }

  ## Get crop specific emergence and development parameters
  crop.param.file <- grep(x = crop.param.files, pattern = paste0("crop_params_", crops$crop[i], ".txt"), value = T)
  crop.param <- readLines(crop.param.file)

  Tmax <- grep(x = crop.param, patter = "^TEFFMX ", value = T)
  Tmax <- gsub(x = Tmax, pattern = "!.*", replacement = "")
  Tmax <- gsub(x = Tmax, pattern = ".*=", replacement = "")
  Tmax <- as.numeric(trimws(Tmax))
  Tbase <- grep(x = crop.param, patter = "^TBASEM ", value = T)
  Tbase <- gsub(x = Tbase, pattern = "!.*", replacement = "")
  Tbase <- gsub(x = Tbase, pattern = ".*=", replacement = "")
  Tbase <- as.numeric(trimws(Tbase))
  Tem <- grep(x = crop.param, patter = "^TSUMEM ", value = T)
  Tem <- gsub(x = Tem, pattern = "!.*", replacement = "")
  Tem <- gsub(x = Tem, pattern = ".*=", replacement = "")
  Tem <- as.numeric(trimws(Tem))
  Idvs <- grep(x = crop.param, patter = "^DVSI ", value = T)
  Idvs <- gsub(x = Idvs, pattern = "!.*", replacement = "")
  Idvs <- gsub(x = Idvs, pattern = ".*=", replacement = "")
  Idvs <- as.numeric(trimws(Idvs))

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
  Tsum1 <- grep(x = crop.param, patter = "^TSUM1 ", value = T)
  Tsum1 <- gsub(x = Tsum1, pattern = ".*=", replacement = "")
  Tsum1 <- gsub(x = Tsum1, pattern = "!.*", replacement = "")
  Tsum1 <- as.numeric(trimws(Tsum1))
  Tsum2 <- grep(x = crop.param, patter = "^TSUM2 ", value = T)
  Tsum2 <- gsub(x = Tsum2, pattern = ".*=", replacement = "")
  Tsum2 <- gsub(x = Tsum2, pattern = "!.*", replacement = "")
  Tsum2 <- as.numeric(trimws(Tsum2))

  ## Loop over growing area
  plant.crop <- plant[, , i]
  harvest.crop <- harvest[, , i]

  x <- 100
  y <- 100
  for (x in 1:dim(plant.crop)[1]) {
    for (y in 1:dim(plant.crop)[2]) {
      if (is.na(plant.crop[x, y])) {
        next
      }

      ## Get crop specific temperatures
      plant.sel <- ceiling(plant.crop[x, y])
      harvest.sel <- ceiling(harvest.crop[x, y])

      if (plant.sel < harvest.sel) {
        Tair.sel <- Tair[x, y, plant.sel:harvest.sel]
      } else if (plant.sel > harvest.sel) {
        Tair.sel <- c(Tair[x, y, plant.sel:365], Tair[x, y, 1:harvest.sel])
      }

      ## Sowing
      Tair.sow <- Tair.sel[1]
      Isow <- 1

      ## Emergence
      Tair.em <- Tair.sel[(Isow + 1):length(Tair.sel)] # Start 1 day after sowing (WOFOST standard)
      Tair.em[Tair.em > Tmax] = Tmax
      Tair.em = Tair.em - Tbase
      Tair.em[Tair.em < 0] <- 0
      Tair.em <- cumsum(Tair.em)
      Iem <- min(which(Tair.em >= Tem)) + Isow + 1

      if (is.infinite(Iem)) {
        tsum1.map[x, y, i] <- 1
        tsum2.map[x, y, i] <- 1
        next
      }

      ## Growing
      Tair.dev <- Tair.sel[(Iem + 1):length(Tair.sel)] # Start 1 day after emergence (WOFOST standard)
      Tair.dev <- afgen(as.matrix(Ttable), Tair.dev)
      Tair.dev <- cumsum(Tair.dev)
      Idev <- length(Tair.dev) + Iem + 1

      if (length(Tair.dev) <= 1) {
        tsum1.map[x, y, i] <- 1
        tsum2.map[x, y, i] <- 1
        next
      }

      Tsums <- calcPhaseWOFOST(Tair.dev, crops$name[i], Idvs)
      tsum1.map[x, y, i] <- Tsums[1]
      tsum2.map[x, y, i] <- Tsums[2]

      if (tsum1.map[x, y, i] < 0 || tsum1.map[x, y, i] < 0) {
        stop("Error in calculation, tsum < 0")
      }
      if (tsum1.map[x, y, i] <= 0 || tsum2.map[x, y, i] <= 0) {
        tsum1.map[x, y, i] <- 1
        tsum2.map[x, y, i] <- 1
        next
      }
    }
  }
  image.plot(tsum1.map[, , i], main = paste0(crops$name[i], " TSUM1"))
  image.plot(tsum2.map[, , i], main = paste0(crops$name[i], " TSUM2"))
}

# Save
dir.create(dirname(tsum1.out))
saveRDS(tsum1.map, tsum1.out)
dir.create(dirname(tsum2.out))
saveRDS(tsum2.map, tsum2.out)
