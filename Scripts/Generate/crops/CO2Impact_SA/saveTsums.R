library(fields)
library(ncdf4)
library(Rcpp)
rm(list = ls())

# Input
crop.file <- "./Saves/crop_mapping.csv"
plant.file <- "./Saves/plantDay_30min_global.RDS"
harvest.file <- "./Saves/harvestDay_30min_global.RDS"
tfactor.file <- "./Saves/tfactor_30min_global.RDS"
tas.dir <- "../../../../Data/Primary/ISIMIP2b"
crop.param.dir <- "../../../../Data/WOFOST/Parameters/Crop/global"
tsum1.out <- "./Saves/tsum1_30min_global.RDS"
tsum2.out <- "./Saves/tsum2_30min_global.RDS"

# Load
crops <- read.csv(crop.file, stringsAsFactors = F)
crop.param.files <- list.files(path = crop.param.dir, full.names = T)
plant <- readRDS(plant.file)
harvest <- readRDS(harvest.file)
Tfactor <- readRDS(tfactor.file)

tas.files = list.files(path = tas.dir, full.names = T)

# Setup
lons <- seq(-179.75, 179.75, by = 0.5)
lats <- seq(-89.75, 89.75, by = 0.5)
noptions <- dim(plant)[4]

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

calcPhaseWOFOST <- function(tas.dev, crop) {
  tsumtot <- tas.dev[length(tas.dev)]

  if (crop == "wheat") {
    tsum1 <- (tsumtot - 100) / 2 + 100
    tsum2 <- (tsumtot - 100) / 2
    if (tsum2 <= 0) {
      tsum2 <- 1
    }
    if (tsum2 > 1500) {
      tsum1 <- tsum1 + tsum2 - 1500
      tsum2 <- 1500
    }
    return(c(tsum1, tsum2))
  } else if (crop == "maize") {
    tsum1 <- (tsumtot - 100) / 2
    tsum2 <- (tsumtot - 100) / 2 + 100
    if (tsum1 <= 0) {
      tsum1 <- 1
    }
    if (tsum2 > 900) {
      tsum1 <- tsum1 + tsum2 - 900
      tsum2 <- 900
    }
    return(c(tsum1, tsum2))
  } else if (crop == "soybean") {
    tsum1 <- (tsumtot / 5) * 2
    tsum2 <- (tsumtot / 5) * 3
    return(c(tsum1, tsum2))
  } else if (crop == "rice") {
    tsum1 <- (tsumtot / 5) * 3
    tsum2 <- (tsumtot / 5) * 2
    if (tsum1 > 1000) {
      tsum2 <- tsum2 + tsum1 - 1000
      tsum1 <- 1000
    }
    return(c(tsum1, tsum2))
  }

  tsum1 <- tsumtot * Tsum1 / (Tsum1 + Tsum2)
  tsum2 <- tsumtot * Tsum2 / (Tsum1 + Tsum2)
  return(c(tsum1, tsum2))
}

# Calculate
tas.file = tas.files[1]
for(tas.file in tas.files){
  print(basename(tas.file))
  
  nc = nc_open(tas.file)
  tas = ncvar_get(nc, "tas")
  nc_close(nc)
  
  force.model = gsub(x = tas.file, pattern = ".*tas_myda_", replacement = "")
  force.model = gsub(x = force.model, pattern = "_.*", replacement = "")
  force.rcp = gsub(x = tas.file, pattern = paste0(".*tas_myda_", force.model, "_"), replacement = "")
  force.rcp = gsub(x = force.rcp, pattern = "_.*", replacement = "")
  force.period = gsub(x = tas.file, pattern = ".nc$", replacement = "")
  force.period = gsub(x = force.period, pattern = ".*_", replacement = "")
  
  tsum1.map <- array(NA, dim = dim(plant))
  tsum2.map <- array(NA, dim = dim(plant))
  i <- 1
  for (i in 1:nrow(crops)) {
    print(crops$name[i])
  
    if (is.na(crops$crop[i])) {
      next
    }
  
    ## Get crop specific emergence and development parameters
    crop.param.file <- grep(x = crop.param.files, pattern = paste0("crop_params_", crops$crop[i], ".txt"), value = T)
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
  
    if (crops$name[i] == "rice") {
      Tem <- 0
    }
  
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
  
    ## Loop over growing area
    plant.crop <- plant[, , i, ]
    harvest.crop <- harvest[, , i, ]
    has.crop <- apply(X = plant.crop, MARGIN = c(1,2), FUN = function(x){sum(!is.na(x)) > 0})
    has.crop[has.crop == 0] = NA
  
    x <- 378
    y <- 188
    for (x in 1:dim(has.crop)[1]) {
      for (y in 1:dim(has.crop)[2]) {
        if (is.na(has.crop[x, y])) {
          next
        }
  
        ## Get crop specific temperatures
        for (o in 1:noptions) {
  
          plant.sel <- ceiling(plant.crop[x, y, o])
          harvest.sel <- ceiling(harvest.crop[x, y, o])
          if (is.na(harvest.sel) || is.na(plant.sel)) {
            tsum1.map[x, y, i, o] <- 1
            tsum2.map[x, y, i, o] <- 1
            next
          }
  
          if (plant.sel < harvest.sel) {
            tas.sel <- tas[x, y, plant.sel:harvest.sel] - 273.15
          } else if (plant.sel > harvest.sel) {
            tas.sel <- c(tas[x, y, plant.sel:365], tas[x, y, 1:harvest.sel]) - 273.15
          }
  
          if (is.na(tas.sel[1])) {
            next
          }
          tas.sel <- tas.sel + Tfactor[x, y]
  
          ## Sowing
          tas.sow <- tas.sel[1]
          Isow <- 1
  
          ## Emergence
          tas.em <- tas.sel - Tbase
          tas.em[tas.em < 0] <- 0
          tas.em[tas.em > Tmax - Tbase] <- Tmax - Tbase
          tas.em <- tas.em[(Isow + 1):length(tas.em)] # Start 1 day after sowing (WOFOST standard)
          tas.em <- cumsum(tas.em)
          Iem <- min(which(tas.em >= Tem))
  
          if (is.infinite(Iem)) {
            tsum1.map[x, y, i, o] <- 1
            tsum2.map[x, y, i, o] <- 1
            next
          }
  
          ## Growing
          tas.dev <- afgen(as.matrix(Ttable), tas.sel)
          tas.dev <- tas.dev[(Isow + Iem - 1):length(tas.dev)] # Start directly after emergence (WOFOST standard)
          tas.dev <- cumsum(tas.dev)
          Idev <- length(tas.dev)
  
          if (length(tas.dev) <= 1) {
            tsum1.map[x, y, i, o] <- 1
            tsum2.map[x, y, i, o] <- 1
            next
          }
  
          Tsums <- calcPhaseWOFOST(tas.dev, crops$name[i])
  
          tsum1.map[x, y, i, o] <- Tsums[1]
          tsum2.map[x, y, i, o] <- Tsums[2]
  
          if (tsum1.map[x, y, i, o] < 0 || tsum1.map[x, y, i, o] < 0) {
            stop("Error in calculation, tsum < 0")
          }
          if (tsum1.map[x, y, i, o] <= 0 || tsum2.map[x, y, i, o] <= 0) {
            tsum1.map[x, y, i, o] <- 1
            tsum2.map[x, y, i, o] <- 1
            next
          }
        }
      }
    }
  
    image.plot(tsum1.map[, , i, 5], main = paste0(crops$name[i], " TSUM1"), zlim = c(0, 2500))
    image.plot(tsum2.map[, , i, 5], main = paste0(crops$name[i], " TSUM2"), zlim = c(0, 2500))
  }
  
  tsum1.out.tmp = gsub(x = tsum1.out, pattern = "_30min", replacement = paste0("_", force.model, "_", force.rcp, "_", force.period, "_30min"))
  tsum2.out.tmp = gsub(x = tsum2.out, pattern = "_30min", replacement = paste0("_", force.model, "_", force.rcp, "_", force.period, "_30min"))
  
  # Save
  dir.create(dirname(tsum1.out.tmp))
  saveRDS(tsum1.map, tsum1.out.tmp)
  dir.create(dirname(tsum2.out.tmp))
  saveRDS(tsum2.map, tsum2.out.tmp)
}
