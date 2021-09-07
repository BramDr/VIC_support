rm(list = ls())

library(raster)
library(rgdal)
library(gdalUtils)
library(Rcpp)

tile.file = "../tileset_indus.txt"
vegcover.dir = "/mnt/annuna/lustre/backup/WUR/ESG/dropp001/Backup/MODIS/MOLT/MOD44B.006"
vegcover.out = "./Saves/VegetationCover/vegetationcover_500m.RDS"
sourceCpp("./Rcpp_support.cpp")

tiles = read.table(tile.file, stringsAsFactors = F)
  
in.files = list.files(vegcover.dir, pattern = "*.hdf$", full.names = T, recursive = T)
in.dates = as.Date(basename(dirname(in.files)), format = "%Y.%m.%d")
in.years = as.numeric(format.Date(in.dates, "%Y"))
in.months = as.numeric(format.Date(in.dates, "%m"))
in.days = as.numeric(format.Date(in.dates, "%d"))
in.doy = as.numeric(format.Date(in.dates, "%j"))

tile = tiles[1,1]
for(tile in tiles[,1]){
  print(tile)
  
  tile.sel = grep(in.files, pattern = tile)
  if(length(tile.sel) == 0){
    print("TILE NOT FOUND")
    next
  }
  
  subdata <- get_subdatasets(in.files[tile.sel[1]])
  subdata.data = grep(x = subdata, pattern = "Percent_NonVegetated$", value = T)
  subdata.quality = grep(x = subdata, pattern = "Quality", value = T)
  data = as.matrix(readGDAL(subdata.data, silent = T))
  quality = as.matrix(readGDAL(subdata.quality, silent = T))
  
  data[data > 100] = 0
  data[quality > 0] = NA
  
  vegcover_total = array(NA, dim = c(dim(data), length(tile.sel)))
  vegcover_total[,,1] = data
  
  z = 2
  for(z in 2:length(tile.sel)){
    idx = tile.sel[z]
    print(basename(in.files[idx]))
    
    subdata <- get_subdatasets(in.files[idx])
    subdata.data = grep(x = subdata, pattern = "Percent_NonVegetated$", value = T)
    subdata.quality = grep(x = subdata, pattern = "Quality", value = T)
    data = as.matrix(readGDAL(subdata.data, silent = T))
    quality = as.matrix(readGDAL(subdata.quality, silent = T))
    
    data[data > 100] = 0
    data[quality > 0] = NA
    
    vegcover_total[,,z] = data
  }
  
  vegcover_mean = apply(X = vegcover_total, MARGIN = c(1,2), FUN = mean, na.rm = T)
  #plot(raster(vegcover_mean))
  
  vegcover.out.tmp = gsub(vegcover.out, pattern = "_500m", replacement = paste0("_mean_", tile, "_500m"))
  dir.create(dirname(vegcover.out.tmp))
  saveRDS(vegcover_mean, vegcover.out.tmp)
}
