rm(list = ls())

library(raster)
library(rgdal)
library(gdalUtils)
library(Rcpp)

tile.file = "../tileset_indus.txt"
landuse.dir = "/mnt/annuna/lustre/backup/WUR/ESG/dropp001/MODIS/MOTA/MCD12Q1.006"
landuse.out = "./Saves/LandCover/landcover_500m_Africa.RDS"
sourceCpp("./Rcpp_support.cpp")

tiles = read.table(tile.file, stringsAsFactors = F)
  
in.files = list.files(landuse.dir, pattern = "*.hdf$", full.names = T, recursive = T)
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
  subdata.data = grep(x = subdata, pattern = "_Type2", value = T)
  data = as.matrix(readGDAL(subdata.data, silent = T))
  
  landuse_total = array(NA, dim = c(dim(data), length(tile.sel)))
  landuse_total[,,1] = data
  
  z = 2
  for(z in 2:length(tile.sel)){
    idx = tile.sel[z]
    print(basename(in.files[idx]))
    
    subdata <- get_subdatasets(in.files[idx])
    subdata.data = grep(x = subdata, pattern = "_Type2", value = T)
    data = as.matrix(readGDAL(subdata.data, silent = T))
    
    landuse_total[,,z] = data
  }
  
  landuse.return = get_landcover_majority(landuse_total)
  landuse.major = landuse.return$major
  landuse.fraction = landuse.return$fraction
  # plot(raster(landuse.major))
  # plot(raster(landuse.fraction))
  
  landuse.out.tmp = gsub(landuse.out, pattern = "_500m", replacement = paste0("_major_", tile, "_500m"))
  dir.create(dirname(landuse.out.tmp))
  saveRDS(landuse.major, landuse.out.tmp)
  landuse.out.tmp = gsub(landuse.out, pattern = "_500m", replacement = paste0("_fraction_", tile, "_500m"))
  dir.create(dirname(landuse.out.tmp))
  saveRDS(landuse.fraction, landuse.out.tmp)
}
